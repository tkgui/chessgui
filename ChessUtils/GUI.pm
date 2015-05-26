###################################################################

package Option;

###################################################################

use Tk;

sub new
{
	my $self=shift;
	
	my $label_text=shift;
	
	my $textvariable=shift;
	
	my $default=shift;
	
	my $option_values=shift;
	
	my $callback=shift;
	
	$self={
		label_text=>$label_text,
		textvariable=>$textvariable,
		default=>$default,
		option_values=>$option_values,
		callback=>$callback
	};
	
	bless $self;
	
	return $self;
}

sub frame
{
	my $self=shift;
	
	my $mw=shift;
	
	$self->{frame}=$mw->Frame();
	
	if($self->{option_values} eq 'check')
	{
	
		$self->{checkbox}=$self->{frame}->Checkbutton(
			-text => $self->{label_text},
			-onvalue => '1',
			-offvalue => '0',
			-command=>$self->{callback},
			-variable => $self->{textvariable}
			)->pack(%button_pack_options,-side=>'left');
	
	}
	else
	{
	
		$self->{label}=$self->{frame}->Label(-text=>$self->{label_text})->pack(-side=>'left');
		
		$self->{optionmenu}=$self->{frame}->Optionmenu(
				-options => $self->{option_values},
				-command => $self->{callback},
				-textvariable => $self->{textvariable}
			)->pack(-side=>'left');
		
	}
		
	return $self->{frame};
}

###################################################################

package Game;

###################################################################

use ChessUtils::Utils;
use ChessUtils::Board;

use Tk;
require Tk::Dialog;

use Time::HiRes qw(gettimeofday tv_interval);
use Cwd;

use Data::Dumper;

use Storable qw(freeze thaw);

use JSON;

use Win32::Clipboard;my $CLIP=Win32::Clipboard();

############################################

# global vars

############################################
# config

$WINBY=1;

############################################

my $dump_txt='../dump.txt';

my $piece_size=40;
my $margin=10;

my $square_size=55;
my $canvas_size=$square_size*8+$margin*2;

my %draginfo;

my %button_pack_options=qw(-padx 5);
my %green_button_attr=qw(-background #afffaf -activebackground #7fff7f);
my %blue_button_attr=qw(-background #afffff -activebackground #7fffff);
my %yellow_button_attr=qw(-background #ffffaf -activebackground #ffff7f);

my %pgn_text_attr=(-foreground=>'#007f00',-background=>'#efefef',-font=>[-family=>'Courier',-size=>14,-weight=>'bold']);
my %pgn_header_text_attr=(-height=>1,-width=>14,-foreground=>'#007f00',-background=>'#efefef',-font=>[-family=>'Courier',-size=>10,-weight=>'bold']);

my @comments=qw(!! ! !? - ?! ? ??);
my $comment_line=join(' ',map { sprintf "%-2s",$_ } @comments);

my $comments={
	'!!'=>{
		name=>'exex',
		value=>5,
		tags=>{-foreground=>'#00ff00'}
	},
	'!'=>{
		name=>'ex',
		value=>3,
		tags=>{-foreground=>'#0000ff'}
	},
	'!?'=>{
		name=>'exqu',
		value=>1,
		tags=>{-foreground=>'#007f7f'}
	},
	'-'=>{
		name=>'neut',
		value=>0,
		tags=>{-foreground=>'#000000'}
	},
	'?!'=>{
		name=>'quex',
		value=>-1,
		tags=>{-foreground=>'#7f7f00'}
	},
	'?'=>{
		name=>'qu',
		value=>-3,
		tags=>{-foreground=>'#7f007f'}
	},
	'??'=>{
		name=>'ququ',
		value=>-5,
		tags=>{-foreground=>'#ff0000'}
	},
	'...'=>{
		name=>'un',
		value=>-100
	}
};

############################################

# globals subs

############################################

sub fen_to_hex
{
	my $fen=shift;
	
	my $bin=unpack 'B*',$fen;

	my $bm5=length($bin) % 5;

	$bin=($bm5?('0'x(5-$bm5)):'').$bin;
	
	my $base32;
	
	while($bin=~s/(.....)//)
	{	
		my $dec=oct('0b'.$1);
		
		$base32.=$dec<26?chr(ord('a')+$dec):chr(ord('0')+$dec-26);
	}
	
	return $base32;
}

sub hex_to_fen
{
	my $hex=shift;
	
	my $bin;
	
	foreach(split //,$hex)
	{
		my $ord=ord($_);
		
		$ord-=(($ord>=ord('a'))?(ord('a')):(ord('0')-26));
		
		$bin.=sprintf "%05b",$ord;

	}
	
	my $bm8=length($bin) % 8;
	
	$bin=$bm8?substr($bin,$bm8):$bin;
	
	my $fen=pack 'B*',$bin;
	
	return $fen;
}


############################################

# engine

############################################

use IPC::Open2;
use threads;
use threads::shared;

use Thread::Queue;

############################################

my $q=new Thread::Queue;
my $r=new Thread::Queue;

my $aq=new Thread::Queue;
my $ar=new Thread::Queue;

my $wq=new Thread::Queue;
my $wr=new Thread::Queue;

my $engine_status=new Thread::Queue;
my $engine_action=new Thread::Queue;

############################################

my $new_command="new\nvariant atomic\n";

############################################

if($WINBY)
{

	my $wpid = open2(\*WCHLD_OUT, \*WCHLD_IN, 'uciengine.exe');

	my $wissue_command_thread=threads->create(sub {

				while(1)
				{
				
					my $command=$wq->dequeue;
					
					print WCHLD_IN $command;
					flush WCHLD_IN;
					
				}
				
			})->detach;
			
			
	my $wread_output_thread=threads->create(sub {

				do
				
					{
					
						my $chunk=<WCHLD_OUT>;
						
						#print $chunk;
						
						$wr->enqueue($chunk);
					
					}
					
				while(1);

		})->detach;
		
}

if(!$WINBY)
{

	my $pid = open2(\*CHLD_OUT, \*CHLD_IN, '../pulsar.exe');

	my $apid = open2(\*ACHLD_OUT, \*ACHLD_IN, '../atomkraft.exe');

	my $issue_command_thread=threads->create(sub {

			while(1)
			{
			
				my $command=$q->dequeue;
				
				print CHLD_IN $command;
				flush CHLD_IN;
				
			}
			
		})->detach;
		
	my $aissue_command_thread=threads->create(sub {

			while(1)
			{
			
				my $command=$aq->dequeue;
				
				print ACHLD_IN $command;
				flush ACHLD_IN;
				
			}
			
		})->detach;
		
	my $read_output_thread=threads->create(sub {

			do
			
				{
				
					my $chunk=<CHLD_OUT>;
					
					my @nocontrols = grep { ( ord ( $_ ) >= 32 ) || ( $_ eq "\n" ) } split //,$chunk;
					
					$chunk=join('',@nocontrols);
					
					#print "$chunk";
					
					if($chunk=~/received move/)
					{
						$engine_status->enqueue(1);
					}
					
					if($chunk=~/finished move/)
					{
						$engine_status->enqueue(0);
					}
					
					if($chunk=~/(^[0-9\-]+)\s+([0-9\-]+)\s+[0-9\-]+\s+[0-9\-]+\s+([a-z].*)/)
						{
							my $depth=$1;
							my $eval=$2;if($eval>0){$eval='+'.$eval;}
							my $line=$3;
							my $engine_line=sprintf("%2d %5s   $line",$depth,$eval);
							
							$r->enqueue($engine_line);
						}
				
				}
				
			while(1);

		})->detach;
		
	my $aread_output_thread=threads->create(sub {

			my $prev_depth;
			do
			
				{
				
					my $chunk=<ACHLD_OUT>;
					
					if($prev_depth eq ''){$prev_depth=$depth};
					
					if($chunk=~s/info depth ([0-9]+)//)
					{
					my $depth=$1;
					$chunk=~s/[\n\r]//g;
					$chunk=~s/score cp ([0-9\-]+)//;
					my $score=$1;
					$chunk=~s/pv //;
					$chunk=sprintf "%2d %5d %s\n",$depth,$score,(substr $chunk,0,40);
					
					if($depth!=$prev_depth)
					{
						print "----------------------------------\n";
						$prev_depth=$depth;
					}
					
					print $chunk;
					}
					
				}
				
			while(1);
				
			
		})->detach;
	
}
	
############################################

$q->enqueue("xboard\nvariant atomic\nanalyze\npost\n");
	
############################################

sub del_engine_arrow
{
	my $self=shift;
	
	if($self->{engine_arrow} ne '')
	{
		$self->{canvas}->delete($self->{engine_arrow});
	}
		
	if($self->{canvas_score_text} ne '')
	{
		$self->{canvas}->delete($self->{canvas_score_text});
	}
}

my $total_built;

sub get_legal_algeb_moves
{
	my $self=shift;
	
	my $current_pos=shift;
	
	my @legal_algeb_moves;
	
	if(!exists($self->{node_book}->{$current_pos}->{legal_algeb_moves}))
	{
	
		$total_built++;
		
		$self->{dont_check_pgn}=1;
		my $legal_algeb_moves=$self->{board}->legal_algeb_moves;
		
		@legal_algeb_moves=sort keys(%{$legal_algeb_moves});
		
		@{$self->{node_book}->{$current_pos}->{legal_algeb_moves}}=@legal_algeb_moves;
		
		####################################
		
		$self->save_fen($current_pos);
		
		####################################
		
	}
	else
	{
	
		@legal_algeb_moves=@{$self->{node_book}->{$current_pos}->{legal_algeb_moves}};
		
	}
	
	return @legal_algeb_moves;
}

sub select_node
{
	my $self=shift;
	
	my $depth=shift;
	
	if($depth>$self->{settings}->{max_depth})
	{	
		return 0;
	}
	
	my $full_algeb;

	my $current_pos=$self->{board}->report_fen(1);
	
	my @legal_algeb_moves=$self->get_legal_algeb_moves($current_pos);
	
	if(@legal_algeb_moves==0)
	{
		return 0;
	}

	foreach(@legal_algeb_moves)
	{
	
		$full_algeb=$_;
		
		if($self->{after_move_book}->{$current_pos}->{$full_algeb}->{orig_eval} eq '')
		{
		
			$self->{analyzed_move}=$full_algeb;
		
			$self->make_move($full_algeb);
			
			$self->move_made;
			
			$self->{analyzing}=1;
			
			$self->{analysis_pos}=$current_pos;
			
			$self->analyze_pos;
			
			return 1;
			
		}
		
	}
	
	@legal_algeb_moves=sort
	{
	
		$self->{board}->{turn}==1
		?
		$self->{after_move_book}->{$current_pos}->{$a}->{eval}<=>$self->{after_move_book}->{$current_pos}->{$b}->{eval}
		:
		$self->{after_move_book}->{$current_pos}->{$b}->{eval}<=>$self->{after_move_book}->{$current_pos}->{$a}->{eval}
		
		
	}@legal_algeb_moves;
	
	while(@legal_algeb_moves)
	{
		my $selected_full_algeb=pop(@legal_algeb_moves);
		
		my $eval=$self->{after_move_book}->{$current_pos}->{$selected_full_algeb}->{orig_eval};
		
		########################################
		# cutoff
		
		if(abs($eval)>$self->{settings}->{cutoff})
		{
			return 0;
		}
		
		########################################
		
		my $criterion=$self->{settings}->{depth_percent}+$depth*10;
		
		if($criterion>100){$criterion=100;}
		
		my $rand=int(rand(100));
		
		#$self->log("Trying $selected_full_algeb at $depth with $criterion chance, rand : $rand\n");
		
		if($rand<$criterion)
		{
		
			#$self->log("--> $selected_full_algeb selected\n");
		
			$self->make_move($selected_full_algeb);
	
			return $self->select_node($depth+1);
		}
	}
	
	return 0;
	
}

sub open_log_window
{

	my $self=shift;
	
	($self->{log_window}=$self->{mw}->Toplevel(-title=>'Log'))->geometry('780x160+520+515');
	
	$self->{log_window}->OnDestroy(sub { $self->{log_window_open}=0; });
	
	$self->{log_text}=$self->{log_window}->Scrolled('Text',-width=>110,-height=>10,-scrollbars=>'se')->pack;
	
	$self->{log_window_open}=1;
	
}

sub close_log_window
{

	my $self=shift;
	
	$self->{log_window}->destroy;
	
}

sub log
{

	my $self=shift;
	
	my $what=shift;
	
	$elapsed = tv_interval ($self->{log_t0}, [gettimeofday]);
	
	my $elapsed_f=sprintf "%8s",(sprintf "%.2f",$elapsed);
	
	if(!$self->{log_window_open}){$self->open_log_window;}
	
	if($self->{log_window_open})
	{
	
		$self->{log_text}->insert('1.0',"$elapsed_f : $what");

		if($self->{analysis_mode})
		{
			$self->{log_window}->configure(-title=>"Analysis log - Positions $self->{examined_nodes}");
		}
	
	}
	
}

sub minimax_out
{
	my $self=shift;
	
	my $forced=shift;
	
	return if ( ( $self->{analysis_mode} && (!$forced) ) || ( $self->{minimax_in_progress} ) );
	
	$self->log("Minimax to depth $self->{settings}->{dump_depth} in progress\n");
	
	$self->{minimax_in_progress}=1;
	
	$self->{minimax_node_count}=0;
	
	$self->{engine_minimax_button}->configure(-text=>'Minimax in progess!');$self->{engine_minimax_button}->update;
	$self->{engine_analysis_button}->configure(-state=>'disabled');$self->{engine_analysis_button}->update;
	
	$self->minimax(0);
	
	$self->{engine_minimax_button}->configure(-text=>'Minimax');$self->{engine_minimax_button}->update;
	$self->{engine_analysis_button}->configure(-state=>'normal');$self->{engine_analysis_button}->update;
	
	$self->log("Minimax done $self->{minimax_node_count} node(s)\n");
	
	$self->move_made;
	
	$self->{minimax_in_progress}=0;

}

sub minimax_out_save
{
	my $self=shift;
	
	$self->{minimax_save}=1;
	
	$self->{log_t0}=[gettimeofday];
	
	$self->minimax_out;
	
	$self->{minimax_save}=0;
}

sub minimax
{
	
	my $self=shift;
	
	my $depth=shift;
	
	my $line=shift;

	my $current_pos=$self->{board}->report_fen(1);
	
	if(!exists($self->{node_book}->{$current_pos}->{legal_algeb_moves}))
	{
		
		if(!$self->{node_book}->{$current_pos}->{has_legal_checked})
		{
		
			$self->{node_book}->{$current_pos}->{has_legal_checked}=1;
		
			if(!$self->{board}->has_legal)
			{	
			
				$self->{node_book}->{$current_pos}->{has_legal_namely}='';

			}
			else
			{
			
				$self->{node_book}->{$current_pos}->{has_legal_namely}=$self->{board}->{has_legal_algeb};
			
			}
			
			############################################
			
			$self->save_fen($current_pos);
			
			############################################
			
		}
			
		my $has_legal_namely=$self->{node_book}->{$current_pos}->{has_legal_namely};
		
		if($has_legal_namely eq '')
		{	
			
			return 'unknown';
			
		}
	
		if($self->{after_move_book}->{$current_pos}->{$has_legal_namely}->{orig_eval} eq '')
		{	
			
			return 'unknown';
			
		}
	
	}
	
	my @legal_algeb_moves=$self->get_legal_algeb_moves($current_pos);
	
	if(@legal_algeb_moves==0)
	{
		return 'unknown';
	}
	
	my $best=$self->{board}->{turn}==1?-10000:10000;
	
	foreach(@legal_algeb_moves)
	{
	
		my $full_algeb=$_;
		
		my $eval=$self->{after_move_book}->{$current_pos}->{$full_algeb}->{orig_eval};
		
		if($eval eq '')
		{
		
			return 'unknown';
			
		}
		
		if($depth<=$self->{settings}->{dump_depth})
		{
			
			my $fen_after=$self->{after_move_book}->{$current_pos}->{$full_algeb}->{fen_after};
			
			my $returned_eval='unknown';
			
			my $clone=$self->{board}->clone;
			
			if($fen_after ne '')
			{
			
				$self->{board}->set_from_fen($fen_after);
				
				$returned_eval=$self->minimax($depth+1,"$line $full_algeb");
			
			}
			
			$self->{board}->copy($clone);
			
			if($returned_eval ne 'unknown')
			{
				$eval=$returned_eval;
			}
			
		}
		
		$self->{after_move_book}->{$current_pos}->{$full_algeb}->{eval}=$eval;
		
		if($self->{minimax_save})
		{
		
			$self->save_fen($current_pos);
			
		}
		
		$self->{minimax_node_count}++;
		
		if((!$self->{analysis_mode})&&($depth==3))
		{
			$self->log("$self->{minimax_node_count} : $line ( $eval )\n");
			$self->{log_window}->update;
		}
		
		$best=
			$self->{board}->{turn}==1
			?
			$eval>$best?$eval:$best
			:
			$eval<$best?$eval:$best
		;
		
	}
	
	return $best;
	
}

sub del_back_to_root
{
	my $self=shift;
	
	while(($self->{board}->report_fen(1) ne $self->{root}) && (@{$self->{moves}}>0) )
	{
		$self->del_move;
	}
	
	if( (@{$self->{moves}}==0) && ($self->{board}->report_fen(1) ne $self->{root}) )
	{
		print "fatal error";
		exit;
	}
}

sub determine_node
{

	my $self=shift;
	
		###################################################
		# unmake previous move unless analysis just started
	
		$self->{analysis_mode}=0;
		
		if(!$self->{analysis_started})
		{
			$self->del_move;
		}
		else
		{
			$self->{analysis_started}=0;
		}
		
		$self->{analysis_mode}=1;
		
		###################################################
	
	my $legal_algeb_moves=$self->{board}->legal_algeb_moves;

	my $full_algeb;

	my $current_pos=$self->{board}->report_fen(1);

	foreach(sort keys(%{$legal_algeb_moves}))
	{
	
		$full_algeb=$_;
		
		if($self->{after_move_book}->{$current_pos}->{$full_algeb}->{orig_eval} eq '')
		{
		
			my $line=join(' ',map { $_->{full_algeb} } @{$self->{moves}})." $full_algeb";
			
			$line=~s/^$self->{root_line}//;
			$line=~s/^ //;
		
			$self->log("Examining root + $line\n");
		
			$self->{analyzed_move}=$full_algeb;
		
			$self->make_move($full_algeb);
			
			$self->move_made;
			
			$self->{analyzing}=1;
			
			$self->{analysis_pos}=$current_pos;
			
			$self->analyze_pos;
			
			return;
			
		}
		
	}
	
	$self->{examined_nodes}++;
	
	$self->log("Position count: $self->{examined_nodes}\n");

	$self->{analysis_mode}=0;$self->del_back_to_root;$self->move_made;$self->{analysis_mode}=1;
	
	$self->{board_frame}->update;
	
	if( ( $self->{do_minimax} % $self->{settings}->{minimax_after} ) == 0 )
	{
		$self->minimax_out(1);
	}
	
	$self->{do_minimax}++;
	
	my $round;
	
	do
	{
		$round++;
		
		#$self->log("Selecting node, round $round\n");
		
		$self->{analysis_mode}=0;$self->del_back_to_root;$self->{analysis_mode}=1;
	}
	while(!$self->select_node(0));

}

sub update_uci_engine
{
	my $self=shift;
	
	my $msg;
	
	my $score;
	
	my $algeb;
	
	my $depth;
	
	while($wr->pending)
	{
		my $uci=$wr->dequeue;
		
		$uci=~s/\r|\n//g;
		
		if($uci=~/ pv (.*)/)
		{
			my $pv=$1;
			
			my @pv=split / /,$pv;
			
			my @pv_short;
			
			for(my $i=1;$i<(@pv>10?10:@pv);$i++)
			{
				push(@pv_short,$pv[$i]);
			}
			
			my $pv_short=join(' ',@pv_short);
			
			my $best=$pv[0];
			
			$algeb=$best;
			
			$uci=~/depth ([^\s]+)/;
			
			$depth=$1;
			
			$uci=~/score cp ([^\s]+)/;
			
			my $cp=$1;
			
			$uci=~/score mate ([^\s]+)/;
			
			my $mate=$1;
			
			$score=$cp ne ''?"$cp":"mate $mate";
			
			my $cp=$1;
			
			$msg="$depth $best $score $pv_short";
			
			$self->{uci_running}=1;

		}
		
		if($uci=~/bestmove ([^\s]+)/)
		{
			my $best=$1;
			
			$algeb=$best;
			
			$msg="best move: $best";
			
			$self->{uci_running}=0;
		}
		
		if($msg ne '')
		{
		
			$self->{engine_move}=$algeb;
		
			print "$msg\n";
			
			$self->{engine_line}->delete('1.0','end');
			$self->{engine_line}->insert('end',$msg);
			
			my $fill='#000000';
			
			if($score ne '')
			{
			
				my $tag='good';
				$fill='#007f00';
				
				if($score<100)
				{
					if($score>-100)
					{
						$tag='neutral';
						$fill='#00007f';
					}
					else
					{
						$tag='bad';
						$fill='#7f0000';
					}
				}
				
				$self->{engine_line}->tagAdd($tag,"1.0","end");
				
				$algeb=~/(.)(.)(.)(.)/;
			
				my $x1=ord($1)-ord('a');
				my $y1=7-(ord($2)-ord('1'));
				my $x2=ord($3)-ord('a');
				my $y2=7-(ord($4)-ord('1'));
				
				my $ij_1={i=>$x1,j=>$y1};
				my $screen_ij_1=$self->ij_to_screen_ij($ij_1);
				
				my $ij_2={i=>$x2,j=>$y2};
				my $screen_ij_2=$self->ij_to_screen_ij($ij_2);
				
				$self->del_engine_arrow;
				
				{
					$self->{engine_arrow}=$self->{canvas}->createLine(
					($screen_ij_1->{i}+0.5)*$square_size+$margin,($screen_ij_1->{j}+0.5)*$square_size+$margin,
					($screen_ij_2->{i}+0.5)*$square_size+$margin,($screen_ij_2->{j}+0.5)*$square_size+$margin,
					-arrow=>'last',
					-fill=>'#ff0000',
					-width=>2
					);
					
					$self->{canvas_score_text}=$self->{canvas}->createText(
					(($screen_ij_1->{i}+$screen_ij_2->{i})/2+0.5)*$square_size+$margin,
					(($screen_ij_2->{j}+$screen_ij_1->{j})/2+0.5)*$square_size+$margin,
					-text => "[$depth] $score",
					-font => [ -family => 'Courier', -size => 20, -weight=> 'bold' ],
					-fill => $fill
					);
				}
			
			}
			
		}
	}
}

sub update_engine
{
	my $self=shift;
	
	if($WINBY)
	{
		$self->update_uci_engine;
		
		goto update_engine_exit;
	}
	
	######################################################

	my $action;
	
	while($engine_action->pending)
	{
		$action=$engine_action->dequeue;
	}
	
	######################################################
	
	my $old_engine_status=$self->{engine_running};
	
	while($engine_status->pending)
	{
		$self->{engine_running}=$engine_status->dequeue;
	}
	
	if( ($old_engine_status) && (!$self->{engine_running}) && (!$self->{analysis_mode}) )
	{
		# normal engine analysis stopped
		
		$self->{analyze_pos_started}=0;
	}
	
	######################################################
	
	my $old_analysis_mode=$self->{analysis_mode};
	
	if(!($self->{analysis_mode}))
	{
	
		if($action eq 'analyze_pos')
		{
		
			if($self->{engine_on})
			{
				$self->analyze_pos;
				
				$self->{analyze_pos_started}=1;
			}
			
		}
			
		if($action eq 'stop')
		{
			$q->enqueue("$new_command");
		}
		
		if(!$self->{analyze_pos_started})
		{
		
			if($action eq 'change_analysis')
			{
			
				# start analysis
			
				my $root=$self->{board}->report_fen(1);
				
				$self->{root}=$root;
			
				$self->{analysis_mode}=1;
				
				$self->{do_minimax}=1;
				
				$self->{analysis_started}=1;
				
				$self->{examined_nodes}=0;
				
				$self->{log_t0}=[gettimeofday];
				
				$self->{root_line}=join(' ',map { $_->{full_algeb} } @{$self->{moves}});
				
				$self->{access_data_old}=$self->{access_data};
				
				$self->{access_data}='disabled';
				
				$self->open_log_window;
			
			}
		
		}
	
	}
	else
	{
	
		if($action eq 'change_analysis')
		{
		
			# stop analysis
		
			$self->{analysis_mode}=0;
			
			$self->{analyzing}=0;
			
			$q->enqueue("$new_command");
			
			$self->del_back_to_root;$self->move_made;
			
			$self->{access_data}=$self->{access_data_old};
			
			$self->close_log_window;
			
		}
		
	}
	
	######################################################
	
	if( ($self->{engine_running}!=$old_engine_status) || ($self->{analysis_mode}!=$old_analysis_mode) )
	{
		$self->{engine_frame}->configure(-background=>
			$self->{analysis_mode}?'#afafff':
			$self->{engine_running}?'#afffaf':$self->{engine_frame_inactive_background}
		);
		
		$self->{engine_analysis_button}->configure(-text=>$self->{analysis_mode}?'* Stop analysis *':'Start analysis');
		
		$self->{engine_analysis_button}->configure(-state=>$self->{analyze_pos_started}?'disabled':'normal');
		$self->{engine_go_button}->configure(-state=>$self->{analysis_mode}?'disabled':'normal');
		$self->{engine_stop_button}->configure(-state=>$self->{analysis_mode}?'disabled':'normal');
		$self->{engine_make_button}->configure(-state=>$self->{analysis_mode}?'disabled':'normal');
		$self->{engine_minimax_button}->configure(-state=>$self->{analysis_mode}?'disabled':'normal');
		
	}
	
	while($r->pending)
	{
	
		my $depth;
		my $eval;
	
		my $engine_line=$r->dequeue;
		
		$engine_line=~s/^\s+//;
		my @fields=split /\s+/,$engine_line;
		
		$depth=$fields[0]+0;
		$eval=$fields[1]+0;
		
		$self->{engine_line}->delete('1.0','end');
		$self->{engine_line}->insert('end',$engine_line);
		
		if(!$self->{analysis_mode})
		{
			
			my $tag='good';
			if($eval<200)
			{
				if($eval>-200)
				{
					$tag='neutral';
				}
				else
				{
					$tag='bad';
				}
			}
			
			$self->{engine_line}->tagAdd($tag,"1.0","end");
			
			$fields[2]=~/(.)(.)(.)(.)/;
			
			my $x1=ord($1)-ord('a');
			my $y1=7-(ord($2)-ord('1'));
			my $x2=ord($3)-ord('a');
			my $y2=7-(ord($4)-ord('1'));
			
			my $ij_1={i=>$x1,j=>$y1};
			my $screen_ij_1=$self->ij_to_screen_ij($ij_1);
			
			my $ij_2={i=>$x2,j=>$y2};
			my $screen_ij_2=$self->ij_to_screen_ij($ij_2);
			
			$self->del_engine_arrow;
			
			if($self->{engine_on})
			{
				$self->{engine_arrow}=$self->{canvas}->createLine(
				($screen_ij_1->{i}+0.5)*$square_size+$margin,($screen_ij_1->{j}+0.5)*$square_size+$margin,
				($screen_ij_2->{i}+0.5)*$square_size+$margin,($screen_ij_2->{j}+0.5)*$square_size+$margin,
				-arrow=>'last',
				-fill=>'#ff0000',
				-width=>2
				);
			}
			
			$self->{engine_move}=$fields[2];
			
		}
		else
		{
			
			# process engine in analysis mode
			
			$self->{engine_line}->tagRemove('good',"1.0","end");
			$self->{engine_line}->tagRemove('neutral',"1.0","end");
			$self->{engine_line}->tagRemove('bad',"1.0","end");
			
			if($depth>=$self->{settings}->{engine_depth})
			{
			
				my $eval=$self->{board}->{turn}==1?$eval:-$eval;
				
				$self->{after_move_book}->{$self->{analysis_pos}}->{$self->{analyzed_move}}->{orig_eval}=$eval;
				
				$self->{after_move_book}->{$self->{analysis_pos}}->{$self->{analyzed_move}}->{eval}=$eval;
				
				#########################################
				
				$self->save_fen($self->{analysis_pos});
				
				#########################################
				
				$q->enqueue("$new_command");
				
				$self->{analyzing}=0;
			}
			
		}
		
	}
	
	if( (!$self->{engine_running}) && ($self->{analysis_mode}) && (!$self->{analyzing}) )
	{
	
		$self->determine_node;
	
	}
	
	if(!$self->{engine_on}){$self->del_engine_arrow;}
	
	update_engine_exit:
	
	$self->{book_text}->tagRemove('sel','1.0','end');
	
	$self->{mw}->after(200,[\&update_engine,$self]);

}

sub save_engine_status
{
	my $self=shift;
	$self->{engine_on_old}=$self->{engine_on};
}

sub restore_engine_status
{
	my $self=shift;
	$self->{engine_on}=$self->{engine_on_old};
}

sub analyze_pos
{
	$self=shift;
	
	my $line=join( "\n", map { $_->{full_algeb} } @{$self->{moves}} );
	
	if($self->{settings}->{pulsar_on})
	{
	
		$q->enqueue("$new_command$line\n");
	
	}
	
	if(!$self->{analysis_mode})
	{
	
		if($self->{settings}->{atomkraft_on})
		{
			$self->copy_fen;
			
			$aq->enqueue("3\n$self->{settings}->{multipv}\ns\n1\n4\n");
		}
		
	}
}

sub change_analysis
{
	my $self=shift;
	
	$engine_action->enqueue('change_analysis');
}

sub inf
{
	my $self=shift;
	
	$wq->enqueue("i\n");
}

sub qinf
{
	my $self=shift;
	
	$wq->enqueue("q\n");
}

sub go
{
	my $self=shift;
	
	if($WINBY)
	{
		$wq->enqueue("go infinite\n");
		return;
	}
	
	$self->{engine_on}=1;
	
	$engine_action->enqueue('analyze_pos');
}

sub stop
{
	my $self=shift;
	
	if($WINBY)
	{
		$wq->enqueue("stop\n");
		return;
	}
	
	$self->{engine_on}=0;
	
	$engine_action->enqueue('stop');

	if($self->{settings}->{atomkraft_on})
	{
		$aq->enqueue("s\n");
	}
}

sub make_engine_move
{
	my $self=shift;
	
	return if $self->{analysis_mode};
	
	$self->make_move($self->{engine_move});
	
	$self->move_made;
}

#####################################################################################

sub reset
{

	my $self=shift;
	
	$self->{board}=new Board;
	
	$self->{flip}=0;
	
	$self->{moves}=[];
	$self->{unmoves}=[];
	
}

sub update_fen_by_pos
{
	
	my $self=shift;
	
	my $fen=shift;
	
	my $pos=shift;
	
	foreach(keys(%{$pos}))
	{
	
		my $key=$_;
		
		if($key eq 'node_book')
		{
		
			$self->{node_book}->{$fen}=$pos->{node_book};

		}
		else
		{

			foreach(keys(%{$pos->{$key}}))
			{
				
				my $move_key=$_;
				
				my $algeb=$key;
				
				my $value=$pos->{$algeb}->{$move_key};
				
				if($move_key eq 'comment')
				{
					$self->{book}->{$fen}->{$algeb}=$value;
				}
				else
				{
					$self->{after_move_book}->{$fen}->{$algeb}->{$move_key}=$value;
				}
				
			}

		}
		
	}
	
}

sub check_node
{

	my $self=shift;
	
	my $fen=shift;
	
	return if ($self->{access_data} eq 'disabled');
	
	my $pos=$self->save_fen($fen);
	
}

sub update_book

{
	my $self=shift;
	
	return if $self->{analysis_mode};
	
	$self->{book_text}->delete('1.0','end');
	
	my $fen=$self->{board}->report_fen(1);
	
	my $y=2;
	
	my @moves=keys(%{$self->{book}->{$fen}});
	
	@moves=sort
		{
			my $comment_sort = ( $comments->{$self->{book}->{$fen}->{$b}}->{value} <=> $comments->{$self->{book}->{$fen}->{$a}}->{value} ) ;
			
			return $comment_sort if $comment_sort!=0;
			
			$self->{board}->{turn}==1
			?
			$self->{after_move_book}->{$fen}->{$b}->{eval} <=> $self->{after_move_book}->{$fen}->{$a}->{eval}
			:
			$self->{after_move_book}->{$fen}->{$a}->{eval} <=> $self->{after_move_book}->{$fen}->{$b}->{eval}
			
		}
		@moves;
	
	$self->{book_text}->insert('end',"\n");
	
	foreach(@moves)
	{
		my $full_algeb=$_;
		my $comment=$self->{book}->{$fen}->{$full_algeb};
		$self->{book_text}->insert('end',sprintf("%-6s %-6s $comment_line \n",$full_algeb,$comment));
		
		my $x=14;
		foreach(@comments)
		{
			$self->{book_text}->tagAdd($comments->{$_}->{name},"$y.$x","$y.".($x+2));
			$x+=3;
		}
		
		if($comment ne '...')
		{
			$self->{book_text}->tagAdd($comments->{$comment}->{name},"$y.0","$y.13");
		}
		
		$self->{book_text}->tagAdd('back',"$y.13","$y.36");
		
		$y++;
		
		my $fen_after=$self->{after_move_book}->{$fen}->{$full_algeb}->{fen_after};
		$self->{book_text}->insert('end',"  -> $self->{after_move_book}->{$fen}->{$full_algeb}->{visited} , eval: $self->{after_move_book}->{$fen}->{$full_algeb}->{eval} , orig eval: $self->{after_move_book}->{$fen}->{$full_algeb}->{orig_eval}\n");
		
		$self->{book_text}->tagAdd('small',"$y.0","$y.80");
		
		$y++;
	}
	
	$self->{book_text}->tagAdd('margin',"1.0","end");
	
}

sub update_legal_moves
{
	my $self=shift;
	
	$self->{legal_moves_text}->delete('1.0','end');
	
	if($self->{settings}->{show_legal_moves})
		{
		
		$self->{board}->{dont_check_pgn}=0;
		my $legal_algeb_moves=$self->{board}->legal_algeb_moves;

		$self->{legal_moves_text}->delete('1.0','end');

		my @pgn_moves=map { '  '.$legal_algeb_moves->{$_}->{move}->{pgn_move} } keys(%{$legal_algeb_moves});
		@pgn_moves=sort @pgn_moves;
		$self->{legal_moves_text}->insert('end',"\n".join("\n",@pgn_moves));
		
		}
}

sub move_made
{
	my $self=shift;
	
	# make necessary updates after move
	
	my $fen=$self->{board}->report_fen(1);
	
	if($WINBY)
	{
	
		#$CLIP->Set($fen);
		#$wq->enqueue("f\n");
		
		my $command="position fen $fen\n";
		$wq->enqueue($command);
		
		if($self->{uci_running})
		{
			$wq->enqueue("stop\n");
			$wq->enqueue("go infinite\n");
		}
	
	}
	
	##################################
	
	$self->check_node($fen);
	
	##################################
	
	$self->update_legal_moves;
	
	$self->update_book;
	
	$self->draw_board;
	
	# analyze if needed
	
	if($self->{engine})
	{
		$self->go;
	}
	else
	{
		$engine_action->enqueue('analyze_pos');
	}

}

sub load_settings
{
	my $self=shift;
	
	open(SETTINGS,"settings.txt");
	my $settings=join('',<SETTINGS>);
	close(SETTINGS);
	
	eval($settings);
	
	if(!exists($self->{settings}->{dump_depth})){$self->{settings}->{dump_depth}="10";}
	if(!exists($self->{settings}->{depth_percent})){$self->{settings}->{depth_percent}="70";}
	if(!exists($self->{settings}->{max_depth})){$self->{settings}->{max_depth}="3";}
	if(!exists($self->{settings}->{engine_depth})){$self->{settings}->{engine_depth}="7";}
	if(!exists($self->{settings}->{minimax_after})){$self->{settings}->{minimax_after}="5";}
	if(!exists($self->{settings}->{book_name})){$self->{settings}->{book_name}='game.eval.txt';}
	if(!exists($self->{settings}->{book_extension})){$self->{settings}->{book_extension}='eval.txt';}
	
	########################################################################
	# options
	
	push(@{$self->{options}},new Option('MultiPV',\$self->{settings}->{multipv},3,[(map { $_ } 1..10)],sub { $self->save_settings; }));
	push(@{$self->{options}},new Option('Pulsar',\$self->{settings}->{pulsar_on},1,'check',sub { $self->save_settings; }));
	push(@{$self->{options}},new Option('Atomkraft',\$self->{settings}->{atomkraft_on},1,'check',sub { $self->save_settings; }));
	
	if(!exists($self->{settings}->{cutoff})){$self->{settings}->{cutoff}=5000;}
	
	foreach(@{$self->{options}})
	{
		my $option=$_;
		
		if(${$option->{textvariable}} eq '')
		{
			${$option->{textvariable}}=$option->{default};
		}
		
	}
	
	########################################################################
	
}

sub save_settings
{
	my $self=shift;
	
	open(SETTINGS,">settings.txt");
	print SETTINGS Data::Dumper->Dump([$self->{settings}],['$self->{settings}']);
	close(SETTINGS);
}

sub reset_game
{
	my $self=shift;
	
	return if $self->{analysis_mode};
	
	$self->{board}->reset;
	
	$self->{moves}=[];
	$self->{unmoves}=[];
	
	$self->stop;
	
	$self->{pgn_white}->delete('1.0','end');
	$self->{pgn_white}->insert('end',"?");
	$self->{pgn_white_elo}->delete('1.0','end');
	$self->{pgn_white_elo}->insert('end',"?");
	$self->{pgn_black}->delete('1.0','end');
	$self->{pgn_black}->insert('end',"?");
	$self->{pgn_black_elo}->delete('1.0','end');
	$self->{pgn_black_elo}->insert('end',"?");
	$self->{pgn_date}->delete('1.0','end');
	$self->{pgn_date}->insert('end',"?");
	$self->{pgn_result}->delete('1.0','end');
	$self->{pgn_result}->insert('end',"?-?");
	$self->{pgn_time_control}->delete('1.0','end');
	$self->{pgn_time_control}->insert('end',"?");
	$self->{pgn_ply_count}->delete('1.0','end');
	$self->{pgn_ply_count}->insert('end',"?");
	$self->{pgn_dir}->delete('1.0','end');
	$self->{pgn_dir}->insert('end',"default.pgn");
	
	$self->{mw}->configure(-title=>'Atomic Chess');
	
	$self->move_made;
	
	my $bck=$self->{load_1_button}->cget(-background);
	my $abck=$self->{load_1_button}->cget(-activebackground);
	for(my $j=1;$j<=5;$j++)
	{
		$self->{"save_".$j."_button"}->configure(-background=>$bck,-activebackground=>$abck);
	}
}

sub flip_board
{
	my $self=shift;
	
	$self->{flip}=!$self->{flip};
	
	$self->draw_board;
}

sub canvas_to_square
{
	my $what=shift;
	
	$what-=$margin;
	$what-= ( $what % $square_size );
	$what/=$square_size;
	
	return $what;
}

sub drag_start {

    my ($c,$self) = @_;
	
	return if $self->{analysis_mode};
	
    my $e = $c->XEvent;
    my ( $sx, $sy ) = ( $e->x, $e->y,,, );
    my ( $cx, $cy ) = ( $c->canvasx($sx), $c->canvasy($sy) );
    my $id = $c->find( 'withtag', 'current' );
    my ( $x1, $y1, $x2, $y2 ) = $c->bbox($id);
	
    $draginfo{id}     = $id;
    $draginfo{startx} = $draginfo{lastx} = $cx;
    $draginfo{starty} = $draginfo{lasty} = $cy;
	
	my $s_i=canvas_to_square($sx);
	my $s_j=canvas_to_square($sy);
	
	my $screen_ij={i=>$s_i,j=>$s_j};
	
	my $ij=$self->screen_ij_to_ij($screen_ij);
	
	my $s_col=($ij->{i}%2?!($ij->{j}%2):$ij->{j}%2);
	
	my $p=$c->itemcget($draginfo{id},-text);
	
	my $p_col=0;
	
	if($p=~/[PNBRQKpnbrqk]/){$p_col=1;}
	
	$p=~tr/+PNBRQKOMVTWL/ pnbrqkomvtwl/;
	
	$c->createText(
			$screen_ij->{i}*$square_size+$margin+$square_size/2,$screen_ij->{j}*$square_size+$square_size/2+$margin,
			-text => $s_col?'+':' ',
			-font => [ -family => 'Chess Merida', -size => $piece_size ],
			);
	
	$c->itemconfigure($draginfo{id},
		-text=>$p,
		-font => [ -family => 'Chess Merida', -size => $piece_size*1.1 ],
		-fill => $p_col?'#0000ff':'#00007f');

}

sub drag_during {
	
	my ($c) = @_;
	my $e = $c->XEvent;
	my ( $sx, $sy ) = ( $e->x, $e->y,,, );
	my ( $cx, $cy ) = ( $c->canvasx($sx), $c->canvasy($sy) );
	my ( $dx, $dy ) = ( $cx - $draginfo{lastx}, $cy - $draginfo{lasty} );
	$c->move( $draginfo{id}, $dx, $dy );
	$draginfo{lastx} = $cx;
	$draginfo{lasty} = $cy;
	
	my ( $x1, $y1, $x2, $y2 ) = $c->bbox( $draginfo{id} );
	
}

sub make_move
{
	my $self=shift;
	
	my $full_algeb=shift;
	
	$self->{board}->{dont_check_pgn}=1;
	
	my $fen_before=$self->{board}->report_fen(1);
	
	if($self->{board}->make_algeb_move($full_algeb))
	{
	
		$self->{unmoves}=[];
		
		my $fen_after=$self->{board}->report_fen(1);
		
		push(@{$self->{moves}},{full_algeb=>$full_algeb,fen_before_move=>$fen_before,fen_after_move=>$fen_after});
		
		my $comment=$self->{book}->{$fen_before}->{$full_algeb};
		
		if($comment eq '')
		{
			$self->{book}->{$fen_before}->{$full_algeb}='...';
			
			$self->check_node($fen_before);
		}
		
		my $fen_after=$self->{board}->report_fen(1);
		
		$self->{after_move_book}->{$fen_before}->{$full_algeb}->{fen_after}=$fen_after;
		
		$self->{after_move_book}->{$fen_before}->{$full_algeb}->{visited}++;
		
		return(1);
		
	}
	else
	{
		return(0);
	}
}

sub drag_end {

	shift;
	$self=shift;
	
	return if $self->{analysis_mode};
	
	my $start_i=canvas_to_square($draginfo{startx});
	my $end_i=canvas_to_square($draginfo{lastx});
	my $start_j=canvas_to_square($draginfo{starty});
	my $end_j=canvas_to_square($draginfo{lasty});
	
	my $start_screen_ij={i=>$start_i,j=>$start_j};
	my $start_ij=$self->screen_ij_to_ij($start_screen_ij);
	
	my $end_screen_ij={i=>$end_i,j=>$end_j};
	my $end_ij=$self->screen_ij_to_ij($end_screen_ij);
	
	my $diff_i=$end_i-$start_i;
	my $diff_j=$end_j-$start_j;
	
	#############################################
	# process move
	
	my $start_pos=Board::ij_to_pos($start_ij->{i},$start_ij->{j});
	my $end_pos=Board::ij_to_pos($end_ij->{i},$end_ij->{j});
	
	my $start_algeb=Board::pos_to_algeb($start_pos);
	my $end_algeb=Board::pos_to_algeb($end_pos);
	
	my $algeb=$start_algeb->{algeb}.$end_algeb->{algeb};
	
	my $orig_piece=$self->{board}->get_piece_at_pos($start_pos);
	
	my $prom_algeb;
	
	if(
		(
			($orig_piece eq 'P') && ($start_ij->{j}==1) && ($diff_i==0)
		)
		||
		(
			($orig_piece eq 'p') && ($start_ij->{j}==6) && ($diff_i==0)
		)
	)
	{
	
		$answer = $self->{mw}->Dialog(-title => 'Promote', 
			   -text => 'Select piece', 
			   -default_button => 'Queen', -buttons => [ 'Queen', 'Knight', 'Bishop','Rook'], 
			   -bitmap => 'question' )->Show( );
		
		$answer=~/^(.)/;$prom_algeb=$1;$prom_algeb=~tr/[kK]/[nN]/;
		$prom_algeb=~tr/[A-Z]/[a-z]/;
	}
	
	my $full_algeb="$algeb$prom_algeb";
	
	$self->make_move($full_algeb);
	
	$self->move_made;
	
	#############################################
	
    %draginfo = ();
}

sub del_move
{

	my $self=shift;
	
	return if $self->{analysis_mode};
	
	my $dont_show=shift;
	
	if(@{$self->{moves}}>0)
	{
		my $move=pop(@{$self->{moves}});
		push(@{$self->{unmoves}},$move);
		
		$self->{board}->set_from_fen($move->{fen_before_move});
		
		if($dont_show)
		{
		}
		else
		{
			$self->move_made;
		}
		
		return(1);
	}
	
	return(0);
}

sub undel_move
{

	my $self=shift;
	
	return if $self->{analysis_mode};
	
	my $dont_show=shift;
	
	if(@{$self->{unmoves}}>0)
	{
		my $move=pop(@{$self->{unmoves}});
		push(@{$self->{moves}},$move);
		
		$self->{board}->set_from_fen($move->{fen_after_move});
		
		if($self->{book}->{$move->{fen_before_move}}->{$move->{full_algeb}} eq '')
		{
		
			$self->{book}->{$move->{fen_before_move}}->{$move->{full_algeb}}='...';
			
			$self->check_node($move->{fen_before_move});
			
		}
		
		if($dont_show)
		{
		}
		else
		{
			$self->move_made;
		}
		
		return(1);
	}
	
	return(0);
}

sub del_all_moves
{

	my $self=shift;
	
	return if $self->{analysis_mode};
	
	$self->{engine}=0;
	$self->stop;
	
	while($self->del_move(1)){};
	
	$self->move_made;
}

sub undel_all_moves
{
	
	my $self=shift;
	
	return if $self->{analysis_mode};
	
	$self->{engine}=0;
	$self->stop;
	
	while($self->undel_move(1)){};
	
	$self->move_made;
}

sub set_from_fen
{
	my $self=shift;
	
	my $fen=$CLIP->Get;
	
	$self->{board}->set_from_fen($fen);
	
	$self->move_made;
	
}

sub copy_fen
{
	my $self=shift;
	
	my $fen=$self->{board}->report_fen;
	
	$CLIP->Set($fen);
	
}

sub show_pgn
{
	my $self=shift;
	
	my $full_pgn=shift;
	
	my $width=shift;
	
	my $show_pgn=$self->{mw}->Toplevel(-title=>'PGN');
	$show_pgn->geometry('+5+5');

	my $show_pgn_text=$show_pgn->Scrolled('Text',%pgn_text_attr,-width=>($width or 60),-height=>30,-scrollbars=>$width eq ''?'e':'se',-wrap=>$width eq ''?'word':'none')->pack;
	
	$show_pgn_text->insert('end',$full_pgn);
}

sub copy_pgn
{
	my $self=shift;
	
	return if $self->{analysis_mode};
	
	my $save=shift;
	
	$self->{board_frame}->update;
	
	my @pgn_body;
	my $cnt=1;
	my $half_move_cnt=0;
	
	my $old=$self->{board}->clone;
	
	my $clone=new Board;
	
	my $number_of_moves=@{$self->{moves}}+0;
	
	my $t0=[gettimeofday];
	
	foreach(@{$self->{moves}})
	{
	
		my $move=$_;
		
		$half_move_cnt++;
		
		my $full_algeb=$move->{full_algeb};
		
		$full_algeb=~/^(..)/;
		my $orig_algeb=$1;
		
		$clone->set_from_fen($move->{fen_before_move});
		
		my $piece=$clone->get_piece_at_pos(Board::algeb_to_pos($orig_algeb));

		$clone->{dont_check_pgn}=0;
		my $legal_algeb_moves=$clone->legal_algeb_moves($piece);
		
		my $pgn_move=$legal_algeb_moves->{$move->{full_algeb}}->{move}->{pgn_move};
		
		my $move_number;
		if($clone->{turn}==1)
		{
			$move_number="$cnt. ";
		}
		else
		{
			$cnt++;
		}
		
		push(@pgn_body,"$move_number$pgn_move");
		
		$self->{board}->set_from_fen($move->{fen_after_move});
		
		if(0)
		{
			$self->draw_board();
			$self->{board_frame}->update;
		}
		
		$elapsed = tv_interval ($t0, [gettimeofday]);
		
		$self->report("processing move $half_move_cnt of $number_of_moves ( elapsed $elapsed )");
		
	}
	
	my $pgn_body=join(' ',@pgn_body);
	
	my $white=$self->{pgn_white}->get('1.0','end-1c');
	my $white_elo=$self->{pgn_white_elo}->get('1.0','end-1c');
	my $black=$self->{pgn_black}->get('1.0','end-1c');
	my $black_elo=$self->{pgn_black_elo}->get('1.0','end-1c');
	my $date=$self->{pgn_date}->get('1.0','end-1c');
	my $result=$self->{pgn_result}->get('1.0','end-1c');
	my $time_control=$self->{pgn_time_control}->get('1.0','end-1c');
	my $ply_count=$self->{pgn_ply_count}->get('1.0','end-1c');
	my $dir=$self->{pgn_dir}->get('1.0','end-1c');
	
	my $sep=' ';
	if($pgn_body eq ''){$sep='';}
	
	my $Variant=$Board::variant;
	my $current_fen=$self->{board}->report_fen;
	
	$Variant=~s/^([a-z])/\u$1/;
	
	my $full_pgn=qq([White "$white"]
[WhiteElo "$white_elo"]
[Black "$black"]
[BlackElo "$black_elo"]
[Date "$date"]
[Result "$result"]
[TimeControl "$time_control"]
[PlyCount "$ply_count"]
[Variant "$Variant"]
[Flip "$self->{flip}"]
[CurrentFen "$current_fen"]

$pgn_body$sep$result

);
	
	if($save)
	{	
		
		open(OUTF,">$self->{settings}->{pgn_dir}$dir");
		print OUTF $full_pgn;
		close(OUTF);
		
	}
	
	if( ($self->{settings}->{show_pgn}) || (!$save) )
	{
		#$CLIP->Set($full_pgn);
		
		$self->show_pgn($full_pgn);
	}
	
	$self->{board}->copy($old);
	
}

sub import_i
{
	my $self=shift;
	
	my $i=shift;
	
	$self->reset_game;
	
	open(GAME,"../Perl2/game$i.txt");
	my $game=join('',<GAME>);
	close(GAME);
	
	my @moves=split /  /,$game;
	
	foreach(@moves)
	{
		my $move=$_;
		$move=~s/X//;
		my $prom_algeb;
		if($move=~/\*(.)/)
		{
			$prom_algeb=$1;
			$prom_algeb=~tr/[A-Z]/[a-z]/;
		}
		$move=~/^(....)/;
		my $full_algeb="$1$prom_algeb";
		
		$self->make_move($full_algeb);
	}
	
	$self->draw_board;
}

sub save_i
{

	my $self=shift;
	
	return if $self->{analysis_mode};
	
	my $i=shift;
	
	my $ll=$self->{last_loaded};
	
	if(($ll ne '')&&($ll!=$i))
	{
		$answer = $self->{mw}->Dialog(-title => 'Overwriting file!',
			
			-width=> 30,
			-text => 'Are you sure?', 
			-default_button => 'No !', -buttons => [ 'No !','Yes :(' ], 
			-bitmap => 'question' )->Show( );
			   
		return if $answer=~/No/;
	}
	
	open(GAME,">game$i.txt");
	print GAME Data::Dumper->Dump([$self->{moves},$self->{flip}],['$self->{moves}','$self->{flip}']);
	close(GAME);
	
	($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks)=stat("game$i.txt");
	
	$self->report("game saved $size characters");
}

sub load_i
{

	my $self=shift;
	
	return if $self->{analysis_mode};
	
	my $i=shift;
	
	$self->reset_game;
	
	open(GAME,"game$i.txt");
	my $game=join('',<GAME>);
	close(GAME);
	
	eval($game);
	
	if(@{$self->{moves}}>0)
	{
		my $last_move=pop(@{$self->{moves}});
		push(@{$self->{moves}},$last_move);
		
		$self->{board}->set_from_fen($last_move->{fen_after_move});
	}
	
	$self->move_made;
	
	($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks)=stat("game$i.txt");
	
	$self->report("game loaded $size characters");
	
	for(my $j=1;$j<=5;$j++)
	{
		$self->{"save_".$j."_button"}->configure(-background=>$i==$j?'#afffaf':'#ffafaf',-activebackground=>$i==$j?'#7fff7f':'#ff7f7f');
	}
	
	$self->{last_loaded}=$i;
}

sub load_book
{

	my $self=shift;
	
	my $name=$self->{settings}->{book_name};
	
	my $extension=$self->{settings}->{book_extension};
	
	my $dir=$self->{settings}->{book_dir};
	
	if(-e $dir)
	{
		
		$self->{save_book_button}->configure(-text=>"Save [ $self->{settings}->{book_name} ]");
		
		$self->save_settings;
		
		my $length;
		
		if($extension eq 'eval.txt')
		{
		
			open(BOOK,$dir);
			my $book_eval=join('',<BOOK>);
			close(BOOK);
			
			$length=length($book_eval);

			eval($book_eval);
			
		}
		elsif($extension eq 'json.txt')
		{
		
			open(BOOK_JSON,$dir);
			my $book_json=join('',<BOOK_JSON>);
			close(BOOK_JSON);
			
			$length=length($book_json);
			
			my $json_decoded=decode_json $book_json;
			
			$self->{book}=$json_decoded->{book};
			$self->{after_move_book}=$json_decoded->{after_move_book};
			$self->{node_book}=$json_decoded->{node_book};
			
		}
		
		# purge book
		if(0)
		{
			foreach(keys(%{$self->{after_move_book}}))
			{
				my $fen=$_;
				foreach(keys(%{$self->{after_move_book}->{$fen}}))
				{
					my $full_algeb=$_;
					delete($self->{after_move_book}->{$fen}->{$full_algeb}->{board_after});
				}
			}
		}
		
		$self->{save_as_name}->delete('1.0','end');
		
		my $name_without_extension=$name;
		$name_without_extension=~s/\.$extension$//;
		
		$self->{save_as_name}->insert('end',$name_without_extension);
		
		my $dir_without_name=$dir;
		$dir_without_name=~s/$name$//;
		
		$self->{save_as_dir}->delete('1.0','end');
		$self->{save_as_dir}->insert('end',$dir_without_name);
		
		$self->{save_as_extension}=$extension;
		
		$self->report("book $dir loaded $length characters");
		
		$self->move_made;
	
	}
	else
	{
	
		$self->report("error: book $dir does not exist");
		
	}
	
}

sub save_as
{

	my $self=shift;
	
	my $save_as_dir=$self->{save_as_dir}->get('1.0','end-1c');
	my $save_as_name=$self->{save_as_name}->get('1.0','end-1c');
	my $extension=$self->{save_as_extension};
	
	if(!opendir(TEST_DIR,$save_as_dir))
	{
		$self->report("error: invalid directory");
	}
	else
	{
		
		$self->{settings}->{book_name}="$save_as_name.$extension";
		$self->{settings}->{book_extension}=$extension;
		
		$self->{settings}->{book_dir}=$save_as_dir.$self->{settings}->{book_name};
		
		$self->save_book;
	}

}

sub fen_to_dir
{
	my $self=shift;
	
	my $fen=shift;
	
	return ("Data/".fen_to_hex($fen).".txt");
}

sub pos_from_data
{
	my $self=shift;
	
	my $dir=shift;
	
	return {} if(!(-e $dir));
	
	open(POS,"$dir");
	my $pos=decode_json join('',<POS>);
	close(POS);
	
	return $pos;
}

sub pos_to_data
{
	my $self=shift;
	
	my $fen=shift;
	
	my $pos=shift;
	
	return if(keys(%{$pos})<=0);
	
	my $dir=$self->fen_to_dir($fen);
	
	open(POS,">$dir");
	my $pos_encoded=encode_json $pos;
	print POS $pos_encoded;
	close(POS);
}

sub list_data
{
	my $self=shift;
	
	opendir(DATA,'Data');
	
	while($dir=readdir(DATA))
	{
		if($dir=~/^([^\.]+)\.txt$/)
		{
		
			my $hex=$1;
		
			my $fen=hex_to_fen($hex);
			
			my $pos=$self->pos_from_data("Data/$dir");
			
			print "$fen\n",Data::Dumper->Dump([$pos]);
		}
	}

}

sub pos_from_fen
{
	my $self=shift;
	
	my $fen=shift;
	
	my $pos=shift;
		
	if(exists($self->{book}->{$fen}))
	{
		foreach(keys(%{$self->{book}->{$fen}}))
		{
			my $algeb=$_;
			
			my $comment_data=$pos->{$algeb}->{comment};
			
			my $comment_book=$self->{book}->{$fen}->{$algeb};
			
			if($comment_data eq '')
			{
			
				$pos->{$algeb}->{comment}=$comment_book;
				
			}
			elsif($comment_book eq '...')
			{
			
				# don't overwrite any comment with uncommented
				
			}
			else
			{
			
				$pos->{$algeb}->{comment}=$comment_book;
				
			}
			
		}
	}
	
	if(exists($self->{after_move_book}->{$fen}))
	{
		foreach(keys(%{$self->{after_move_book}->{$fen}}))
		{
			my $algeb=$_;
			
			foreach(keys(%{$self->{after_move_book}->{$fen}->{$algeb}}))
			{
			
				my $key=$_;
			
				$pos->{$algeb}->{$key}=$self->{after_move_book}->{$fen}->{$algeb}->{$key};

			}
		}
	}
	
	if(exists($self->{node_book}->{$fen}))
	{
		foreach(keys(%{$self->{node_book}->{$fen}}))
		{
			my $key=$_;
			
			$pos->{node_book}->{$key}=$self->{node_book}->{$fen}->{$key};
		}
	}
	
	return $pos;

}

sub save_fen
{

	my $self=shift;
	
	my $fen=shift;
	
	my $pos=$self->pos_from_fen($fen,$self->pos_from_data($self->fen_to_dir($fen)));

	$self->pos_to_data($fen,$pos);
	
	$self->update_fen_by_pos($fen,$pos);
	
	return $pos;
	
}

sub export_book
{
	my $self=shift;
	
	my @keys=(keys(%{$self->{book}}),keys(%{$self->{after_move_book}}),keys(%{$self->{node_book}}));
	
	my @joined_keys=keys(%{{map { $_=>undef } @keys}});
	
	foreach(@joined_keys)
	{
		my $fen=$_;
		
		$self->save_fen($fen);
	}
	
	#$self->list_data;
}

sub save_book
{

	my $self=shift;
	
	return if $self->{analysis_mode};
	
	my $dir=$self->{settings}->{book_dir};
	
	my $ok;
	
	$self->{save_book_button}->configure(-text=>"Save book [ $self->{settings}->{book_name} ]");
	
	$self->save_settings;
	
	if($self->{settings}->{book_extension} eq 'eval.txt')
	{
	
		if(open(BOOK,">$dir"))
		{
			print BOOK Data::Dumper->Dump([$self->{book},$self->{after_move_book}],['$self->{book}','$self->{after_move_book}']);
			close(BOOK);
			$ok=1;
			
		}
		
	}
	elsif($self->{settings}->{book_extension} eq 'json.txt')
	{
	
		if(open(JSON,">$dir"))
		{
			print JSON encode_json {book=>$self->{book},after_move_book=>$self->{after_move_book},node_book=>$self->{node_book}};
			close(JSON);
			
			$ok=1;
		}

	}
	
	
	if($ok)
	{
		my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$book_size,$atime,$mtime,$ctime,$blksize,$blocks)=stat($dir);
		
		$self->report("book saved $book_size characters to $dir");
	}
	else
	{
		$self->report("error: unable to write to $dir");
	}
	
}

sub game_clicked
{
	
	shift;
	my $self=shift;
	
	return if $self->{analysis_mode};
	
	my $coords=Utils::split_text_index($self->{game_text}->index('current'));
	
	my $move_no=int($coords->{x}/$self->{game_text_move_width})+1;
	
	my $cur_move_no=@{$self->{moves}}+0;
	my $del_move_no=@{$self->{unmoves}}+0;
	my $all_move_no=$cur_move_no+$del_move_no;
	
	return if(($move_no<0)||($move_no>$all_move_no));
	
	do
	{
	
		$cur_move_no=@{$self->{moves}}+0;
		
		$move_no<$cur_move_no?$self->del_move:$self->undel_move;
		
	}while(@{$self->{moves}}!=$move_no);
	
	
}

sub book_clicked
{
	
	shift;
	my $self=shift;
	
	return if $self->{analysis_mode};
	
	my $coords=Utils::split_text_index($self->{book_text}->index('current'));
	
	my $line=$self->{book_text}->get("$coords->{y}.0","$coords->{y}.5");
	
	$line=~/^([^ ]+)/;
	
	my $full_algeb=$1;
	
	if($full_algeb=~/^[a-z0-9]{4,5}/)
	{
		
		if($coords->{x}<6)
		{
			$self->make_move($full_algeb);
			
			$self->move_made;
		}
		elsif($coords->{x}>13)
		{
			my $c=int(($coords->{x}-14)/3);
			my $cc=$comments[$c];
			
			my $fen=$self->{board}->report_fen(1);
			$self->{book}->{$fen}->{$full_algeb}=$cc;
			
			if($WINBY)
			{
				$wq->enqueue("o$full_algeb=$cc\n");
			}
			
			$self->move_made;
		}
		
	}
}

sub dump_book
{
	my $self=shift;
	
	return if $self->{analysis_mode};
	
	$self->reset_game;
	
	open(DUMP,">$dump_txt");
	$self->dump_recursive(0,'');
	close(DUMP);
	
	$self->reset_game;
	
	($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks)=stat($dump_txt);
	
	$self->report("book dumped $size characters");
	
	open(DUMP,$dump_txt);
	my $dump=join('',<DUMP>);
	$self->show_pgn($dump,120);
	close(DUMP);
	
}

sub dump_recursive
{
	my $self=shift;
	
	my $level=shift;
	
	my $line=shift;
	
	if($level>$self->{settings}->{dump_depth})
	{
		print DUMP "$line\n";
		return;
	}
	
	my $fen=$self->{board}->report_fen(1);
	
	my @keys=keys(%{$self->{after_move_book}->{$fen}});
	
	if(@keys>0)
	{
		foreach(@keys)
		{
			my $full_algeb=$_;
			
			my $fen_after=$self->{after_move_book}->{$fen}->{$full_algeb}->{fen_after};
			
			$self->{board}->set_from_fen($fen_after);
			
			my $comment=$self->{book}->{$fen}->{$full_algeb};if($comment eq '...'){$comment='';}else{$comment=" $comment";}
			
			$self->dump_recursive($level+1,"$line $full_algeb$comment");
		}
	}
	else
	{
		print DUMP "$line\n";
	}
	
}

sub load_pgn
{

	my $self=shift;
	
	return if $self->{analysis_mode};
	
	my $extension=shift;
	
	my $dont_create=shift;
	
	my $pgn_dir=$self->{settings}->{pgn_dir};
	
	if(!$dont_create)
	{
		$self->{show_pgn}=$self->{mw}->Toplevel();
		$self->{show_pgn}->geometry('+5+5');
		
		$self->{show_pgn_text}=$self->{show_pgn}->Scrolled('Text',%pgn_text_attr,-width=>40,-height=>30,-scrollbars=>'e',-insertontime=>0)->pack(-side=>'left');
		
		$self->{show_pgn_text}->bind('<Button-1>',[\&show_pgn_clicked,$self,$extension]);
		
		if($extension eq 'pgn')
		{
			$self->{show_pgn_boards}=$self->{show_pgn}->Scrolled('Text',%pgn_text_attr,-foreground=>'#000000',
				-font => [ -family => 'Chess Merida', -size => 14 ],
				-width=>12,-height=>34,-scrollbars=>'e',-insertontime=>0)->pack(-side=>'right');
				
			$self->{show_pgn_boards}->tagConfigure('comment',-font=>[-family=>'Courier',-size=>10]);
			
			$self->{show_pgn_boards}->bind('<Button-1>',[\&show_pgn_clicked,$self,'boards']);
			
		}
		
	}
	
	$self->{show_pgn}->configure(-title=>$pgn_dir);
	
	$self->{show_pgn_text}->delete('1.0','end');
	
	opendir(DIR,$pgn_dir);
	
	my @dirs;
	
	my $dir;
	
	while($dir=readdir(DIR))
	{
		
		if( ($dir=~/\.$extension$/) || opendir(DUMMY,"$pgn_dir$dir") )
		{
		
			push(@dirs,$dir);
		
		}
		
	}
	
	@dirs=reverse(@dirs);
	
	my $index=0;
	
	foreach(@dirs)
	{
		
		$dir=$_;
		
		{
		
			$self->{show_pgn_text}->insert('1.0',"$dir\n");
			
			if($extension eq 'pgn')
			{
			
				my $pgn=$self->parse($dir);
				
				my $current_fen=$pgn->{headers}->{CurrentFen};
				
				if($current_fen ne '')
				{
				
					my $board=new Board;
					
					$board->set_from_fen($current_fen);
					
					my $rep=$board->board_rep;
					
					$rep=join("\n",$rep=~/.{8}/g);
					
					if($pgn->{headers}->{Flip})
					{
						$rep=join('',reverse(split //,$rep));
					}
				
					$self->{show_pgn_boards}->insert('1.0',"$rep\n\n");
					
					my $dir_name=$dir;$dir_name=~s/\.pgn$//;
					
					$self->{show_pgn_boards}->insert('1.0',"$dir_name\n\n");
					$self->{show_pgn_boards}->tagAdd('comment','1.0','1.40');
					
					$self->{pgn_list}->{$index}=$dir;
					
					$index++;
				
				}
			
			}
			
		}
		
	}
	
	$self->{pgn_list_size}=$index;
	
}

sub parse
{

	my $self=shift;
	my $dir=shift;
	
	my $pgn={dir=>$dir};
	open(PGN,"$self->{settings}->{pgn_dir}$dir");
	while(my $line=<PGN>)
	{
		$pgn->{full_pgn}.=$line;
		
		$line=~s/[\n\r]//g;
		if($line=~/\[([^\]]+)\]/)
		{
			my $header=$1;
			my @header=split /\s+/,$header;
			my $key=$header[0];
			my $value=$header[1];$value=~s/\"//g;
			$pgn->{headers}->{$key}=$value;
		}
		else
		{
			$pgn->{body}.="$line ";
		}
	}
	close(PGN);
	
	# remove newlines, excess white spaces
	
	$pgn->{body}=~s/^\s+//g;
	$pgn->{body}=~s/\s+$//g;
	while($pgn->{body}=~s/  / /){};
	$pgn->{body}=~s/([0-9]+\.)\s+/$1/g;
	$pgn->{headers}->{game}=$pgn->{body};
	
	$pgn->{raw}=$pgn->{body};
	
	# establish reason

	my $reason;
	
	while($pgn->{raw}=~s/\{([^\}]*)\}//){$reason.=$1;}
	
	$reason=~tr/[A-Z]/[a-z]/;
	$reason=~s/^\s*//;
	$reason=$reason eq ''?' forced':" $reason";
	
	$pgn->{reason}=$reason;
	
	$pgn->{raw}=~s/[0-9]+\.//g;
	
	while($pgn->{raw}=~s/  / /){};
	
	#remove numerical result if any
	
	my @pgn_raw=split / /,$pgn->{raw};
	my $numresult=pop(@pgn_raw);
	
	if($numresult=~/[0-9\/\?]+\-[0-9\/\?]+$/)
	{
		$pgn->{numresult}=$numresult;
		$pgn->{raw}=join(' ',@pgn_raw);
	}
	
	return $pgn;
}

sub report
{

	my $self=shift;
	
	my $what=shift;

	$self->{status_line}->delete('1.0','end');
	$self->{status_line}->insert('end'," # $what");
	if($self->{settings}->{update_status_bar}){$self->{status_frame}->update;}
	
}

sub do_load_pgn
{

	my $self=shift;
	
	my $line=shift;

	my $pgn=$self->parse($line);
	
	my @moves=split / /,$pgn->{raw};
	
	$self->reset_game;
	
	my $number_of_moves=@moves+0;
	my $cnt=1;
	my $success=0;
	
	my @failed;
	
	my $t0=[gettimeofday];
	
	my $show_legal=$self->{settings}->{show_legal_moves};
	
	foreach(@moves)
	{
		my $pgn_move=$_;
		
		$pgn_move=~/^(.)/;
		
		my $piece=$1;
		
		my $is_black_turn=$self->{board}->{turn}==-1;
		
		if($piece=~/[NBRQK]/)
		{
			if($is_black_turn)
			{
				$piece=$Board::black_of_piece{$piece};
			}
		}
		elsif($piece eq 'O')
		{
			$piece=$is_black_turn?'k':'K';
		}
		else
		{
			$piece=$is_black_turn?'p':'P';
		}
	
		$self->{board}->{dont_check_pgn}=0;
		my $legal_algeb_moves=$self->{board}->legal_algeb_moves($piece);
		
		$self->{settings}->{show_legal_moves}=0;
		
		my $found;
		
		foreach(keys(%{$legal_algeb_moves}))
		{
			my $full_algeb=$_;
			
			if($legal_algeb_moves->{$full_algeb}->{move}->{pgn_move} eq  $pgn_move)
			{
			
				my $fen_before=$self->{board}->report_fen(1);
			
				$self->make_move($full_algeb);
				
				if(0)
				{
				
					$self->move_made;
					
					$self->{board_frame}->update;
					
				}
				else
				{
					
					$self->check_node($fen_before);
					
				}
				
				$found=1;
				
			}
			
		}
		
		$success+=$found;
		
		if(!$found)
		{
			push(@failed,"$cnt $pgn_move");
		}
		
		my $failed;
		
		if(@failed>0)
		{
			$failed=" ( failed: ".join(' , ',@failed)." )";
		}
		
		my $elapsed=tv_interval($t0,[gettimeofday]);
		
		$self->report("processing move $cnt of $number_of_moves$failed ( elapsed $elapsed )");
		
		$cnt++;

	}
	
	$self->{settings}->{show_legal_moves}=$show_legal;
	
	$self->{pgn_white}->delete('1.0','end');
	$self->{pgn_white}->insert('end',$pgn->{headers}->{White});
	$self->{pgn_white_elo}->delete('1.0','end');
	$self->{pgn_white_elo}->insert('end',$pgn->{headers}->{WhiteElo});
	$self->{pgn_black}->delete('1.0','end');
	$self->{pgn_black}->insert('end',$pgn->{headers}->{Black});
	$self->{pgn_black_elo}->delete('1.0','end');
	$self->{pgn_black_elo}->insert('end',$pgn->{headers}->{BlackElo});
	$self->{pgn_date}->delete('1.0','end');
	$self->{pgn_date}->insert('end',$pgn->{headers}->{Date});
	$self->{pgn_result}->delete('1.0','end');
	$self->{pgn_result}->insert('end',$pgn->{headers}->{Result});
	$self->{pgn_time_control}->delete('1.0','end');
	$self->{pgn_time_control}->insert('end',$pgn->{headers}->{TimeControl});
	$self->{pgn_ply_count}->delete('1.0','end');
	$self->{pgn_ply_count}->insert('end',$pgn->{headers}->{PlyCount});
	$self->{pgn_dir}->delete('1.0','end');
	$self->{pgn_dir}->insert('end',$line);
	
	$self->{flip}=$pgn->{headers}->{Flip};
	
	$self->{mw}->configure(-title=>$line);
	
	$self->move_made;

	if($self->{settings}->{show_pgn})
	{
		$self->show_pgn($pgn->{full_pgn});
	}
	
}

sub set_from_pgn
{
	my $self=shift;
	
	my $pgn=$CLIP->Get;
	
	open(TEMP,">$self->{settings}->{pgn_dir}temp.pgn");
	print TEMP $pgn;
	close(TEMP);
	
	$self->do_load_pgn('temp.pgn');
}

sub show_pgn_clicked
{

	shift;
	my $self=shift;
	
	return if $self->{analysis_mode};
	
	my $extension=shift;
	
	my $line;
	my $coords;
	
	if($extension eq 'boards')
	{
	
		$coords=Utils::split_text_index($self->{show_pgn_boards}->index('current'));
		
		my $coord_y=$coords->{y};
		my $index=int(($coord_y-1)/11);
		
		$line=$self->{pgn_list}->{$self->{pgn_list_size}-1-$index};
		
		print Data::Dumper->Dump([$self->{pgn_list}],['$self->{pgn_list}']);
		
		$extension='pgn';
		
		print "boards $coord_y $index $line\n";
	
	}
	else
	{
	
		$coords=Utils::split_text_index($self->{show_pgn_text}->index('current'));
		
		$line=$self->{show_pgn_text}->get("$coords->{y}.0","$coords->{y}.120");
	
	}
	
	
	my $pgn_dir=$self->{settings}->{pgn_dir};
	if(!($pgn_dir=~/\/$/)){$pgn_dir.='/';}

	my $is_dir="$pgn_dir/$line";$is_dir=~s/\/\//\//g;
	
	if(opendir(NEWDIR,$is_dir))
	{
	
		if($line eq '.')
		{
		}
		elsif($line eq '..')
		{
			@pgn_dir=split /\//,$pgn_dir;
			if(@pgn_dir>1)
			{
				pop(@pgn_dir);
				$pgn_dir=join('/',@pgn_dir);
				if(!($pgn_dir=~/\/$/)){$pgn_dir.='/';}
				$self->{settings}->{pgn_dir}=$pgn_dir;
			}
		}
		else
		{
			$self->{settings}->{pgn_dir}=$is_dir;
		}
		
		$self->save_settings;
		
		$self->load_pgn($extension,1);
		
		return;
	}
	
	$self->{show_pgn}->destroy;
	
	if($extension eq 'pgn')
	{
		$self->do_load_pgn($line);
	}
	elsif( ($extension eq 'json.txt') || ($extension eq 'eval.txt') )
	{
	
		$self->{settings}->{book_name}=$line;
		$self->{settings}->{book_extension}=$extension;
		
		$self->{settings}->{book_dir}=$self->{settings}->{pgn_dir}.$line;
		
		$self->load_book;
	}

}

sub open_options
{
	my $self=shift;
	
	if(!Exists($self->{options_window}))
	{
		($self->{options_window}=$self->{mw}->Toplevel(-title=>'Options'))->geometry('+750+75');
		
		$self->{cutoff_frame}=$self->{options_window}->Frame()->pack;
		
		$self->{cutoff_frame}->Label(-text=>'Cutoff')->pack(-side=>'left');
		
		my @cutoff_options=map { $_*500 } 1..10;
		
		$self->{cutoff_optionmenu}=$self->{cutoff_frame}->Optionmenu(
			-options => \@cutoff_options,
			-command => sub { $self->save_settings; },
			-textvariable => \$self->{settings}->{cutoff}
		)->pack(%button_pack_options,-side=>'left');
	
	foreach(@{$self->{options}})
	{
		my $option=$_;
		
		$option->frame($self->{options_window})->pack;
	}
		
	}
	else
	{
		$self->{options_window}->raise;
	}
}

my $depth_percent;

########################################################################

sub new

########################################################################

{

	shift;
	
	my $mw=shift;

	my $self={
	mw=>$mw,
	book=>{},
	after_move_book=>{}
	};
	
	bless $self;
	
	#########################################
	
	mkdir('Data');
	
	mkdir('Pgn');
	my @dir=split /\//,getcwd;
	$self->{default_pgn_dir}=join('/',@dir).'/Pgn/';
	
	open(EMPTY,'>../Pgn/empty.eval.txt');
	print EMPTY Data::Dumper->Dump([{},{}],['$self->{book}','$self->{after_move_book}']);
	close(EMPTY);
	
	#########################################
	
	$self->load_settings;
	
	if(!exists($self->{settings}->{pgn_dir})){$self->{settings}->{pgn_dir}=$self->{default_pgn_dir};}
	if(!exists($self->{settings}->{book_dir})){$self->{settings}->{book_dir}=$self->{default_pgn_dir}.$self->{settings}->{book_name};}
	
	$self->save_settings;
	
	#########################################
	
		#########################################
		
		$self->{status_frame}=$mw->Frame(-padx=>5,-pady=>5,-borderwidth=>'4',-relief=>'raised')->grid(-row=>1,-column=>0,-columnspan=>4);
		
		$self->{status_line}=$self->{status_frame}->Text(-width=>80,-font=>[-size=>10],-height=>1,-insertontime=>0)->pack(-side=>'left');
		$self->{show_pgn_checkbox}=$self->{status_frame}->Checkbutton(-text => 'Shw', -onvalue => '1', -offvalue => '0',-command=>sub{$self->save_settings;},-variable => \$self->{settings}->{show_pgn})->pack(%button_pack_options,-side=>'left');
		$self->{update_status_bar_checkbox}=$self->{status_frame}->Checkbutton(-text => 'Upd', -onvalue => '1', -offvalue => '0',-command=>sub{ $self->save_settings; },-variable => \$self->{settings}->{update_status_bar})->pack(%button_pack_options,-side=>'left');
		
		$self->{status_frame}->Label(-text=>'Dump depth')->pack(-side=>'left');
		
		my @dump_depth_options=map { $_*5+10 } 0..4;
		
		$self->{dump_depth_optionmenu}=$self->{status_frame}->Optionmenu(
			-options => \@dump_depth_options,
			-command => sub { $self->save_settings; },
			-textvariable => \$self->{settings}->{dump_depth}
		)->pack(%button_pack_options,-side=>'left');
		
		$self->{status_frame}->Label(-text=>'Depth%')->pack(-side=>'left');
		
		my @depth_percent_options=map { $_*10+10 } 0..9;
		
		$self->{depth_percent_optionmenu}=$self->{status_frame}->Optionmenu(
			-options => \@depth_percent_options,
			-command => sub { $self->save_settings; },
			-textvariable => \$self->{settings}->{depth_percent}
		   )->pack(%button_pack_options,-side=>'left');
		   
		$self->{status_frame}->Label(-text=>'Max depth')->pack(-side=>'left');
		
		my @max_depth_options=map { $_ } 0..30;
		
		$self->{max_depth_optionmenu}=$self->{status_frame}->Optionmenu(
			-options => \@max_depth_options,
			-command => sub { $self->save_settings; },
			-textvariable => \$self->{settings}->{max_depth}
		)->pack(%button_pack_options,-side=>'left');
		
		$self->{status_frame}->Label(-text=>'Engine depth')->pack(-side=>'left');
		
		my @engine_depth_options=map { $_ } 3..10;
		
		$self->{max_depth_optionmenu}=$self->{status_frame}->Optionmenu(
			-options => \@engine_depth_options,
			-command => sub { $self->save_settings; },
			-textvariable => \$self->{settings}->{engine_depth}
		)->pack(%button_pack_options,-side=>'left');
		   
		$self->{status_frame}->Label(-text=>'Minimax after')->pack(-side=>'left');
		
		my @minimax_after_options=(1,map { $_*5 } 1..10);
		
		$self->{minimax_after_optionmenu}=$self->{status_frame}->Optionmenu(
			-options => \@minimax_after_options,
			-command => sub { $self->save_settings; },
			-textvariable => \$self->{settings}->{minimax_after}
		)->pack(%button_pack_options,-side=>'left');
		
		#################
		
		$self->{save_as_frame}=$mw->Frame(-padx=>5,-pady=>5,-borderwidth=>'4',-relief=>'raised')->grid(-row=>2,-column=>0,-columnspan=>4);
		
		$self->{save_as_frame}->Label(-text=>'Dir')->pack(-side=>'left',-padx=>5);
		
		$self->{save_as_dir}=$self->{save_as_frame}->Text(-width=>80,-font=>[-size=>12],-height=>1)->pack(-side=>'left');
		
		$self->{save_as_frame}->Label(-text=>'Name')->pack(-side=>'left',-padx=>5);
		
		$self->{save_as_name}=$self->{save_as_frame}->Text(-width=>20,-font=>[-size=>20],-height=>1)->pack(-side=>'left',-padx=>5);
		
		$self->{save_as_frame}->Label(-text=>'Type')->pack(-side=>'left');
		
		my @save_as_extension_options=('eval.txt','json.txt');
		
		$self->{dump_depth_optionmenu}=$self->{save_as_frame}->Optionmenu(
			-options => \@save_as_extension_options,
			-command => sub {  },
			-textvariable => \$self->{save_as_extension}
		)->pack(%button_pack_options,-side=>'left');
		
		$self->{save_as_frame}->Button(-text=>'Save as',%green_button_attr,-command=>[\&save_as,$self])->pack(-side=>'left');
		
		#################
		
		$self->{board_frame}=$mw->Frame(-padx=>5,-pady=>5,-borderwidth=>'4',-relief=>'raised')->grid(-row=>0,-column=>0);
		
		$self->{engine_frame}=$self->{board_frame}->Frame(-borderwidth=>'2',-relief=>'raised',-pady=>3)->pack();
		
		$self->{engine_line}=$self->{engine_frame}->Text(-width=>41,-font=>[-family=>'Courier',-size=>14,-weight=>'bold'],-height=>1,-insertontime=>0)->pack();
		$self->{engine_line}->tagConfigure('good',-foreground=>'#007f00');
		$self->{engine_line}->tagConfigure('neutral',-foreground=>'#0000ff');
		$self->{engine_line}->tagConfigure('bad',-foreground=>'#7f0000');
		
		$self->{engine_frame_inactive_background}=$self->{engine_frame}->cget(-background);
		
		$self->{engine_controls_frame}=$self->{engine_frame}->Frame(-borderwidth=>'2',-relief=>'raised',-pady=>3)->pack();
		
		$self->{engine_analysis_button}=$self->{engine_controls_frame}->Button(-text=>'Start analysis',-command=>[\&change_analysis,$self])->pack(%button_pack_options,-side=>'left');
		$self->{engine_minimax_button}=$self->{engine_controls_frame}->Button(-text=>'Minimax',-command=>[\&minimax_out_save,$self])->pack(%button_pack_options,-side=>'left');
		
		$self->{engine_on_checkbox}=$self->{engine_controls_frame}->Checkbutton(-text => 'Engine', -onvalue => '1', -offvalue => '0',-variable => \$self->{engine})->pack(%button_pack_options,-side=>'left');
		$self->{engine_go_button}=$self->{engine_controls_frame}->Button(-background=>'#afffaf',-activebackground=>'#afdfaf',-text=>'Go',-command=>[\&go,$self])->pack(%button_pack_options,-side=>'left');
		$self->{engine_stop_button}=$self->{engine_controls_frame}->Button(-background=>'#ffafaf',-activebackground=>'#dfafaf',-text=>'Stop',-command=>[\&stop,$self])->pack(%button_pack_options,-side=>'left');
		if($WINBY)
		{
			$self->{engine_i_button}=$self->{engine_controls_frame}->Button(-background=>'#afffaf',-activebackground=>'#afdfaf',-text=>'Inf',-command=>[\&inf,$self])->pack(%button_pack_options,-side=>'left');
			$self->{engine_q_button}=$self->{engine_controls_frame}->Button(-background=>'#ffafaf',-activebackground=>'#dfafaf',-text=>'QInf',-command=>[\&qinf,$self])->pack(%button_pack_options,-side=>'left');
		}
		$self->{engine_make_button}=$self->{engine_controls_frame}->Button(-background=>'#afafff',-activebackground=>'#afafdf',-text=>'Make',-command=>[\&make_engine_move,$self])->pack(%button_pack_options,-side=>'left');
		
		$self->{engine_controls_frame}->Button(-text=>'<',-command=>[\&del_move,$self])->pack(%button_pack_options,-side=>'left');
		$self->{engine_controls_frame}->Button(-text=>'>',-command=>[\&undel_move,$self])->pack(%button_pack_options,-side=>'left');
		
		$self->{canvas} = $self->{board_frame}->Canvas(
			-width  => $canvas_size,
			-height => $canvas_size,
			-borderwidth=>'4',
			-relief=>'raised'
		)->pack();
		
		$self->{canvas}->bind( 'draggable', '<1>'                   => [ \&drag_start , $self] );
		$self->{canvas}->bind( 'draggable', '<B1-Motion>'           => \&drag_during );
		$self->{canvas}->bind( 'draggable', '<Any-ButtonRelease-1>' => [ \&drag_end , $self ] );
		$self->{canvas}->bind( 'draggable', '<B1-Enter>' => undef );
		$self->{canvas}->bind( 'draggable', '<B1-Leave>' => undef );
		
		$self->{board_controls_frame}=$self->{board_frame}->Frame(-borderwidth=>'2',-relief=>'raised',-pady=>3)->pack();
		
		$self->{board_controls_frame}->Button(-text=>'Flip',-command=>[\&flip_board,$self])->pack(%button_pack_options,-side=>'left');
		$self->{board_controls_frame}->Button(-text=>'<<',-command=>[\&del_all_moves,$self])->pack(%button_pack_options,-side=>'left');
		$self->{board_controls_frame}->Button(-text=>'<',-command=>[\&del_move,$self])->pack(%button_pack_options,-side=>'left');
		$self->{board_controls_frame}->Button(-text=>'>',-command=>[\&undel_move,$self])->pack(%button_pack_options,-side=>'left');
		$self->{board_controls_frame}->Button(-text=>'>>',-command=>[\&undel_all_moves,$self])->pack(%button_pack_options,-side=>'left');
		$self->{board_controls_frame}->Checkbutton(-text => 'ShwL',-command=>sub{ $self->save_settings;$self->move_made; }, -onvalue => '1', -offvalue => '0',-variable => \$self->{settings}->{show_legal_moves})->pack(%button_pack_options,-side=>'left');
		$self->{board_controls_frame}->Button(-text=>'<- PGN',-command=>[\&set_from_pgn,$self])->pack(%button_pack_options,-side=>'left');
		$self->{board_controls_frame}->Button(-text=>'-> PGN',-command=>[\&copy_pgn,$self])->pack(%button_pack_options,-side=>'left');
		$self->{board_controls_frame}->Button(-text=>'<- FEN',-command=>[\&set_from_fen,$self])->pack(%button_pack_options,-side=>'left');
		$self->{board_controls_frame}->Button(-text=>'-> FEN',-command=>[\&copy_fen,$self])->pack(%button_pack_options,-side=>'left');
		$self->{board_controls_frame}->Button(-text=>'Reset',-command=>[\&reset_game,$self])->pack(%button_pack_options,-side=>'left');
		
		$self->{save_load_frame}=$mw->Frame(-borderwidth=>'2',-relief=>'raised',-padx=>3)->grid(-row=>0,-column=>1);
		
		$self->{pgn_frame}=$self->{save_load_frame}->Frame(-borderwidth=>'2',-relief=>'raised',-padx=>3,-pady=>3)->pack;
		
		$self->{pgn_frame}->Button(-text=>'Load PGN',-command=>[\&load_pgn,$self,'pgn'])->pack(-pady=>1);
		
		$self->{pgn_frame}->Label(-text=>'White')->pack(-pady=>1);
		$self->{pgn_white}=$self->{pgn_frame}->Text(%pgn_header_text_attr)->pack();
		$self->{pgn_frame}->Label(-text=>'White Elo')->pack(-pady=>1);
		$self->{pgn_white_elo}=$self->{pgn_frame}->Text(%pgn_header_text_attr)->pack();
		$self->{pgn_frame}->Label(-text=>'Black')->pack(-pady=>1);
		$self->{pgn_black}=$self->{pgn_frame}->Text(%pgn_header_text_attr)->pack();
		$self->{pgn_frame}->Label(-text=>'Black Elo')->pack(-pady=>1);
		$self->{pgn_black_elo}=$self->{pgn_frame}->Text(%pgn_header_text_attr)->pack();
		$self->{pgn_frame}->Label(-text=>'Date')->pack(-pady=>1);
		$self->{pgn_date}=$self->{pgn_frame}->Text(%pgn_header_text_attr)->pack();
		$self->{pgn_frame}->Label(-text=>'Result')->pack(-pady=>1);
		$self->{pgn_result}=$self->{pgn_frame}->Text(%pgn_header_text_attr)->pack();
		$self->{pgn_frame}->Label(-text=>'Time Control')->pack(-pady=>1);
		$self->{pgn_time_control}=$self->{pgn_frame}->Text(%pgn_header_text_attr)->pack();
		$self->{pgn_frame}->Label(-text=>'Ply Count')->pack(-pady=>1);
		$self->{pgn_ply_count}=$self->{pgn_frame}->Text(%pgn_header_text_attr)->pack();
		$self->{pgn_frame}->Label(-text=>'File')->pack(-pady=>1);
		$self->{pgn_dir}=$self->{pgn_frame}->Text(%pgn_header_text_attr)->pack();
		
		$self->{pgn_frame}->Button(-text=>"Save PGN",-command=>[\&copy_pgn,$self,1])->pack(-pady=>1);
		
		$self->{numbered_save_load_frame}=$self->{save_load_frame}->Frame(-borderwidth=>'2',-relief=>'raised',-padx=>3,-pady=>3)->pack;
		$self->{load_frame}=$self->{numbered_save_load_frame}->Frame(-borderwidth=>'2',-relief=>'raised',-padx=>3,-pady=>3)->pack(-side=>'left');
		$self->{save_frame}=$self->{numbered_save_load_frame}->Frame(-borderwidth=>'2',-relief=>'raised',-padx=>3,-pady=>3)->pack(-side=>'left');
		
		for(my $i=1;$i<=5;$i++)
		{
			$self->{"save_".$i."_button"}=$self->{save_frame}->Button(-text=>"Save $i",-command=>[\&save_i,$self,$i])->pack;
			$self->{"load_".$i."_button"}=$self->{load_frame}->Button(-text=>"Load $i",-command=>[\&load_i,$self,$i])->pack;
		}
		
		$self->{legal_moves_frame}=$mw->Frame(-borderwidth=>'2',-relief=>'raised',-padx=>3)->grid(-row=>0,-column=>2);
		
		$self->{legal_moves_text}=$self->{legal_moves_frame}->Scrolled('Text',-scrollbars=>'e',-width=>12,-height=>41,-insertontime=>0)->pack();
		
		$self->{book_frame}=$mw->Frame(-borderwidth=>'2',-relief=>'raised')->grid(-row=>0,-column=>3,-padx=>3);
		
		$self->{book_controls_frame}=$self->{book_frame}->Frame(-borderwidth=>'2',-relief=>'raised',-pady=>3)->pack();
		
		$self->{options_button}=$self->{book_controls_frame}->Button(-text=>'Options',-command=>[\&open_options,$self])->pack(%button_pack_options,-side=>'left');
		
		$self->{save_book_button}=$self->{book_controls_frame}->Button(-text=>'Save',%green_button_attr,-command=>[\&save_book,$self])->pack(%button_pack_options,-side=>'left');
	
		$self->{book_controls_frame}->Button(-text=>'<<',-command=>[\&del_all_moves,$self])->pack(%button_pack_options,-side=>'left');
		$self->{book_controls_frame}->Button(-text=>'<',-command=>[\&del_move,$self])->pack(%button_pack_options,-side=>'left');
		$self->{book_controls_frame}->Button(-text=>'>',-command=>[\&undel_move,$self])->pack(%button_pack_options,-side=>'left');
		$self->{book_controls_frame}->Button(-text=>'>>',-command=>[\&undel_all_moves,$self])->pack(%button_pack_options,-side=>'left');
		
		$self->{dump_book_button}=$self->{book_controls_frame}->Button(-text=>'Dump book',-command=>[\&dump_book,$self])->pack(%button_pack_options,-side=>'left');
		
		$self->{load_eval_book_button}=$self->{book_controls_frame}->Button(-text=>'Load eval',%yellow_button_attr,-command=>[\&load_pgn,$self,'eval.txt'])->pack(%button_pack_options,-side=>'left');
		$self->{load_json_book_button}=$self->{book_controls_frame}->Button(-text=>'Load json',%blue_button_attr,-command=>[\&load_pgn,$self,'json.txt'])->pack(%button_pack_options,-side=>'left');
		
		$self->{export_book_button}=$self->{book_controls_frame}->Button(-text=>'Export',-command=>[\&export_book,$self])->pack(%button_pack_options,-side=>'left');
		
		$self->{book_text}=$self->{book_frame}->Scrolled('Text',-font=>[-family=>'Courier',-size=>14],-scrollbars=>'e',-width=>50,-height=>18,-insertontime=>0)->pack(-pady=>5);
		$self->{book_text}->bind('<Button-1>',[\&book_clicked,$self]);
		
		$self->{game_frame}=$self->{book_frame}->Frame()->pack(-padx=>10,-pady=>5);
		
		$self->{game_text_move_width}=6;
		$self->{game_text_width}=13;
		
		$self->{game_text}=$self->{game_frame}->Scrolled('Text',-scrollbars=>'e',-padx=>5,-width=>$self->{game_text_width}*$self->{game_text_move_width},-height=>10,-insertontime=>0)->pack();
		$self->{game_text}->bind('<Button-1>',[\&game_clicked,$self]);
		
		$self->{game_text}->tagConfigure('undel',-foreground=>'#ff0000');
		
		foreach(@comments)
		{
			$self->{book_text}->tagConfigure($comments->{$_}->{name},%{$comments->{$_}->{tags}},-font=>[-family=>'Courier',-size=>14,-weight=>'bold']);
		}
		
		$self->{book_text}->tagConfigure('back',-background=>'#cfcfcf');
		$self->{book_text}->tagConfigure('small',-font=>[-family=>'Courier',-size=>12]);
		$self->{book_text}->tagConfigure('margin',-lmargin1=>15);
		
		#########################################
	
	$self->reset;
	
	$self->load_book;
	
	$self->reset_game;
	
	$self->{mw}->after(500,[\&update_engine,$self]);
	
	return $self;
	
}

########################################################################

sub ij_to_screen_ij
{
	my $self=shift;
	
	my $ij=shift;
	
	if($self->{flip})
	{
		return({i=>7-$ij->{i},j=>7-$ij->{j}});
	}
	else
	{
		return $ij;
	}
}

sub screen_ij_to_ij
{
	my $self=shift;
	
	my $screen_ij=shift;
	
	if($self->{flip})
	{
		return({i=>7-$screen_ij->{i},j=>7-$screen_ij->{j}});
	}
	else
	{
		return $screen_ij;
	}
}

sub draw_board
{

	my $self=shift;
	
	if($self->{dont_draw}){return;}
	
	$self->{canvas}->delete('all');
	
	my @rep=split //,$self->{board}->board_rep;
	
	for(my $i=0;$i<8;$i++)
	{
		for(my $j=0;$j<8;$j++)
		{
			my $ij={i=>$i,j=>$j};
			
			my $screen_ij=$self->ij_to_screen_ij($ij);
			
			$self->{canvas}->createText(
			$screen_ij->{i}*$square_size+$margin+$square_size/2,$screen_ij->{j}*$square_size+$square_size/2+$margin,
			-text => $rep[$j*8+$i],
			-font => [ -family => 'Chess Merida', -size => $piece_size ],
			-tags       => ['draggable']
			);
		}
	}
	
	my $left=join(' ',map { sprintf "%-5s",$_->{full_algeb} } @{$self->{moves}});
	my $all=join(' ',(map { sprintf "%-5s",$_->{full_algeb} } @{$self->{moves}}),(reverse(map { sprintf "%-5s",$_->{full_algeb} } @{$self->{unmoves}})));
	my $x1=length($left);
	$self->{game_text}->delete('1.0','end');
	$self->{game_text}->insert('end',$all);
	$self->{game_text}->tagAdd('undel',"1.$x1",'end');
	
}

sub annotate_cpp
{
	$self=shift;
	my $cnt;
	my $cpp;
	foreach(keys(%{$self->{book}}))
	{
		my $fen=$_;
		foreach(keys(%{$self->{book}->{$fen}}))
		{
			my $algeb=$_;
			my $annot=$self->{book}->{$fen}->{$algeb};
			if(($annot ne '...')&&($annot ne ''))
			{
				$cnt++;
				my $id="fen$cnt";
				my $decl="fen$cnt".'[]';
				$cpp.=qq(
char $decl="$fen";
dummy.set_from_fen($id);
annotate_move(&dummy,"$algeb","$annot");
);
			}
		}
	}
	print "cnt $cnt\n";
	$CLIP->Set($cpp);
}

1;
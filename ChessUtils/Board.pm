###################################################################

package Board;

###################################################################

# global vars

$variant="standard";

%color_of_piece		=	qw(p -1 n -1 b -1 r -1 q -1 k -1 P 1 N 1 B 1 R 1 Q 1 K 1);
%type_of_piece		=	qw(p P n N b B r R q Q k K P P N N B B R R Q Q K K);
%white_of_piece		=	%type_of_piece;
%algeb_of_piece		=	qw(p p n n b b r r q q k k P p N n B b R r Q q K k);
%black_of_piece		=	%algeb_of_piece;
%inverse_of_piece	=	qw(p P P p n N N n b B B b r R R r q Q Q q k K K k);

%vector_piece=qw(Q 1 R 1 B 1);

$start_rep='rnbqkbnrpppppppp                                PPPPPPPPRNBQKBNR';

$check_test_pieces=
{
	1	=>	'KNBRQP',
	-1	=>	'knbrqp'
};

$piece_vectors=
{
	Q=>[[0,1],[0,-1],[1,0],[-1,0],[1,1],[-1,1],[1,-1],[-1,-1]],
	K=>[[0,1],[0,-1],[1,0],[-1,0],[1,1],[-1,1],[1,-1],[-1,-1]],
	R=>[[0,1],[0,-1],[1,0],[-1,0]],
	B=>[[1,1],[-1,1],[1,-1],[-1,-1]],
	N=>[[1,2],[1,-2],[-1,2],[-1,-2],[2,1],[2,-1],[-2,1],[-2,-1]]
};

# global subs

sub pos_to_ij
{

	my $pos=shift;
	
	if(($pos<0)||($pos>63))
	{
		# out of board
		
		return(NULL);
	}
	
	my $i=$pos%8;
	my $j=($pos-$i)/8;
	
	my $ij={i=>$i,j=>$j};
	
	return($ij);
	
}

sub ij_to_pos
{

	my $i=shift;
	my $j=shift;
	
	if(($i<0)||($i>7)||($j<0)||($j>7))
	{
		# out of board
		
		return(-1);
	}
	
	my $pos=$j*8+$i;
	
	return($pos);
}

sub pos_to_algeb
{
	
	my $pos=shift;
	
	my $ij=pos_to_ij($pos);
	
	if($ij)
	{
		my $algeb={file=>chr($ij->{i}+ord('a')),rank=>chr(7-$ij->{j}+ord('1'))};
		
		$algeb->{algeb}=$algeb->{file}.$algeb->{rank};
		
		return $algeb;
	}
	else
	{
		return NULL;
	}
	
}

sub algeb_to_pos
{
	
	my $algeb=shift;
	
	$algeb=~/^(.)(.)/;
	
	my $i=ord($1)-ord('a');
	my $j=7-(ord($2)-ord('1'));
	
	return(ij_to_pos($i,$j));
	
}

# object subs

sub board_rep
{
	my $self=shift;
	
	my @fen=split //,$self->{rep};
	
	my $col=0;
	for(my $i=0;$i<@fen;$i++)
	{
	
		if(($i%8)!=0){$col=1-$col;}
		if($col==0)
		{
		
			$fen[$i]=~tr/ PNBRQKpnbrqk/ pnbrqkomvtwl/;
		
		}
		else
		{
		
			$fen[$i]=~tr/ PNBRQKpnbrqk/+PNBRQKOMVTWL/;
		
		}
	
	}
		
	return join('',@fen);
}

sub report_fen
{

	my $self=shift;
	
	my $no_counts=shift;

	my $fen=join('/',map { $_=~s/( +)/length $1/ge; $_; } $self->{rep}=~/.{8}/g );
	
	$fen.=' '.($self->{turn}==1?'w':'b');
	
	$fen.=$self->{castling_rights} eq ''?' -':" $self->{castling_rights}";
	
	$fen.=' '.($self->{ep_pos} eq ''?'-':(pos_to_algeb($self->{ep_pos})->{algeb}));
	
	if($no_counts){return $fen;}
	
	$fen.=" $self->{half_move_clock}";
	
	$fen.=" $self->{full_move_number}";
	
	return $fen;
	
}

sub set_from_fen
{

	my $self=shift;
	
	my $fen=shift;
	
	my ($rep,$turn,$castling_rights,$ep_square,$half_move_clock,$full_move_number)=split / /,$fen;
	
	$rep=~s/([0-9]+)/' ' x $1/eg;
	$rep=~s/\///g;
	$turn=$turn eq 'w'?1:-1;
	$castling_rights=~s/\-//;
	$ep_square=~s/\-//;
	$half_move_clock=$half_move_clock eq ''?'0':$half_move_clock;
	$full_move_number=$full_move_number eq ''?'1':$full_move_number;
	
	my $ep_pos;
	
	if($ep_square ne '')
	{
		$ep_pos=algeb_to_pos($ep_square);
	}
	
	$self->{rep}=$rep;
	$self->{turn}=$turn;
	$self->{castling_rights}=$castling_rights;
	$self->{ep_pos}=$ep_pos;
	
}

sub reset
{

	my $self=shift;
	
	$self->{rep}=$start_rep;
	
	$self->{castling_rights}='KQkq';
	$self->{half_move_clock}=0;
	$self->{full_move_number}=1;
	$self->{ep_pos}='';
	
	$self->{turn}=1;
	
}

sub clone
{

	my $self=shift;
	
	my $clone=new Board;
	
	$clone->{rep}=$self->{rep};
	
	$clone->{castling_rights}=$self->{castling_rights};
	$clone->{half_move_clock}=$self->{half_move_clock};
	$clone->{full_move_number}=$self->{full_move_number};
	$clone->{ep_pos}=$self->{ep_pos};
	
	$clone->{turn}=$self->{turn};
	
	return $clone;
	
}

sub copy
{

	my $self=shift;
	
	my $copy=shift;
	
	$self->{rep}=$copy->{rep};
	
	$self->{castling_rights}=$copy->{castling_rights};
	$self->{half_move_clock}=$copy->{half_move_clock};
	$self->{full_move_number}=$copy->{full_move_number};
	$self->{ep_pos}=$copy->{ep_pos};
	
	$self->{turn}=$copy->{turn};
	
}

#############################################################

sub new

#############################################################

{

	my $self={
	};
	
	bless $self;
	
	$self->reset;
	
	return $self;
	
}

sub print_as_string
{

	$self=shift;
	
	my $print=join("\n",$self->{rep}=~/.{8}/g)."\n--------\n";
	$print.="half move clock: $self->{half_move_clock}, full move number: $self->{full_move_number}\n";
	$print.="e.p. square: ---$self->{ep_pos}---, castling rights: ---$self->{castling_rights}---";
	
	return($print);
	
}

sub print
{

	$self=shift;
	
	print "------------------------\n",$self->print_as_string,"\n------------------------\n";
	
}

sub get_piece_at_pos
{

	my $self=shift;
	
	my $pos=shift;
	
	my $piece=substr($self->{rep},$pos,1);
	
	return($piece);
	
}

sub set_piece_at_pos
{

	my $self=shift;
	
	my $pos=shift;
	
	my $piece=shift;
	
	substr($self->{rep},$pos,1,$piece);
	
}

sub legal_targets_at_pos_for_piece
{

	my $self=shift;
	
	my $pos=shift;
	
	my $piece=shift;
	
	my $ij=pos_to_ij($pos);
	
	my $i0=$ij->{i};
	my $j0=$ij->{j};
	
	my $PIECE=$type_of_piece{$piece};
	my $color=$color_of_piece{$piece};
	
	my $target_pos_list=[];
	
	if($piece eq 'p')
	{
		if($j0==1)
		{
			if(($self->get_piece_at_pos($pos+8) eq ' ')&&($self->get_piece_at_pos($pos+16) eq ' '))
			{
				push(@{$target_pos_list},{pos=>$pos+16,ep_pos=>$pos+8});
			}
		}
		
		if($self->get_piece_at_pos($pos+8) eq ' ')
		{
			if($j0==6)
			{
				push(@{$target_pos_list},{pos=>$pos+8,prom=>'n'});
				push(@{$target_pos_list},{pos=>$pos+8,prom=>'b'});
				push(@{$target_pos_list},{pos=>$pos+8,prom=>'r'});
				push(@{$target_pos_list},{pos=>$pos+8,prom=>'q'});
			}
			else
			{
				push(@{$target_pos_list},{pos=>$pos+8});
			}
		}
		
		if(($i0>0)&&(($color_of_piece{$self->get_piece_at_pos($pos+7)}==1)||($self->{ep_pos} eq ($pos+7))))
		{
			push(@{$target_pos_list},{pos=>$pos+7});
		}
		
		if(($i0<7)&&(($color_of_piece{$self->get_piece_at_pos($pos+9)}==1)||($self->{ep_pos} eq ($pos+9))))
		{
			push(@{$target_pos_list},{pos=>$pos+9});
		}
	}
	elsif($piece eq 'P')
	{
		if($j0==6)
		{
			if(($self->get_piece_at_pos($pos-8) eq ' ')&&($self->get_piece_at_pos($pos-16) eq ' '))
			{
				push(@{$target_pos_list},{pos=>$pos-16,ep_pos=>$pos-8});
			}
		}
		
		if($self->get_piece_at_pos($pos-8) eq ' ')
		{
			if($j0==1)
			{
				push(@{$target_pos_list},{pos=>$pos-8,prom=>'N'});
				push(@{$target_pos_list},{pos=>$pos-8,prom=>'B'});
				push(@{$target_pos_list},{pos=>$pos-8,prom=>'R'});
				push(@{$target_pos_list},{pos=>$pos-8,prom=>'Q'});
			}
			else
			{
				push(@{$target_pos_list},{pos=>$pos-8});
			}
		}
		
		if(($i0>0)&&(($color_of_piece{$self->get_piece_at_pos($pos-9)}==-1)||($self->{ep_pos} eq ($pos-9))))
		{
			push(@{$target_pos_list},{pos=>$pos-9});
		}
		
		if(($i0<7)&&(($color_of_piece{$self->get_piece_at_pos($pos-7)}==-1)||($self->{ep_pos} eq ($pos-7))))
		{
			push(@{$target_pos_list},{pos=>$pos-7});
		}
	}
	else
	{
		my $vectors=$piece_vectors->{$PIECE};
		
		foreach(@{$vectors})
		{
			my $di=$_->[0];
			my $dj=$_->[1];
			
			for(my $step=1;$step<=($vector_piece{$PIECE}?7:1);$step++)
			{
				
				my $vector_i=$i0+$step*$di;
				my $vector_j=$j0+$step*$dj;
				my $vector_pos=ij_to_pos($vector_i,$vector_j);
				if($vector_pos<0){goto vector_finished;}
				my $piece_at_vector_pos=$self->get_piece_at_pos($vector_pos);
				if($color_of_piece{$piece_at_vector_pos}==$color){goto vector_finished;}
				
				push(@{$target_pos_list},{pos=>$vector_pos});
				
				if($color_of_piece{$piece_at_vector_pos}==-$color){goto vector_finished;}
			}
			
			vector_finished:
		}
	}

	if(($piece eq 'k')&&($pos==4))
	{
		if((substr($self->{rep},5,2) eq '  ')&&($self->{castling_rights}=~/k/))
		{
			push(@{$target_pos_list},{pos=>6,castles=>'O-O'});
		}
		
		if((substr($self->{rep},1,3) eq '   ')&&($self->{castling_rights}=~/q/))
		{
			push(@{$target_pos_list},{pos=>2,castles=>'O-O-O'});
		}
	}
	
	if(($piece eq 'K')&&($pos==60))
	{
		if((substr($self->{rep},61,2) eq '  ')&&($self->{castling_rights}=~/K/))
		{
			push(@{$target_pos_list},{pos=>62,castles=>'O-O'});
		}
		
		if((substr($self->{rep},57,3) eq '   ')&&($self->{castling_rights}=~/Q/))
		{
			push(@{$target_pos_list},{pos=>58,castles=>'O-O-O'});
		}
	}
	
	return($target_pos_list);
	
}

sub legal_moves
{

	my $self=shift;
	
	my $limit_to=shift;
	
	my $legal_moves=[];
	
	for(my $pos=0;$pos<64;$pos++)
	{
		my $piece=$self->get_piece_at_pos($pos);
		
		my $go=1;
		
		if($limit_to ne '')
		{
			if($piece ne $limit_to)
			{
				$go=0;
			}
		}
		
		if( ($color_of_piece{$piece}==$self->{turn}) && $go )
		{
			my @target_pos_list=@{$self->legal_targets_at_pos_for_piece($pos,$piece)};
			
			my $orig_algeb_data=pos_to_algeb($pos);
			
			my $orig_algeb=$orig_algeb_data->{algeb};
			my $orig_algeb_file=$orig_algeb_data->{file};
			my $orig_algeb_rank=$orig_algeb_data->{rank};
		
			foreach(@target_pos_list)
			{
				my $target_pos=$_;
				my $dest_pos=$target_pos->{pos};
				
				my $dest_piece=$self->get_piece_at_pos($dest_pos);
				
				my $dest_algeb=pos_to_algeb($dest_pos)->{algeb};
				
				my $prom=$target_pos->{prom};
				my $prom_type=$type_of_piece{$prom};
				my $prom_algeb=$algeb_of_piece{$prom};
				
				my $castles=$target_pos->{castles};
				
				my $capture;
				my $ep=( ($dest_pos eq $self->{ep_pos}) && ($type_of_piece{$piece} eq 'P') );
				
				if( ($dest_piece ne ' ') || ($ep) )
				{
					$capture=1;
				}
				
				my $pgn_piece;
				my $piece_type=$type_of_piece{$piece};
				
				$orig_algeb=~/^(.)(.)/;
				
				my $orig_file=$1;
				my $orig_rank=$2;
				
				if($piece_type eq 'P')
				{
					if($capture)
					{
						$pgn_piece=$orig_file;
					}
				}
				else
				{
					$pgn_piece=$piece_type;
				}
				
				my $pgn_takes=$capture?'x':'';
				
				my $pgn_algeb=$dest_algeb;
				
				my $pgn_ep=$ep?' e.p.':'';
				
					my $pgn_ep='';
				
				my $pgn_prom=$prom eq ''?'':"=$prom_type";
				
				my $pgn_move="$pgn_piece$pgn_takes$pgn_algeb$pgn_ep$pgn_prom";
				my $pgn_rank_move="$pgn_piece$orig_algeb_rank$pgn_takes$pgn_algeb$pgn_ep$pgn_prom";
				my $pgn_file_move="$pgn_piece$orig_algeb_file$pgn_takes$pgn_algeb$pgn_ep$pgn_prom";
				my $pgn_full_move="$pgn_piece$orig_algeb$pgn_takes$pgn_algeb$pgn_ep$pgn_prom";
				
				if($castles ne ''){$pgn_move=$castles;}
				
				push(@{$legal_moves},
				{
					orig_algeb=>$orig_algeb,
					orig_pos=>$pos,
					dest_algeb=>$dest_algeb,
					dest_pos=>$dest_pos,
					ep_pos=>$target_pos->{ep_pos},
					prom=>$prom,
					castles=>$castles,
					prom_algeb=>$prom_algeb,
					full_algeb=>"$orig_algeb$dest_algeb$prom_algeb",
					capture=>$capture,
					ep=>$ep,
					pgn_piece=>$pgn_piece,
					pgn_takes=>$pgn_takes,
					pgn_algeb=>$pgn_algeb,
					pgn_ep=>$pgn_ep,
					pgn_prom=>$pgn_prom,
					pgn_move=>$pgn_move,
					pgn_file_move=>$pgn_file_move,
					pgn_rank_move=>$pgn_rank_move,
					pgn_full_move=>$pgn_full_move,
					piece=>$piece
				}
				);
			}
		}
	}
	
	return($legal_moves);
	
}

sub legal_algeb_moves
{

	my $self=shift;
	
	my $limit_to=shift;
	
	my $legal_moves=$self->legal_moves($limit_to);
	
	my $legal_algeb_moves={};
	
	my $pgn_moves={};
	
	foreach(@{$legal_moves})
	{
		my $move=$_;
		
		my $clone=$self->clone;
		
		if($clone->make_move_legal({legal=>1,move=>$move}))
		{
			
			if(!$self->{dont_check_pgn})
			{
			
				my $check;
				my $mate;
				my $exploded;
				my $stalemate;
				
				if($clone->is_in_check(-$self->{turn}))
				{
					$check=1;
				}
				
				if($clone->is_exploded(-$self->{turn}))
				{
					$exploded=1;
				}
				
				if(!$exploded)
				{
				
					my $any_legal;
				
					if(0)
					{
						my @opp_moves=@{$clone->legal_moves};
						
						foreach(@opp_moves)
						{
							my $lmove=$_;
							my $clone2=$clone->clone;
							if($clone2->make_move_legal({legal=>1,move=>$lmove}))
								{
									$any_legal=1;goto legality_check_done;
								}
						}
						
						legality_check_done:
					
					}
					
					$any_legal=$clone->has_legal;
					
					if(!$any_legal)
					{
						if($check)
						{
							$mate=1;
						}
						else
						{
							$stalemate=1;
						}
					}
					
					if($check&&!$mate)
					{
						$move->{pgn_check}='+';
					}
					elsif($mate)
					{
						$move->{pgn_check}='#';
					}
					elsif($stalemate)
					{
						$move->{pgn_check}='=';
					}
					
				}
				else
				{
					$move->{pgn_check}='';
				}
				
				$move->{pgn_move}.=$move->{pgn_check};
				$move->{pgn_file_move}.=$move->{pgn_check};
				$move->{pgn_rank_move}.=$move->{pgn_check};
				
			}
			
			$legal_algeb_moves->{$move->{full_algeb}}=
			{
				legal=>1,
				move=>$move
			};
			
			push(@{$pgn_moves->{$move->{pgn_move}}},$move);
		
		}
	}
	
	if(!$self->{dont_check_pgn})
	{
	
		foreach(keys(%{$pgn_moves}))
		{
			my $pgn_move=$_;
			my @pgn_moves=@{$pgn_moves->{$pgn_move}};
			
			if(@pgn_moves>1)
			{
			
				my $file_ok=1;
				my $file_hash={};
				my $rank_ok=1;
				my $rank_hash={};
				
				foreach(@pgn_moves)
				{
					my $full_algeb=$_->{full_algeb};
					my $move=$legal_algeb_moves->{$full_algeb}->{move};
					
					$file_hash->{$move->{pgn_file_move}}++;
					if($file_hash->{$move->{pgn_file_move}}>1)
					{
						$file_ok=0;
					}
					$rank_hash->{$move->{pgn_rank_move}}++;
					if($rank_hash->{$move->{pgn_rank_move}}>1)
					{
						$rank_ok=0;
					}
				}
				
				foreach(@pgn_moves)
				{
					my $full_algeb=$_->{full_algeb};
					my $move=$legal_algeb_moves->{$full_algeb}->{move};
					
					if($file_ok)
					{
						$legal_algeb_moves->{$full_algeb}->{move}->{pgn_move}=$move->{pgn_file_move};
					}
					elsif($rank_ok)
					{
						$legal_algeb_moves->{$full_algeb}->{move}->{pgn_move}=$move->{pgn_rank_move};
					}
					else
					{
						$legal_algeb_moves->{$full_algeb}->{move}->{pgn_move}=$move->{pgn_full_move};
					}
				}
				
			}
		}
		
	}
	
	return($legal_algeb_moves);
	
}

##################################################################
##################################################################

sub has_legal
{

	my $self=shift;
	
	my $legal_moves=[];
	
	for(my $pos=0;$pos<64;$pos++)
	{
		my $piece=$self->get_piece_at_pos($pos);
		
		if($color_of_piece{$piece}==$self->{turn})
		{
			my @target_pos_list=@{$self->legal_targets_at_pos_for_piece($pos,$piece)};
			
			my $orig_algeb_data=pos_to_algeb($pos);
			
			my $orig_algeb=$orig_algeb_data->{algeb};
			my $orig_algeb_file=$orig_algeb_data->{file};
			my $orig_algeb_rank=$orig_algeb_data->{rank};
		
			foreach(@target_pos_list)
			{
				my $target_pos=$_;
				my $dest_pos=$target_pos->{pos};
				
				my $dest_piece=$self->get_piece_at_pos($dest_pos);
				
				my $dest_algeb=pos_to_algeb($dest_pos)->{algeb};
				
				my $prom=$target_pos->{prom};
				my $prom_type=$type_of_piece{$prom};
				my $prom_algeb=$algeb_of_piece{$prom};
				
				my $castles=$target_pos->{castles};
				
				my $capture;
				my $ep=( ($dest_pos eq $self->{ep_pos}) && ($type_of_piece{$piece} eq 'P') );
				
				if( ($dest_piece ne ' ') || ($ep) )
				{
					$capture=1;
				}
				
				my $pgn_piece;
				my $piece_type=$type_of_piece{$piece};
				
				$orig_algeb=~/^(.)(.)/;
				
				my $orig_file=$1;
				my $orig_rank=$2;
				
				if($piece_type eq 'P')
				{
					if($capture)
					{
						$pgn_piece=$orig_file;
					}
				}
				else
				{
					$pgn_piece=$piece_type;
				}
				
				my $pgn_takes=$capture?'x':'';
				
				my $pgn_algeb=$dest_algeb;
				
				my $pgn_ep=$ep?' e.p.':'';
				
					my $pgn_ep='';
				
				my $pgn_prom=$prom eq ''?'':"=$prom_type";
				
				my $pgn_move="$pgn_piece$pgn_takes$pgn_algeb$pgn_ep$pgn_prom";
				my $pgn_rank_move="$pgn_piece$orig_algeb_rank$pgn_takes$pgn_algeb$pgn_ep$pgn_prom";
				my $pgn_file_move="$pgn_piece$orig_algeb_file$pgn_takes$pgn_algeb$pgn_ep$pgn_prom";
				my $pgn_full_move="$pgn_piece$orig_algeb$pgn_takes$pgn_algeb$pgn_ep$pgn_prom";
				
				if($castles ne ''){$pgn_move=$castles;}
				
				my $legal_move=
				{
					orig_algeb=>$orig_algeb,
					orig_pos=>$pos,
					dest_algeb=>$dest_algeb,
					dest_pos=>$dest_pos,
					ep_pos=>$target_pos->{ep_pos},
					prom=>$prom,
					castles=>$castles,
					prom_algeb=>$prom_algeb,
					full_algeb=>"$orig_algeb$dest_algeb$prom_algeb",
					capture=>$capture,
					ep=>$ep,
					pgn_piece=>$pgn_piece,
					pgn_takes=>$pgn_takes,
					pgn_algeb=>$pgn_algeb,
					pgn_ep=>$pgn_ep,
					pgn_prom=>$pgn_prom,
					pgn_move=>$pgn_move,
					pgn_file_move=>$pgn_file_move,
					pgn_rank_move=>$pgn_rank_move,
					pgn_full_move=>$pgn_full_move,
					piece=>$piece
				};
				
				my $clone=$self->clone;
				
				if($clone->make_move_legal({legal=>1,move=>$legal_move}))
				{
					$self->{has_legal_algeb}=$legal_move->{full_algeb};
					return 1;
				}
				
			}
		}
	}
	
	return 0;
	
}

##################################################################
##################################################################


sub make_move
{

	my $self=shift;
	my $move=shift;
	
	my $orig_pos=$move->{orig_pos};
	my $dest_pos=$move->{dest_pos};
	my $orig_piece=$move->{piece};
	my $dest_piece=$self->get_piece_at_pos($dest_pos);
	
	$self->set_piece_at_pos($orig_pos,' ');
	
	my $isep=(($dest_pos eq $self->{ep_pos})&&($type_of_piece{$orig_piece} eq 'P'));
	
	if(($dest_piece eq ' ')&&(!$isep))
	{
		if($move->{prom} ne '')
		{
			$self->set_piece_at_pos($dest_pos,$move->{prom});
		}
		else
		{
			$self->set_piece_at_pos($dest_pos,$orig_piece);
		}
		
	}
	else
	{
	
		if($variant eq 'atomic')
		{
			$self->set_piece_at_pos($dest_pos,' ');
		}
		
		if($variant eq 'standard')
		{
			$self->set_piece_at_pos($dest_pos,$orig_piece);
		}
		
		if($isep)
		{
			if($orig_piece eq 'p')
			{
				$self->set_piece_at_pos($dest_pos-8,' ');
			}
			else
			{
				$self->set_piece_at_pos($dest_pos+8,' ');
			}
		}
	
		if($variant eq 'atomic')
		{
			my $ij=pos_to_ij($dest_pos);
			for(my $di=-1;$di<=1;$di++)
			{
				for(my $dj=-1;$dj<=1;$dj++)
				{
					my $dpos=ij_to_pos($ij->{i}+$di,$ij->{j}+$dj);
					if($dpos>=0)
					{
						my $dpiece=$self->get_piece_at_pos($dpos);
						if($type_of_piece{$dpiece} ne 'P')
						{
							$self->set_piece_at_pos($dpos,' ');
						}
					}
				}
			}
		}
	}
	
	$self->{ep_pos}=$move->{ep_pos};
	
	$self->{turn}=-$self->{turn};
	
	if($orig_piece eq 'k')
		{
			$self->{castling_rights}=~s/[qk]//g;
			
			if($move->{castles} eq 'O-O')
			{
				$self->set_piece_at_pos(7,' ');
				$self->set_piece_at_pos(5,'r');
			}
			
			if($move->{castles} eq 'O-O-O')
			{
				$self->set_piece_at_pos(0,' ');
				$self->set_piece_at_pos(3,'r');
			}
		}
		
	if($orig_piece eq 'r')
		{
			if($orig_pos==0)
			{
				$self->{castling_rights}=~s/[q]//g;
			}
			
			if($orig_pos==7)
			{
				$self->{castling_rights}=~s/[k]//g;
			}
		}
		
	if($orig_piece eq 'K')
		{
			$self->{castling_rights}=~s/[QK]//g;
			
			if($move->{castles} eq 'O-O')
			{
				$self->set_piece_at_pos(63,' ');
				$self->set_piece_at_pos(61,'R');
			}
			
			if($move->{castles} eq 'O-O-O')
			{
				$self->set_piece_at_pos(56,' ');
				$self->set_piece_at_pos(59,'R');
			}
		}
		
	if($orig_piece eq 'R')
		{
			if($orig_pos==56)
			{
				$self->{castling_rights}=~s/[Q]//g;
			}
			
			if($orig_pos==63)
			{
				$self->{castling_rights}=~s/[K]//g;
			}
		}
		
	if(substr($self->{rep},0,1) eq ' ')
	{
		$self->{castling_rights}=~s/[q]//g;
	}
	
	if(substr($self->{rep},7,1) eq ' ')
	{
		$self->{castling_rights}=~s/[k]//g;
	}
	
	if(substr($self->{rep},56,1) eq ' ')
	{
		$self->{castling_rights}=~s/[Q]//g;
	}
	
	if(substr($self->{rep},63,1) eq ' ')
	{
		$self->{castling_rights}=~s/[K]//g;
	}
	
}

sub is_adjacent
{

	my $self=shift;
	
	my $color=shift;
	
	my $pos_king=shift;
	
	my $test_king=$color==1?'K':'k';
	
	my $ij_king=pos_to_ij($pos_king);
	
	for(my $i=-1;$i<=1;$i++)
	{
		for(my $j=-1;$j<=1;$j++)
		{
		
			if(!(($i==0)&&($j==0)))
			{
			
			
				my $test_pos=ij_to_pos($ij_king->{i}+$i,$ij_king->{j}+$j);
				
				if($test_pos>=0)
				{
				
					my $test_piece=substr($self->{rep},$test_pos,1);
					
					if($test_piece eq $test_king)
					{
					
						return 1;
					
					}
					
				}
				
			}
			
		}
	
	}
	
	return 0;

}

sub is_in_check
{	

	my $self=shift;
	
	my $color=shift;
	
	my $pos_king=shift;
	
	if($self->is_adjacent($color,$pos_king))
	{
		return 0;
	}
	
	my @check_test_pieces=split //,$check_test_pieces->{$color};
	
	my $king=shift(@check_test_pieces);
	
	$pos_king=$pos_king ne ''?$pos_king:index($self->{rep},$king);
	
	foreach(@check_test_pieces)
	{
		
		my $test_piece=$_;
		
		my $inv_test_piece=$inverse_of_piece{$test_piece};
		
		my @legal_moves=@{$self->legal_targets_at_pos_for_piece($pos_king,$test_piece)};
		
		foreach(@legal_moves)
		{
			my $legal_move=$_;
			my $dest_pos=$legal_move->{pos};
			my $dest_piece=$self->get_piece_at_pos($dest_pos);
			if($dest_piece eq $inv_test_piece){goto check;}
		}
		
	}
	
	return 0;
	
	check:
	
	return 1;
	
}

sub is_exploded
{

	my $self=shift;
	
	my $turn=shift;
	
	my $pos_king=index($self->{rep},$turn==1?'K':'k');
	
	return($pos_king<0);
	
}

sub make_move_legal
{

	my $self=shift;
	
	my $legal_move=shift;
	
	if($legal_move->{legal})
	{
		
		my $move=$legal_move->{move};
	
		if
			(
				(
					($move->{castles} eq 'O-O')
					&&
					( 
						( ($move->{piece} eq 'k') && ( ($self->is_in_check(-1,4)) || ($self->is_in_check(-1,5)) ) )
						||
						( ($move->{piece} eq 'K') && ( ($self->is_in_check(1,60)) || ($self->is_in_check(1,61)) ) )
					)
				)
				||
				(
					($move->{castles} eq 'O-O-O')
					&&
					( 
						( ($move->{piece} eq 'k') && ( ($self->is_in_check(-1,4)) || ($self->is_in_check(-1,3)) ) )
						||
						( ($move->{piece} eq 'K') && ( ($self->is_in_check(1,60)) || ($self->is_in_check(1,59)) ) )
					)
				)
			)
			
			{
				return(0);
			}
	
		my $clone=$self->clone;
		
		$clone->make_move($move);
		
		if(
		($clone->is_in_check($self->{turn}) && (!($clone->is_exploded(-$self->{turn}))) )
		||
		($clone->is_exploded($self->{turn}))
		)
		{
			return(0);
		}
		else
		{
			$self->copy($clone);
		}
	}
	else
	{
		return(0);
	}
	
	return(1);
	
}

sub make_algeb_move
{

	my $self=shift;
	
	my $algeb=shift;
	
	my $legal_algeb_moves=$self->legal_algeb_moves;
	
	my $legal_move=$legal_algeb_moves->{$algeb};
	
	return($self->make_move_legal($legal_move));
	
}

1;
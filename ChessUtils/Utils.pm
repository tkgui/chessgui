###################################################################

package Utils;

###################################################################

# general utility routines

sub signed
{

	my $what=shift;
	
	my $what=$what+0;
	
	return $what if $what<=0;
	
	return "+$what";

}

sub my_reverse_lines
{

	my $what=shift;
	
	my @what=split /\n/,$what;
	
	@what=reverse(@what);
	
	$what=join("\n",@what);

}

my %months=qw(1 January 2 February 3 March 4 April 5 May 6 June 7 July 8 August 9 September 10 October 11 November 12 December);
sub normalize_date
{

	my $date=shift;
	
	if($date=~/([0-9]{4})\.([0-9]+)\.([0-9]+)/)
	{
	
		my $year=$1;
		my $month=$2+0;
		my $day=$3;
		
		return "$months{$month} $day. - $year";
	
	}
	
	return $date;

}

sub tolower
{

	my $what=shift;
	
	$what=~tr/[A-Z]/[a-z]/;
	
	return $what;

}

# pad or cut string to certain length and align it

sub limit
{

	my $what=shift;
	my $limit=shift;
	my $align=shift;
	
	if($align eq 'c')
	{
		my $pad=int(($limit-length($what))/2);
		$what=(' ' x $pad).$what;
		$align='';
		
	}
	
	if(length($what)>$limit)
		{
		
			$what=substr $what,0,$limit;
		
		}
	
	sprintf('%'.$align.$limit.'s',$what);

}

# extract y and x coordinates from text index

sub split_text_index
{

	my $index=shift;
	
	$index=~/^([^\.]+)\.(.*)/;
	
	my $coords;
	
	$coords->{y}=$1;
	$coords->{x}=$2;
	
	return $coords;

}

1;
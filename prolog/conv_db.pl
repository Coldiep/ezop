#use glob;





sub conv_file
{
my $fn = shift;

open my $file, $fn or die;

local $/;

my $data = <$file>;


$data =~ s/([^\.])\n/$1\.\n/g;


close $file;

open $file, ">$fn" or die("cannot open file $fn");

print $file $data;


close $file;
}




my $mask = shift or die;


my @files = glob($mask); 



  foreach $file (@files) {
   print "convert file $file\n"; 
   conv_file($file);
  }

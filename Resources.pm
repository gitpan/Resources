# Copyright 1995 Francesco Callari, McGill University. See notice
# at end of this file.
#
# Filename: Resources.pm
# Author: Francesco Callari (franco@cim.mcgill.ca)
# Created: Wed May 31 17:55:21 1995
# Version: $Id: 
#    Resources.pm,v 0.1 1995/10/19 02:49:43 franco Exp franco $


=head1 NAME 

Resources - handling application defaults in Perl.

=head1 SYNOPSIS

    use Resources;

    $res = new Resources;
    $res = new Resources "resfile";

=head1 DESCRIPTION

Resources are a way to specify information of interest to program or
packages. 

Applications use resource files to specify and document the values of
quantities or attributes of interest.

Resources can be loaded from or saved to resource files. Methods are
provided to search, modify and create resources.

Packages use resources to hardwire in their code the default values for
their attributes, along with documentation for the attibutes themselves.

Packages inherit resources when subclassed, and the resource names are
updated dynamically to reflect a class hierarchy.

Methods are provided for interactive resource inspection and editing.

=head2 1. Resource Files.

A resource specification in a (text) resource file is a line of the form:

        name: value

There may be any number of whitespaces between the name and the colon
character, and between the colon and the value. 

=over 8

=item B<name>

A B<word> not containing whitespaces, colons (':'), dots ('.') or asterisks
('*'), nor starting with '_'.

A sequence in one the following forms:

        word.name   
        word*name   
        *name

No distinction is made between uppercase and lowercase letters.

The asterisks in a resource name act as wildcards, matching any sequence of
characters. 

=item B<value> 

An unadorned word or a quoted sequence of whitespace-separated words. Both
single (' ') and double quotes quotes (" ") are allowed, and they must be
paired.  

Any I<constant> scalar constructor in Perl, including anon references to
constant arrays or hashes.  

The special words B<yes>, B<true>, B<no>, B<false> (case insensitive) are
treated as boolean resources and converted 1 and 0, unless they are quoted.

=back

Examples of valid resource specifications:

     hotdog*pickles : plenty      # A word.
     hotdog.price   : 22          # Another word
     hotdog.recipe : 'Help me!'   # A quoted sentence
     hotdog*taste*sucks : yes     # A boolean, converted to 1.
     hotdog*smell*sucks : 'yes'   # yes, aken verbatim
     hotdog.size  : [1, [2, 3]]           # An anon array.
     hotdog.lett : {"P"=>1, "Q"=>[2, 3]}  # An anon hash.

Examples of illegal resource names:

     hot dog    # Whitespace in the name.
     .hotdog    # Leading dot in name.
     hot._dog   # Leading underscore in _dog.
     hotdog*    # Trailing asterisk.
     hotdog.    # Trailing dot.

A resource file may contain comments: anything from a hash ('#') character to
the end of a line is ignored, unless the hash character appears inside a
quoted value string.

=head2 2. Resource hashes

Resource hashes provide a way to specify the default values for the attributes
of a package in its source code, along with documentation for the attributes
themselves. The documentation itself is "dynamical" (as opposed to the static,
pod-like variety) in that it follows a class hyerarchy and is suitable for
interactive display and editing.

A resource hash is just a hash of B<($Name> => B<[$Value, $Doc])> things. Each
hash key B<$Name> is a resource name as above. Each hash value is either just
the resource $Value, or a reference to an anon array B<[$Value, $Doc]>, with
B<$Doc> being the resource documentation.

The resource $Name I<cannot> contain wildcard ('*') or colon (':') characters,
nor start or end with a dot ('.'). Also, it must I<not> be prefixed
with the package name (since this is automatically prepended by the B<merge>
method, see below). Names starting with an underscore ('_') character are
special in that they define "hidden" resources. These may not be specified
in resource files, nor dynamically viewed/edited: they come handy to specify
global parameters when you dont want to use global application-wide variables,
and/or want to take advantage of the inheritance mechanism.

The resource $Value can be any I<constant> scalar Perl constructor, including
references to arrays and/or hashes of constants (or references
thereof). Boolean values must be specified as 1 or 0.

The resource documentation is a just string of any length: it will be
appropriately broken into lines for visualization purposes. It can also be
missing, in which case a base class documentation is used (if any exists, see
the B<merge> method below).

The content of a resource hash is registered in a global Resource object using
the B<merge> method.

Here is an example of deafults specification for a package.

        package HotDog;
        @ISA = qw( Food );

        %HotDogDefaults =  (
            taste       => ["Yuck!", "Tongue feeling"],
            meatcolor   => ["Corpse", "Freshness indicator"],
            stinks      => [1, "Does it smell?"],
	    onions      => [ { on => 2, ions => [3, 5] }, "Rings" ],
	    'food.good' => 0, # just a scalar, doc is in base class
	    '_ghost'    => [0, "Hidden. Mr. Invisible"] 
        );

=head2 3. Objects and resources

The recommended way to use resources with Perl objects is to pass a
Resource object to the "new" method of a package.  The method itself will
merge the passed resources with the package defaults, and the passed resource
will override the defaults where needed.

Resource inheritance via subclassing is then easily achieved, amounting
to something like:

        package HotDog;
        @ISA = qw( Food );

        # Called as: $hotdog = new HotDog ($resources);
        sub new {
           my $package = shift;
           my $res = shift;

	   $res = new Resources if (ref($res) !~ /Resources/o);
           $res->merge(\%HotDogDefaults); 

           my $hd = bless new Food ($res);
     
           # Whatever initialization code
           return $hd;
        }

=cut
   
require 5.001;
package Resources;
use strict;
use Carp;
use Safe;
use English;
use FileHandle;

# 
# Global variables
#
use vars qw( $VERSION %Defaults $NAME $VALUE $DOC );

$VERSION = "1.00";

$VALUE=0, $DOC=1;

# Resource defaults
%Defaults = 
  ( 
   'resources.appname'   => ["$0",
			     "The application name of this Resource object."],
   'resources.editor'    => ["/bin/vi", 
			     "Resource editor command."], 
   'resources.pager'     => ["/bin/more", 
			     "Resource pager command."],
   'resources.separator' => [':',
			     "Pattern separating names from values in ".
			     "resource files."],
   'resources.tmpfil'    => ["/tmp/resedit$$", 
			     "Editor temporary file."],
   'resources.updates'   => [0, 
			     "Number of resource updates."],
   'resources.verbosity' => [1, 
			     "True to print warnings."],
   'resources.viewcols'  => [78, 
			     "Width of view/edit window."],
   'resources.writepod'  => [0,
			     "Boolean. True if the write method should ".
			     "output in POD format."],
  );

#
# Method declarations
#
sub new;
sub DESTROY;
sub load;
sub merge;
sub put;
sub valbyname;
sub docbyname;
sub valbypattern;
sub docbypattern;
sub namebyclass;
sub valbyclass;
sub docbyclass;
sub each;
sub names;
sub view;
sub edit;

#
# Unexported subroutines
#
sub _chain_classes;
sub _parse;
sub _parse_ref;
sub _error;
sub _printformat; 
sub _dump;   

=head2 4. Methods in class Resources

=head2 4.1. Creation and initialization

=over 8

=item B<new Resources ($resfile);>

Creates a new resource database, initialized with the defaults for
class Resources (see below for a list of them).

If a nonempty file name is specified in $resfile, it initializes the object
with the content of the so named resource file. For safe (non overwriting)
loading, see the B<load> method below.

If the special file name "_RES_NODEFAULTS" is specified, the object is created
completely empty, with not even the Resources class defaults in it.

Returns the new object, or undef in case of error.

=cut

sub new {
   my $type = shift;
   my $resfile = shift;
   my ($name, $valdoc);
   my $res = bless {};
  
   $res->{'_WILDS'}   = {};   # wildcarded resources
   $res->{'_RES'}     = {};   # named resources

   # Safe environment for the evaluation of constructors.
   $res->{'_SAFE'} = new Safe or 
      ($res->_error("new", "Can't get a Safe object."), return undef);

   # Hack hack - the special filename "_RES_NODEFAULTS" is
   # used to prevent resource initialization (e.g. when called by the
   # "bypattern" method
   unless ($resfile eq "_RES_NODEFAULTS") {
      # Must make sure this is not overridden by a wildcard
      $res->{'_WILDS'}->{'.*resources\.updates'} = [0];
      
      # Just in case of M$-DOG.EXE
      $Defaults{'resources.appname'}->[0]=
	 (split(/\./, $Defaults{'resources.appname'}->[0]))[0];

      # Bootstrap defaults. We don't want any subclassing here 
      while (($name, $valdoc) = each(%Defaults)) {
	 $res->put($name, @{$valdoc});
      }
   }

   if ($resfile && $resfile ne "_RES_NODEFAULTS") {
      $res->load($resfile) || 
	 ($res->_error("new", "Can't load"), return undef);
   }

   $res;
}
    

sub DESTROY {
   my $res=shift;
   Safe::DESTROY($res->{'_SAFE'});
}


=item B<load($resfile, $mode, $nonew);>

Loads resources from a file named $resfile into a resource database. 

The loading is controlled by the $mode argument as follows.

If $mode is omitted, or equal to "replace", the content of the file is loaded
as it is, save for syntax checks.

If $mode is equal to "merge", resource merging via B<merge> is performed if
the database is not empty. This effectively prevents loaded resources from
overwriting those already defined in the database.

The $nonew argument controls whether loading of non already defined resurces is
allowed. If it is true, safe loading is performed: attempting to load
non-wildcarded resource names that do not match those already present in the
database causes an error. This can be useful if you want to make sure that
only pre-defined resources (for which you presumably have hardwired defaults)
are loaded. It can be a safety net against typos in a resource file.

Use is made of B<Safe::reval> to parse values specified through Perl
constructors (only constants, anon hashes and anon arrays are allowed).

Returns 1 if ok, 0 if error.

=cut
 
sub load {
   my $res = shift;
   my ($filnam, $mode, $nonew) = @_;
   my ($lin, @line, $name, @allvals, $value, %allres, $def, @dum);
   my ($sep, $expr, $evaled);

   $res->_error("load","No filename.") && return 0 unless defined $filnam;
   
   $mode = "replace" unless $mode=lc($mode);
   $res->_error("load","Invalid mode.") && return 0 
      unless ($mode eq "replace" || $mode eq "merge");
   
   $res->_error("load", $!) && return 0 unless open(_RESFILE, $filnam);
   $res->{'_SAFE'}->share('$expr');
   $sep = $res->{'_RES'}->{'resource.separator'} || ':';

   while ($lin = <_RESFILE>) {
      chomp $lin;

      # Get rid of trailing comment (but not in quoted strings).
      1 while $lin =~ s/^(.*\".*)\#(.*\".*)$/$1__RES_NO_COMM__$2/ ;
      1 while $lin =~ s/^(.*\'.*)\#(.*\'.*)$/$1__RES_NO_COMM__$2/ ;
      @line = split(/\#/, $lin);
      $def = $line[0];
      $def =~ s/__RES_NO_COMM__/\#/go;
      
      # Skip empty lines
      next unless $def;

      # Split on first separator
      ($name, @allvals)=split(/$sep/, $def);
      $value=join($sep, @allvals);

      # Get rid of trailing/leading whitespaces.
      $name  =~ s/^\s+|\s+$//g;
      $value =~ s/^\s+|\s+$//g;

      next unless $name;

      # Name may not 
      #     - contain whitespaces or
      #     - terminate with wildcard or dot,
      #     - start with dot
      #     - contain ._ sequences (which are for hidden resources only)
      $res->_error("load", "$filnam: line $.: bad resource name: $name") 
	 && return 0 if $name =~ /\s+|^\.|\.$|\*$|\._/o;
     
      # Parse value: 
      # If the whole thing is quoted, take it as it is:
      if ($value =~ s/^\'(.*)\'$|^\"(.*)\"$/$1/ ) {
	 $allres{$name} = [ $value ];
      } elsif ($value =~ /^[\[\{].*/) {
	 # Do anon hashes array
	 $evaled = $res->{'_SAFE'}->reval('$expr=' . $value);
	 if ($@) {
	    $res->_error("load", 
			 "$filnam: error in line $. ($@) - $name : $value");
	    return 0;
	 } else {
	    $allres{$name} = [ $evaled ];
	 }
      } else {
	 # Swallow it anyway, damnit! ;-)
	 $allres{$name} = [ $value ];
      }
   } 
   close(_RESFILE);
    
   # Safe loading checks
   if ($nonew) {
      my $resnames = join(' ', sort($res->names()));

      foreach $name (keys(%allres)) {
	 unless ($resnames =~ /$name/) {
	    $res->_error("load", "unknown resource $name in $filnam");
	    return(0);
	 }
      }
   }

   # Merge
   return $res->merge(\%allres) if $mode eq "merge";

   # Replace
   while (($name, $value) = each(%allres)) {
      $res->put($name, @{$value}) || 
	 (_error("load", "failed put $name : $value") && return 0) 
   }
   1;
}


=item B<merge(\%defaults);>

Merges the resources of $res with the content of the resource hash
%defaults.  Only non-wildcarded names are admissible in %defaults. Merging
does I<not> change the value of resources already defined in $res. 

Note that merging reflects subclassing in that the names of default resources
of a base classe are prepended with the name of the caller package (lowercased
and stripped of all foo::bar:: prefixes), followed by a dot. In the above
example, the defaults of HotDog will be renamed, after merging as:

   hotdog.taste, hotdog.meatcolor, ...

and, if Bell::Taco is a new blessed HotDog, then they will be translated again
upon merging, to read:

   taco.hotdog.taste, taco.hotdog.meatcolor, ...

And so on all the way up to the main package, but with the application name
($0, a.k.a $PROGRAM_NAME in English) prepended in place of "main". See also
the documentation of the "byclass" method below for some conditions under
which this will not work properly.

The new names are the ones to use when accessing these resources by name.

Again, if a resource is already defined when merge is entered, than its
values overrides the defaults. See also the EXAMPLES section.

Returns 1 if OK, 0 if error.

=cut

sub merge {
   my $res = shift;
   my ($def) = @_;
   my ($dname, $dvalue, $doc, $val, $vref, $class);
   
   return 1 unless $def;
   $res->_error("merge", "Argument must be HASH ref") && return 0 
      unless ref($def) =~ /HASH/o;

   # Build up prefix based on class hierarchy
   $class = $res->_chain_classes();

   defloop: while (($dname, $dvalue) = each(%{$def})) {
      # Check for bad args: 
      # Names cannot contain * or :, nor start/end with a dot
      $dname =~ /\*|^\.|\.$|\:/ && do {
	 $res->error("merge", "Bad default resource name: $dname ");
	 return 0;
      };
      # Values must be 2-elements arrays, with a scalar 2nd
      # component (the doc)
      unless(($vref = ref($dvalue)) && ($vref =~ /ARRAY/o) &&
	     scalar(@{$dvalue})<=2 && !ref($dvalue->[1])      )  {
	 $res->_error("merge", "Bad default resource value for ".
		      "resource $dname : $dvalue");
	 return 0;
      };

      $doc=$val='';

      # Do not merge this default (or merge only the documentation) 
      # if already defined by a subclass.
      $dname = $class . $dname;
      if (exists($res->{'_RES'}->{$dname})) {
	 next defloop unless ($doc=$dvalue->[$DOC]);
	 $val = $res->{'_RES'}->{$dname}->[$VALUE];
      } else {
	 $doc = $dvalue->[$DOC];
	 $val = $dvalue->[$VALUE];
      } 

      $res->put($dname, $val, $doc) ||
	 ($res->_error("merge", "error on $dname: $dvalue") && return 0);
   }

   1;		
}

=back

=head2 4.2. Looking up resources

The values and documentation strings stored in a Resource object can be
accessed by specifying their names in three basic ways:

=over 8

=item directly ("byname" methods)

As in "my.nice.cosy.couch" .

=item by a pattern ("bypattern" methods)

As in "m??nice.*" .

=item hierarchically ("byclass" methods)

If class My is a Nice, and Nice is a
Cosy, and if the current stack of method calls goes from package My to Cosy,
then asking for "couch" in package Cosy gets you the value/doc of
"my.nice.cosy.couch". This behaviour is essential to proper resource
subclassing, as explained in detail below.

=back

The "bypattern" methods must search through the Resource object, while the
"byclass" methods must work through call frames and ISA lists. Both ways
tend to be slow in case of large resource databases, so the "byvalue" methods
should always be used where appropriate. See the EXAMPLE section for more.

It is also possible to retrieve the whole content of a resource database
("names" and "each" methods)

Note that all the resource lookup methods return named (non "wildcarded")
resources only. Wildcarded resources (i.e. those specified in resource files,
and whose names contain one or more '*') are best thought as placeholders, to
be used when the value of an actual named resource is set. 

For example, a line in a resource file like

          *background : yellow

fixes to yellow the color of all resources whose name ends with "background".
However, your actual packages will never worry about unless they really need
a background. In this case they either have a "background" resource in
their defaults hash, or subclass a package that has one.

=over 8

=item B<valbyname($name);>
	
Retrieves the value of a named resource from a Resource database. The $name
argument is a string containing a resource name with no wildcards. 

The resource is first looked up with the program name ($0) automatically
prepended to the passed name. If this fails, a second search is done using the
passed name verbatim.

Returns the undefined value if no such resource is defined.

=cut

sub valbyname {
   my $res = shift;	
   my ($name) = @_;	
   my $fullname;

   $fullname = $res->{'_RES'}->{'resources.appname'}->[0] . ".$name";

   if (exists($res->{'_RES'}->{$fullname})) {
      return $res->{'_RES'}->{$fullname}->[$VALUE];
   } elsif (exists($res->{'_RES'}->{$name})) {
      return $res->{'_RES'}->{$name}->[$VALUE];
   } else {
      return undef;
   }
}

=item B<docbyname($name);>

Retrieves the documentation string of a named resource from a Resource
database. The $name argument is a string containing a resource name with no
wildcards. 

The resource is first looked up with the program name ($0) automatically
prepended to the passed name. If this fails, a second search is done using the
passed name verbatim.

Returns the undefined value if no such resource is defined.

=cut

sub docbyname {
   my $res = shift;	
   my ($name) = @_;	
   my $fullname;

   $fullname = $res->{'_RES'}->{'resources.appname'} . ".$name";

   if (exists($res->{'_RES'}->{$fullname})) {
      return $res->{'_RES'}->{$fullname}->[$DOC];
   } elsif (exists($res->{'_RES'}->{$name})) {
      $res->{'_RES'}->{$name}->[$DOC];
   } else {
      return undef;
   }
}


=item B<bypattern($pattern);>

Retrieves the full names, values and documentation strings of all the named
(non wildcarded) resources whose name matches the given $pattern. The pattern
itself is string containing a Perl regular expression, I<not> enclosed in
slashes.

Returns a new Resource object containing only the matching resources, or 
the undefined value if no matches are found.

=cut

sub bypattern {
   my $res = shift;	
   my ($pattern) = @_;	
   my ($name, $valdoc);
   my $newres = new Resources("_RES_NODEFAULTS") || return undef;

   while (($name, $valdoc) = $res->each()) {
      $newres->put($name, @{$valdoc}) if $name =~ /$pattern/ ;
   }

   return $newres if %{$newres->{'_RES'}};
   undef;
}

=item B<valbypattern($pattern);>

Retrieves the full names and values of all named (non wildcarded) resources
whose name matches the given pattern. 

Returns a new Resource object containing only names and values of the matching
resources (i.e. with undefined doc strings), or the undefined value if no
matches are found.

=cut

sub valbypattern {
   my $res = shift;	
   my ($pattern) = @_;	
   my ($newres, $i);
   
   $newres = $res->bypattern($pattern) || return undef;
   for $i ($newres->names()) {
      undef($newres->{'_RES'}->{$i}->[$DOC]); 
   }
   
   $newres;
}

=item B<docbypattern($pattern);>

Retrieves the full names and documentation strings of all named (non
wildcarded) resources whose name matches the given pattern.

Returns a new Resource object containing only names and docs of the matching
resources (i.e. with undefined resource values), or the undefined value if no
matches are found.

=cut

sub docbypattern {
   my $res = shift;	
   my ($pattern) = @_;	
   my ($newres, $i);
   
   $newres = $res->bypattern($pattern) || return undef;
   for $i ($newres->names()) {
      undef($newres->{'_RES'}->{$i}->[$VALUE]); 
   }
   
   $newres;
}



=item B<byclass($suffix);>

Resources are naturally ordered with respect to a hierarchy of classes, in
that if two resource names share a common suffix, then the resource defined in
the subclass overrides the one of a base class (unless the former is
wildcarded in a resource file).

For example, if class HotDog has a default for a resource named
"hotdog.pickles", and class Taco (a subclass of HotDog) has a default for
"taco.hotdog.pickles", then we expect that the latter should override the
former in a Taco object.  

Note however that we do I<not> want "mc.donalds.hotdog.pickles" to override
"taco.hotdog.pickles", for a Mc class unrelated to Taco. This means that
finding the right default in the database is not just a matter of counting
dots, but of following a class hierarchy,

This method returns a 3 element list containing the name/value/doc tuple of
the the most subclassed resource from the current class, among those having a
common suffix. In the above example, a call like

        package HotDog 
        ...
        ($name,$value,$doc) = $res->byclass("pickles");

will set $name, $value and $doc equal to those of the "taco.hotdog.pickles"
resource, if this HotDog is subclassed by Taco, and to those of
"mc.donalds.hotdog.pickles", if it is subclassed by Mc via Donalds instead.

To do this, the algorithm walks upward in the current subroutine call stack,
and looks up the ISA list of each caller package to determine whether the
callers are subclasses or not. Since it depends on the current call stack, it
will work only when calls are really nested, i.e. with inherited methods or
explicitly nested calls (e.g. in the "new" method of subclasses).

The passed name suffix must not contain wildcards, nor start with a dot.

Be careful not to confuse the "byclass" with the "byname" and "bypattern"
retrieval methods: they are used for two radically different goals. See the
EXAMPLES section for more.

Returns the empty list if no resources are found for the given suffix,
or if the suffix is incorrect.

=cut

sub byclass {
   my ($res, $suffix) = @_;
   my ($name, $value, $doc);
   my (@rest, $base);

   # No patterns or leading/trailing dots
   $suffix =~ /^\.|\.$|\*/ && do {
      $res->_error("byclass", "bad suffix $suffix");
      return ();
   };
   
   # Chain classes starting from leftmost in suffix.
   if ($suffix =~ /\./o) {
      ($base, @rest) = split(/\./, $suffix);
   } else {
      $base='';
      @rest=($suffix);
   }

   $base = $res->_chain_classes($base) or return ();

   $name = "$base@rest";

   return () unless exists($res->{'_RES'}->{$name});

   ($value, $doc) = @{$res->{'_RES'}->{$name}};

   return ($name, $value, $doc);
}


=item B<namebyclass($suffix);>

As the B<byclass> method above, but returns just the resource name (i.e. the
suffix with all the subclasses prepended).

=cut

sub namebyclass {
   my ($res, $suffix) = @_;
   my @nvd = $res->byclass($suffix);
   
   $nvd[0];
}

=item B<valbyclass($suffix);>

As the B<byclass> method above, but returns just the resource value.

=cut

sub valbyclass {
   my ($res, $suffix) = @_;
   my @nvd = $res->byclass($suffix);
   
   $nvd[1];
}


=item B<docbyclass($suffix);>

As the B<byclass> method above, but returns just the resource documentation.

=cut

sub docbyclass {
   my ($res, $suffix) = @_;
   my @nvd = $res->byclass($suffix);
   
   $nvd[2];
}



=item B<each;>

Returns the next name/[value,doc] pair of the named (non wildcarded) resources
in a resource database, exactly as the B<each> Perl routine. 

=cut

sub each {
   my $res=shift;
   my ($name, $val, $doc) = each(%{$res->{'_RES'}}) or return ();

   return ($name, $val, $doc);
}


=item B<names;>

Returns a list of the names of all named (non-wildcarded) resources in a
resource database, or undef if the databasee is empty.

=cut

sub names {
   my $res=shift;
   my @names=keys(%{$res->{'_RES'}});
   
   return @names;
}

=head2 4.3. Assigning and removing Resources

=item B<put($name, $value, $doc);>

Writes the value and doc of a resource in the database.  It is possible to
specify an empty documentation string, but name and value must be defined.

Wildcards ('*' characters) are allowed in the $name, but the $doc is ignored
in this case (documentation is intended for single resources, not for sets
of them).

The value is written unchanged unless the resource database already
contains a wildcarded resource whose name includes $name (foo*bar
includes foo.bar, foo.baz.bar, etc.). In this case the value of the
wildcarded resource overrides the passed $value.

Returns 1 if ok, 0 if error.

=cut 

sub put {
   my $res=shift;
   my ($name, $value, $doc) = @_;
   my ($r);

   $res->_error("put", "name or value undefined") and return 0 
      unless defined($name) && defined($value);

   $name = lc($name);

   # Name must be one word and may not terminate with wildcard or dot
   # or start with dot. Must check here too because of defaults.
   $res->_error("put", "bad resource name: $name") && return 0
      if scalar(split(/\s+/, $name)) > 1 || $name=~/^\.|\.$|\*$/;

   # Do booleans.
   $value =~ s/^true$|^yes$/1/i;
   $value =~ s/^false$|^no$/0/i;
   
   # Do wildcards (they take priority over named)
   # Match of wildcards is done hyerarchically:
   #      *b  contains a*b
   #      a*b contains a*c*b
   # In case of conlict, newer overwrite older ones.
   if ($name =~ /\*/) {

      my ($I_have, $patname, $wild);
      $I_have=0;

      # Dots must be matched literally when name is used as a pattern
      ($patname = $name) =~ s/\./\\\./go;

      # a*b => a.*b (regexp cannot start with *)
      $patname =~ s/\*/\.\*/g;

      # First compare with known wildcarded resources.
      foreach $wild (keys(%{$res->{'_WILDS'}})) {
	 # Remove old wildcards if the new one contains them 
	 ($wild =~ /$patname\Z/) && delete($res->{'_WILDS'}->{$wild});

	 # Skip if a more general old one is found
	 ($name =~ /$wild\Z/) && ($I_have = 1, last);
      }
      $res->{'_WILDS'}->{$patname}=[$value, undef] unless $I_have;

      # Then update the old named ones 
      foreach $r (keys(%{$res->{'_RES'}})) {
	 $res->{'_RES'}->{$r}->[$VALUE] = $value if $r =~ /$patname\Z/; 
      }

   } else { 
      # Named resources.
      # Check if it is already wildcarded: if so, use wildcard's value
      my $wild; 
      foreach $wild (keys(%{$res->{'_WILDS'}})) {
	 if ($name =~ /$wild\Z/) {
	    $value = $res->{'_WILDS'}->{$wild}->[$VALUE];
	    last;
	 }
      }
      $res->{'_RES'}->{$name}->[$VALUE]=$value;
      $res->{'_RES'}->{$name}->[$DOC]=$doc if length($doc);
   }

   1;
}


=item B<removebyname($name);>

Removes the named (non wildcarded) resources from the database.

Returns 1 if OK, 0 if the resource is not found in the database.

=cut

sub removebyname {
   my $res = shift;
   my ($name) = @_;
   my ($i, $cnt, $newres);

   return 0 unless exists $res->{'_RES'}->{$name};
   delete($res->{'_RES'}->{$name});
   1;
}

=item B<removebypattern($name);>

Removes from the database all resources (both named I<and> wildcarded) whose
name mathes $pattern. An exactly matching name must be specified for
wildcarded resources (foo*bar to remove foo*bar).

Returns the number of removed resources.

=cut

sub removebypattern {
   my $res = shift;
   my ($name) = @_;
   my ($i, $cnt, $newres);

   $newres=$res->bypattern($name) || return 0;

   foreach $i ($newres->names()) {
      delete($res->{'_RES'}->{$i});
      $cnt++;
   }
   foreach $i (keys(%{$res->{'_WILDS'}})) {
      ($cnt++ , delete($res->{'_WILDS'}->{$i})) if $i eq $name;
   }

   $cnt;
}


=head2 4.6. Viewing and editing resources.

=item B<view;>

Outputs the current content of a Resource object by piping to a pager program.

The environment variable $ENV{RESPAGER}, the resource "resources.pager" and
the environment variable $ENV{PAGER} are looked up, in this very order, to
find the pager program. Defaults to B</bin/more> if none of them is found.

The output format is the same of a resource file, with the resource names
alphabetically ordered, and the resource documentation strings written
as comments.

Returns 1 if ok, 0 if error.

=cut

sub view {
   my $res=shift;
   my ($name, $value, $doc, $view, $pager, $p);

   if ($p = $ENV{RESPAGER}) {
      $pager = $p;
   } elsif ($p = $res->valbyname("resources.pager")) {
      $pager = $p;
   } elsif ($p = $ENV{PAGER}) {
      $pager = $p;
   } else {
      $pager='/bin/more';
   }

   # Make sure we don't output POD.
   my $pod = $res->valbyname("resources.writepod");
   $res->put("resources.writepod", 0);

   $p = $res->write("|$pager");
   $res->_error("view", "write failed") unless $p;

   $res->put("resources.writepod", $pod);
   
   return $p;
}


=item B<edit($nonew);>

Provides dynamical resource editing of a Resource object via an external
editor program. Only resource names and values can be edited (anyway, what is
the point of editing a resource comment on the fly?).

The environment variables $ENV{RESEDITOR}, the resource "resouces.editor", the
environment variables $ENV{VISUAL} and $ENV{EDITOR} are looked up, in this
very order, to find the editor program. Defaults to B</bin/vi> if none is
found.

The editor buffer is initialized in the same format of a resource file, with
the resource names alphabetically ordered, and the resource documentation
strings written as comments. The temporary file specified by the
"resources.tmpfil" resource is used to initialize the editor, or
'/tmp/resedit<pid>' if that resource is undefined.

When the editor is exited (after saving the buffer) the method attempts to
reload the edited resources. If an error is found the initial object is left
unchanged, a warning with the first offending line in the file is printed, and
the method returns with undef. Controlled resource loading is obtained by
specifying a true value for the $nonew argument (see B<load>).

If the loading is successful, a new (edited) resource object is returned,
which can be assigned to the old one for replacement. 

After a successful edit, the value of the resource "resources.updates" (which
is always defined to 0 whenever a new resource is created) is increased by
one. This is meant to notify program the and/or packages of the resource
change, so they can proceed accordingly if they wish.

=cut

sub edit {
   my ($res, $nonew) = @_;
   my ($newres, $editor, $p, $status, $tmpfil);

   if ($p = $ENV{RESEDITOR}) {
      $editor = $p;
   } elsif ($p = $res->valbyname("resources.editor")) {
      $editor = $p;
   } elsif ($p = $ENV{VISUAL}) {
      $editor = $p;
   } elsif ($p = $ENV{EDITOR}) {
      $editor = $p;
   } else {
      $editor='/bin/vi';
   }

   $tmpfil = ($res->valbyname("resources.tmpfil") || "/tmp/resedit$$.txt");

   # Make sure we don't output POD.
   my $pod = $res->valbyname("resources.writepod");
   $res->put("resources.writepod", 0);
   $p = $res->write($tmpfil);
   $res->put("resources.writepod", $pod);

   $p || (_error("edit", "write failed") && return $p);

   $status = system("$editor $tmpfil");
   return 0 if $status>>8; # Editor failed

   $newres = new Resources() || undef;
   $newres->load($tmpfil, "replace", $nonew) || undef($newres);
   unlink($tmpfil);

   return $newres;
}

=head2 4.5. Miscellaneous methods

=item B<write($filename);>

Outputs all resources of a resource database into a resource file (overwriting
it). 

The resource documentation strings are normally written as comments, so the
file itself is immediately available for resource loading. However, if the
boolean resource "resources.writepod" is true, then the (non wildcarded)
resources are output in POD format for your documentational pleasure.

As usual in Perl, the filename can allo be of the form "|command", in which
case the output is piped into "comma1nd".

For resources whose value is a reference to an anon array or hash, it produces
the appropriate constant Perl contructor by reverse parsing. The parser itself
is available as a separate method named B<_parse> (see package source for
documentation).

Returns 1 if ok, 0 if error.

=cut
 
sub write {
   my $res = shift;
   my ($filnam) = @_;
   my ($name, $value, $doc, $view);

   $res->_error("write", "No filename") && return 0 unless defined $filnam;
   $filnam = ">$filnam" unless $filnam =~ /^\|/;
   ($res->_error("write", $!) && return 0) unless open(RESOUT, $filnam);

   autoflush RESOUT (1);

   if ($res->valbyname("resources.writepod")) {

      print RESOUT "=head2 Resources\n\n=over 8\n";

      for $name (sort($res->names())) {
	 next if $name =~ /\._/; # hidden

	 my $val = $res->valbyname($name);
	 my @doclines=split(/ /, $res->docbyname($name));
	 my $len=0;
	 my $lin;

	 $val = $res->_parse($val) if ref($val);
	 print RESOUT "\n=item $name : $val\n\n";
	 
	 while (scalar(@doclines)) {
	    $lin='';
	    while (length($lin)<60 && scalar(@doclines)) {
	       $lin .= shift(@doclines) . ' ';
	    }
	    chomp $lin;
	    print RESOUT "$lin\n";
	 }
      }

   } else {
      $view = "#\n# Wildcarded resources\n#\n";
      
      for $name (sort(keys(%{$res->{'_WILDS'}}))) {
	 ($value, $doc) = @{$res->{'_WILDS'}->{$name}};
	 $name =~ s/\\\./\./go;
	 $name =~ s/\.\*/\*/go;
	 $value = $res->_parse($value) if ref($value);
	 $view .= "$name : $value" . "__RES_COMM__$doc\n";
      }
      
      $view .= "#\n# Named resources\n#\n";
      
      for $name (sort($res->names())) {
	 next if $name =~ /\._/o; # "hidden" resource
	 $value = $res->valbyname($name);
	 $doc = $res->docbyname($name);
	 $value = $res->_parse($value) if ref($value);
	 $view .= "$name : $value" . "__RES_COMM__$doc\n";
      }
      
      $res->_printformat(\*RESOUT, $view);
      close(RESOUT);
   }
}

 
#
# LOCAL (UNEXPORTED) METHODS
#
#


# $res->_dump -- dumps the content of res on stderr. Used for debugging.
#
sub _dump {
   my $res=shift;
   my ($name, $value, $doc, $valdoc);
   warn "_dump: WILDCARDED RESOURCES\n";
   for $name (sort(keys(%{$res->{'_WILDS'}}))) {
      $value= $res->{'_WILDS'}->{$name}->[$VALUE]; 
      $name =~ s/\.\*/\*/g;
      $name =~ s/\\//g;
      warn "_dump: $name : $value\n";
   }

   warn "_dump: NAMED RESOURCES\n";
   for $name (sort(keys(%{$res->{'_RES'}}))) {
      $valdoc= $res->{'_RES'}->{$name}; 
      $name =~ s/\\//g;
      $value= $valdoc->[$VALUE];
      $doc=$valdoc->[$DOC];
      warn "_dump: $name : $value # $doc\n";
   }
}

# _parse($value) -- Returns a string containing the value of a resource $name,
#                   written in the same format as for a resource file (i.e. in
#                   Perl syntax if the value is not a scalar.
#                   Returns the string, or undef in case of errors.
#
sub _parse {
   my $res=shift;
   my ($value) = @_;
   my ($ref);

   return $value unless $ref = ref($value);
   return _parse_ref($value, $ref);
}   

#  
# _parse_ref -- This does recursive parsing for hass/array references .
#

sub _parse_ref {
   my ($value, $ref) =@_;
   my $parsed='';
   
   $ref eq 'ARRAY' && do {
      my $element;
      $parsed = '[';
      for $element (@{$value}) {
	 my $refref;
	 if ($refref = ref($element)) {
	    my $parspars = _parse_ref($element, $refref)
	       || return undef;
	    $parsed .= $parspars;
	 } elsif (_isint($element) || _isreal($element)) {
	    $parsed .= "$element, ";
	 } else {
	    $parsed .= "'$element', ";
	 }
      }
      $parsed =~ s/,\s$//;
      $parsed .= ']';
      return $parsed;
   };

   $ref eq 'HASH' && do {
      my ($nam, $val);
      $parsed = '{';
      while (($nam, $val) = each(%{$value})) {
	 my $refref;
	 return undef if (ref($nam));
	 if ($refref = ref($val)) {
	    my $parspars = _parse_ref($val, $refref)
	       || return undef;
	    $parsed .= "'$nam' => $parspars, ";
	 } elsif (_isint($val) || _isreal($val)) {
	    $parsed .= "'$nam' => $val, ";
	 } else {
	    $parsed .= "'$nam' => '$val', ";
	 }
      }
      $parsed =~ s/,\s$//;
      $parsed .= '}';
      return $parsed;
   };

   return undef; # We do only arrays and hashes

   sub _isint {
      my ($num)=@_;
      $num =~ /\A-?\d+/o;
   }
   sub _isreal {
      my ($num)=@_;
      $num =~ /((-?\d*\.\d+)|(-?\d*\.\d+[eE]-?\d+))/o;
   }
}
	 
#
# _chain_classes($base) -- Returns a resource name prefix obtained by
#    chaining, left to right, the names of the caller packages (lowercased,
#    deprived of leading :: qualifiers, and exclusive of Resources and main)
#    that are hyerarchically subclassed starting from class $base (or from the
#    package imemdiately above Resources if $base is omitted). The order is
#    left to right from subclass to superclass. To find the subclasses we look
#    for each package name in the @ISA list of its caller.  
#    Returns undef if $base is not in the hierarchy.
#
sub _chain_classes {
   my $res=shift;
   my ($base) = @_;
   my ($thispkg, $abovepkg, $filename, $line, $callerlevel);
   my ($class, $var, $isa);
 
   # Up one frame as long as called from within package Resources
   for (($thispkg, $filename, $line) = caller; 
	$thispkg =~ /Resources/oi;
	($thispkg, $filename, $line) = caller(++$callerlevel)) {
      ;
   }

   # Up one frame as long we're at $base.
   if ($base && $thispkg !~ /$base/i) {
      my $done=0;
      do {
	 ($thispkg, $filename, $line) = caller($callerlevel++); 
	 $thispkg ne 'main' and $done=1;
      } while ($thispkg !~ /$base/i && !$done);
   }

   # When $thispkg == "Foo::Bar", $class == "bar."
   unless (($class = $thispkg) eq 'main') {
      $class =~ s/.*::(\w+)$/$1/;
      $class = lc($class) . '.' if $class;
   }

   # Now get all hierarchy of classes up to main exclusive, and prepend to the
   # resource name only the subclass names. To find the subclasses we look for
   # thispkg name in the @ISA list of its caller.
   $abovepkg=$class;
   while ($abovepkg ne 'main' && 
	  (($abovepkg, $filename, $line) = caller($callerlevel++))) {   
      do {
	 no strict; # To use the symbolic reference
	 $var = "$abovepkg\::ISA";
	 $isa = join(' ', @{$var});
      };
      next if $isa !~ /$thispkg/; # Prepend only if subclass
      $thispkg = $abovepkg;
      $abovepkg =~ s/.*::(\w+)$/$1/; 
      $abovepkg = lc($abovepkg) . '.';
      $class = $abovepkg . $class; 
   }

   # If abovepkg is not main here, we are in trouble;
   return undef unless $abovepkg eq 'main';
   $class = '' if $class eq 'main';

   # Add final touch: appname
   $class = lc($res->{'_RES'}->{'resources.appname'}->[0]) . '.' . $class;

   return $class;
}


#
# _error ($subname) - wrapper around caller(), used for debugging
#
sub _error {
   my $res=shift;
   my ($place, $msg) = @_;

   $res->valbyname("resources.verbosity") &&
      warn("error: $0: Resources: $place, $msg\n");
      
   1;
}


#
# _printformat($fh, $msg) 
#        prints to filehandle $fh the documentation $doc.
#       formatted in resources.viewcolumn  columns, not breking expression and
#       continuing comments. 
#

sub _printformat {
   my $res=shift;
   my ($fh, $msg) = @_;
   my ($line, $cols, $def, $comm, @comms);

   $cols = $res->valbyname("resources.viewcols");
   $cols = 78 unless $cols;

   for $line (split(/\n/, $msg)) {
      # print right away if it's short
      if (length($line) <= $cols) {
	 $line =~ s/__RES_COMM__$//o;
	 $line =~ s/__RES_COMM__/ \# /;
	 print $fh "$line\n";
	 next;
      }
      
      ($def, $comm) = split(/__RES_COMM__/, $line);

      my $deflen = length($def)+1;

      my $commlen = ($cols-$deflen) % $cols;
      @comms = split(/\s+/, $comm);
      shift(@comms) unless $comms[0];

      print $fh ("$def # ", _commwds($commlen, \@comms), "\n");
      while ($comm=_commwds($commlen, \@comms)) {
	 $comm = ' ' x $deflen . "# $comm";
	 print $fh "$comm\n";
      }
   }

   sub _commwds {
      my ($len, $comp) = @_;
      my ($shft, $wd, $ls, $lw);
       
      $ls=1;
      while (1) {
	 $wd=shift(@{$comp});
	 last unless $wd;
	 $lw=length($wd)+1;
	 last if $lw + $ls > $len;
	 $shft .= "$wd ";
	 $ls += $lw;
      }
      unshift(@{$comp}, $wd) if $wd;
      return $shft;
   }
}

			     
1;

__END__
# Local Variables:
# mode: perl
# End:


#
# Resources::test()  - test routine
#
sub _test {
   my ($nam, $val);
   my %defaults=(
		 'res1' => 1,
		 'res2' => '\'2 3 4\'',
		 'res3' => [5,6,7,[1,2,3]],
		 'res4' => '\'/a/b/c goo bar\'',
		 'res5' => {'a' => 4, 'b' => [3,4,{'q'=>3}]},
		 );
   
   sleep 3; warn "\n\nNOW CREATING AND DUMPING\n\n";

   my $res=new Resources();
   die "_test: new failed\n" unless $res;
   $res->merge(\%defaults);
   $res->_dump();

   sleep 3; warn "\n\nNAMES\n";
   foreach $nam ($res->names()) {warn "$nam\n"} 

   sleep 3; warn "\n\nNOW SAVING\n\n";
   $res->write("gogo.res");
   
   sleep 3; warn "\n\nNOW RELOADING AND VIEWING\n\n";
   $res->load("gogo.res");
   $res->view();

   sleep 3; warn "\n\nNOW EDITING\n\n";
   $res=$res->edit();

   sleep 3; warn "\n\nNOW VIEWING EDITED RES\n\n";
   $res->view();
   1;
}

=head2 5. Resources of Resources

As you may have guessed at this point, the default configuration of this
package itself is defined by resources. The resource class is, of course,
"resources" (meaning that all the names have a leading "resource.").

To prevent chaos, however, these resources cannot be subclassed. This should
not be a problem in normal applications, since the Resource package itself is
not meant to be subclassed, but to help building a hierarchy of classes
instead.

The currently recognized resources, and their default values, are:

=item resources.appname : ''

The application name of this Resource object. 

=item resources.editor : /bin/vi

Resource editor. 

=item resources.pager : /bin/more

Resource pager. 

=item resources.separator : ':'

Pattern separating names from values in resource files. 

=item resources.tmpfil : ''

Editor temporary file. 

=item resources.updates : 0

Number of resource updates. 

=item resources.verbosity : 1

True to print warnings. 

=item resources.viewcols : 78

Width of view/edit window. 

=item resources.writepod : false

Boolean. True if the write method should output in POD format. 

=back

=head1 EXAMPLES

Here is a more complex example of hyerarchical inheritance of resources.
Package HotDog is subclassed from Junk and Food. The subclass has defaults for
two resources defined by the base classes ("food.edible" and "junk.germs"),
and their values will override the base class defaults. 

Remember that after merging all resources names are prefixed with the current
class name.

   use Resources;
   package Food;
   %FoodDefaults = ( edible => "Sure" );
   
   sub new {
      my ($pack, $res) = @_;
      $res = new Resources unless $resources;
      $res->merge(\%FoodDefaults) || die ("can't merge defaults");
       
      my $food= bless {};
      $food->{Edible} = $res->valbyclass("edible");
      # Use valbyclass so a subclass like HotDog can change this by its
      # defaults.   
   }
 
   # A Food method to say if it can be eaten.
   sub eat { 
      my $food=shift; 
      return $food->{Edible}; 
   }

   package HotDog;
   use Junk;
   use Food;
   use Carp;
   @ISA = qw( Junk Food );

   %HotDogDefaults = 
      (
       taste         => "Yuck!",
       meatcolor     => "Cadaveric",
       stinks        => 1,
       'food.edible' => "Perhaps", # Quotes needed in these 
       'junk.germs'  => "Amoeba"   #    names because of the dots
      );

   sub new {
      my ($package, $res) = @_;
      
      $res = new Resources unless $resources;
      $res->merge(\%HotDogDefaults) || die ("can't merge defaults");
	       
      my $food   = new Food ($res); # Override Food's edible.
      my $junk   = new Junk ($res); # Override Junks's germs.
      my $hd = bless { %{food}, %{junk} }; # Merge and subclass.

      $hd->{Meatcolor} = $res->valbyclass("meatcolor");
      # Same reason as above

      return $hd;
   }

   # A HotDog method to get a taste via resources
   sub taste { 
      my ($hd, $res) = @_; 
      print $res->valbyname("hotdog.taste");
   }

   package main;
   # Whatever
   #
   $res = new Resources("AppDefltFile") || die;
   $snack = new HotDog($res);  
   $gnam = $snack->eat();  # hotdog.food.edible overridees food.edible, 
                           #   so here $gnam equals "Perhaps"

   $slurp = $snack->taste($res) # $slurp equals "Yuck!", unless 
                                # this resource too was overridden 
                                # by a subclass of HotDog , or
                                # differently specified in 
                                # "AppDefltFile"



Note the different use of B<valbyclass>  and B<valbyname>  to access
resources:

=item B<valbyclass>

Is used for the resources like "edible" in Food, i.e. resources which are
defined by the current class in its defaults. This because a subclass of it
(e.g. HotDog) might have overridden them (by having an entry for "food.edible"
in its defaults. 

Generally speaking, you should not use B<valbyclass> outside the B<new>
(creation and initialization) method of a class.

=item B<valbyname>

Is used after the initialization, when all subclassed resources have been
resolved nicely. In any case, its arguments is the complete resource name,
with all its dots.

=head1 SEE ALSO

Safe(3). 

=head1 BUGS

The underlying idea is to use a centralized resource database for the whole
application. This ensures uniformity of behaviour across kin objects, but
allow special characterizations only at the cost of subclassing.

=head1 AUTHOR

	Francesco Callari <franco@cim.mcgill.ca> 
	Artifical Perception Laboratory,
	Center for Intelligent Machines, 
	McGill University.

        WWW: http://www.cim.mcgill.ca/~franco/Home.html

=head1 COPYRIGHT

Copyright 1996 Francesco Callari, McGill University

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose without fee is hereby granted without fee,
provided that the above copyright notice appear in all copies and that both
that copyright notice and this permission notice appear in supporting
documentation, and that the name of McGill not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  McGill makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.

MCGILL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT SHALL
MCGILL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN
AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

=cut

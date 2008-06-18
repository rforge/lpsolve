
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<! --- R-Forge Logo --- >
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<h1 align="left"><u>Using lp_solve in R</u></h1>

<p>
R is a language and environment for statistical computing and graphics. It is a GNU project which is similar to the S language and environment which was developed at Bell Laboratories (formerly AT&T, now Lucent Technologies) by John Chambers and colleagues. R can be considered as a different implementation of S. There are some important differences, but much code written for S runs unaltered under R.  For more information or to download R please visit the R <a href="http://www.r-project.org">website</a>.
</p>

<p>
There are currently two R packages based on lp_solve. The <i>lpSolve</i> R package provides high-level functions for solving general linear/integer problems, assignment problems and transportation problems. The <i>lpSolveAPI</i> R package provides an R API mirroring the lp_solve C API and hence provides a great deal more functionality but has a steeper learning curve. Both packages are available from <a href="http://cran.r-project.org/">CRAN</a>.
</p>

<h5>Installation</h5>

<p>
To install the lpSolve package use the command command:

<pre>
  &gt; install.packages("lpSolve")
</pre>

and to install the lpSolveAPI package use the command:
<pre>
  &gt; install.packages("lpSolveAPI")
</pre>
</p>

<h5>Note</h5>

<p>
The <tt>&gt;</tt> shown before each R command is the R prompt.  Only the text after <tt>&gt;</tt> must be entered.
</p>


<h3>Getting Help</h3>

<p>
Documentation is provided for each function in the lpSolve and lpSolveAPI packages using R's built-in help system.  For example, the command

<pre>
  &gt; ?lp
</pre>

will display the documentation for the <tt>lp</tt> function in the lpSolve package. The lpSolve package also contains the functions <tt>lp.assign</tt> and <tt>lp.transport</tt> which solve assignment and transportation problems. Read these three help files first. If you have a problem that can not be solved using one of these three functions you will have to use the lpSolveAPI package.
</p>

<h3>Building and Solving Linear Programs Using the lpSolveAPI R Package</h3>

<p>
The lpSolveAPI package provides an API for building and solving linear programs that mimics the lp_solve C API.  This approach allows much greater flexibility but also has a few caveats.  The most important is that the <i>lpSolve linear program model objects</i> created by <tt>make.lp</tt> and <tt>read.lp</tt> are not actually R objects but external pointers to lp_solve 'lprec' structures.  R does not know how to deal with these structures.  In particular, R cannot duplicate them.  Thus one must never assign an existing lpSolve linear program model object in R code.
</p>

<p>
Consider the following example.  First we create an empty model x.

<pre>
  &gt; x <- make.lp(2, 2)
</pre>

Then we assign x to y.

<pre>
  &gt; y <- x
</pre>

Next we set some columns in x.

<pre>
  &gt; set.column(x, 1, c(1, 2))
  &gt; set.column(x, 2, c(3, 4))
</pre> 

And finally, take a look at y.

<pre>
  &gt; y
  Model name: 
              C1    C2         
  Minimize     0     0         
  R1           1     3  free  0
  R2           2     4  free  0
  Type      Real  Real         
  upbo       Inf   Inf         
  lowbo        0     0         
</pre>

The changes we made in x appear in y as well.  Although x and y are two distinct objects in R, they both refer to the <b>same</b> lp_solve 'lprec' structure.
</p>

<p>
The safest way to use the lpSolve API is inside an R function - do not return the lpSolve linear program model object.
</p>

<h5>Learning by Example</h5>

<pre>
  &gt; lprec <- make.lp(0, 4)
  &gt; set.objfn(lprec, c(1, 3, 6.24, 0.1))
  &gt; add.constraint(lprec, c(0, 78.26, 0, 2.9), ">=", 92.3)
  &gt; add.constraint(lprec, c(0.24, 0, 11.31, 0), "<=", 14.8)
  &gt; add.constraint(lprec, c(12.68, 0, 0.08, 0.9), ">=", 4)
  &gt; set.bounds(lprec, lower = c(28.6, 18), columns = c(1, 4))
  &gt; set.bounds(lprec, upper = 48.98, columns = 4)
  &gt; RowNames <- c("THISROW", "THATROW", "LASTROW")
  &gt; ColNames <- c("COLONE", "COLTWO", "COLTHREE", "COLFOUR")
  &gt; dimnames(lprec) <- list(RowNames, ColNames)</pre>

Lets take a look at what we have done so far.

<pre>
  &gt; lprec  # or equivalently print(lprec)
  Model name: 
              COLONE    COLTWO  COLTHREE   COLFOUR          
  Minimize         1         3      6.24       0.1          
  THISROW          0     78.26         0       2.9  >=  92.3
  THATROW       0.24         0     11.31         0  <=  14.8
  LASTROW      12.68         0      0.08       0.9  >=     4
  Type          Real      Real      Real      Real          
  upbo           Inf       Inf       Inf     48.98          
  lowbo         28.6         0         0        18
</pre>

Now lets solve the model.

<pre>
  &gt; solve(lprec)
  [1] 0

  &gt; get.objective(lprec)
  [1] 31.78276

  &gt; get.variables(lprec)
  [1] 28.60000  0.00000  0.00000 31.82759

  &gt; get.constraints(lprec)
  [1]  92.3000   6.8640 391.2928
</pre>

<p>
Note that there are some commands that return an answer.  For the accessor functions (generally named get.*) the output should be clear.  For other functions (e.g., <tt>solve</tt>), the interpretation of the returned value is described in the documentation.  Since <tt>solve</tt> is generic in R, use the command

<pre>
  &gt; ?solve.lpExtPtr
</pre>

to view the appropriate documentation.  The assignment functions (generally named set.*) also have a return value - often a logical value indicating whether the command was successful - that is returned invisibly.  Invisible values can be assigned but are not echoed to the console.  For example,

<pre>
  &gt; status <- add.constraint(lprec, c(12.68, 0, 0.08, 0.9), ">=", 4)
  &gt; status
  [1] TRUE
</pre>

indicates that the operation was successful.  Invisible values can also be used in flow control.
</p>


<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>

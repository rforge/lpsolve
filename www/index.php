
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
There are currently two R packages based on lp_solve. The <i>lpSolve</i> package provides high-level functions for solving general linear/integer problems, assignment problems and transportation problems. The <i>lpSolveAPI</i> package provides a complete implementation of the lp_solve API.  The <i>lpSolveAPI</i> package has a lot more functionality than <i>lpSolve</i>, however, it also has a slightly more difficult learning curve. Both packages are available from <a href="http://cran.r-project.org/">CRAN</a>.
</p>

<p>
<span style="color:red">Caveat (19.04.2011): the <i>lpSolve</i> package is based on lp_solve version 5.5.0.7 which was released on 27.12.2007.  The current version of lp_solve (used in the <i>lpSolveAPI</i> package) is 5.5.2.0 and was released on 22.08.2010.</span> 
</p>

<p>
You can find the <strong>project summary page</strong> <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>.
</p>

<h5>Installation</h5>

<p>
To install the <i>lpSolve</i> package use the command:

<pre>
  &gt; install.packages("lpSolve")
</pre>

and to install the <i>lpSolveAPI</i> package use the command:

<pre>
  &gt; install.packages("lpSolveAPI")
</pre>

After the packages have been downloaded and installed, you can load them into your R session using the <tt>library</tt> function, e.g.,

<pre>
  &gt; library(lpSolveAPI)
</pre>

This needs to be done once in each R session (i.e., every time you launch R).

</p>

<h5>Note</h5>

<p>
The <tt>&gt;</tt> shown before each R command is the R prompt.  Only the text after <tt>&gt;</tt> should be entered.
</p>


<h3>Getting Help</h3>

<p>
Documentation for the <i>lpSolve</i> and <i>lpSolveAPI</i> packages is provided using R's built-in help system.  For example, the command

<pre>
  &gt; ?make.lp
</pre>

will display the documentation for the <tt>make.lp</tt> function.  You can list all of the functions in the <i>lpSolveAPI</i> package with the following command.

<pre>
  &gt; ls("package:lpSolveAPI")
</pre>

The documentation for each of these functions can be accessed using the <tt>?</tt> operator. Note that you must append <tt>.lpExtPtr</tt> to the names of the generic functions (<tt>dim</tt>, <tt>dimnames</tt>, <tt>plot</tt>, <tt>print</tt> and <tt>solve</tt>), otherwise you will get the documentation for the standard generic function.

</p>

<h3>Building and Solving Linear Programs Using the lpSolveAPI Package</h3>

<p>
The lpSolveAPI package provides an API for building and solving linear programs that mimics the lp_solve C API.  This approach allows  greater flexibility but also has a few caveats.  The most important is that the <i>lpSolve linear program model objects</i> created by <tt>make.lp</tt> and <tt>read.lp</tt> are not actually R objects.  Rather, they are pointers to lp_solve 'lprec' structures which are created and store externally.  R does not know how to deal with these structures.  In particular, R cannot duplicate them.  You should never assign an lpSolve linear program model object in R code.
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

<h3>A brief Example</h3>

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
  Upper          Inf       Inf       Inf     48.98          
  Lower         28.6         0         0        18          
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

</body>
</html>

# A50 - compare genome assemblies

a50 is a tool for comparing genome assemblies, providing a bit more
information than the usual numeric statistics, like N50.  For a quick
overview of the options, use 'a50 --help'.


## General usage

To compare assemblies, you need two or more fasta-formatted files
containing contigs.  Running 'a50' with the files as arguments
produces a plot with one curve per input.  On the x-axis are the
contigs, ordered by size, and on the y-axis is the corresponding
cumulative size.  Generally, a better assembly has a steeper curve
(big contigs) which ends early (few contigs) near the y-value
corresponding to the expected genome size.

The plot is generated using 'gnuplot', so the gnuplot executable must
be in your $PATH.  You can pass various information to gnuplot on the
commandline, specifically you can use -f to specify output format
('terminal' in gnuplot lingo), -o to specify an output file, and -e to
produce horizontal lines, e.g. at the expected genome size.

For example:

        a50 -t pdf -o asm.pdf asm1.fasta asm2.fasta -e 8e+8

will plot the assemblies given in asm1.fasta and asm2.fasta against an
expected genome size of 800Mb in PDF format to the file asm.pdf.  If
no output or format is specified, gnuplot will display the graph in a
window.  If an output file, but no format is specified, a50 will try
to determine the format from the file name extension.

##  Using EST or transcripts as a reference

A similar way to compare assemblies is to measure the gene coverage of
each contig.  To do this, you need to have the genes available,
typically in the form of raw or assembled ESTs.  By specifying a set
of transcripts using the -E option, e.g:

        a50 asm.fasta asm2.fasta -E ests.fasta

will plot curves with contigs of the assemblies ordered by size along
the x-axis as before, but the y-value will be the total amount of
transcript data mapped to the contigs.

The mapping process runs BLAT, and stores the resulting PSL files in a
tempdir, typically /tmp, but overriden with the $TMPDIR variable or
specified with -T.  Only the best hit for each EST is retained, so
exons in other contigs are not counted.

## Comparison to other measures

You can read various other measures off the graph fairly easily.
Total assembly size is the y-value at which the curve ends, and the
number of contigs in the assembly is the corresponding x-value.  An
assembly with greater average contig size will end to the left and/or
above of an assembly with shorter average contigs.  N50 is the slope
of the graph at the y-value corresponding to half the genome size.

#!/bin/bash
K=$1
N=$2
echo """
<?xml-stylesheet type=\"text/xsl\" href=\"./golden.xsl\"?>
<inputs>
    <k>$K</k>
    <n>$N</n>
</inputs>
""" | xsltproc -

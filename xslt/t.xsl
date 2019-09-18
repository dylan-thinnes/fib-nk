<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" omit-xml-declaration="yes"/>
<xsl:param name="orig_list" select="'0 1 1'"/>
<xsl:template name="sum">
    <xsl:param name="accumulator" select="0"/>
    <xsl:param name="list" select="'0'"/>

    <xsl:param name="head" select="number(substring-before($list, ' '))"/>
    <xsl:param name="tail" select="substring-after($list, ' ')"/>

    <xsl:choose>
        <xsl:when test="not($head = $head)">
            <xsl:value-of select="$accumulator + number($list)"/>
        </xsl:when>
        <xsl:otherwise>
            <xsl:call-template name="sum">
                <xsl:with-param name="accumulator" select="$accumulator + $head"/>
                <xsl:with-param name="list" select="$tail"/>
            </xsl:call-template>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>
<xsl:template name="find_cycle" match="/">
    <xsl:param name="xs" select="$orig_list"/>
    <xsl:param name="prime">
        <xsl:call-template name="sum">
            <xsl:with-param name="accumulator" select="0"/>
            <xsl:with-param name="list" select="$xs"/>
        </xsl:call-template>
    </xsl:param>
    <xsl:param name="prime_mod" select="$prime mod 10"/>
    <xsl:param name="new_list" select="concat(substring-after($xs,' '),' ',string($prime_mod))"/>

    <xsl:choose>
        <xsl:when test="$new_list = $orig_list">
            <xsl:value-of select="substring-before($xs,' ')"/>
            <xsl:text>&#xa;</xsl:text>
        </xsl:when>
        <xsl:otherwise>
            <xsl:value-of select="substring-before($xs,' ')"/>
            <xsl:text>&#xa;</xsl:text>
            <xsl:call-template name="find_cycle">
                <xsl:with-param name="xs" select="$new_list"/>
            </xsl:call-template>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>
</xsl:stylesheet>

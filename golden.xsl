<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" omit-xml-declaration="yes"/>

<xsl:variable name="n" select="/inputs/n"/>
<xsl:variable name="k" select="/inputs/k"/>
<xsl:template match="/">
    <!--
    <xsl:call-template name="increment_seed">
        <xsl:with-param name="k" select="/inputs/k"/>
        <xsl:with-param name="seed" select="'0 0 9'"/>
    </xsl:call-template>
    -->
    <xsl:call-template name="find_all_cycles">
        <xsl:with-param name="k" select="/inputs/k"/>
        <xsl:with-param name="seed" select="'0 0 0'"/>
    </xsl:call-template>
</xsl:template>

<!-- Sum function over string lists -->
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

<xsl:template name="generate_zeros">
    <xsl:param name="k"/>

    <xsl:choose>
        <xsl:when test="$k = 1">
            <xsl:text>0</xsl:text>
        </xsl:when>
        <xsl:otherwise>
            <xsl:text>0 </xsl:text>
            <xsl:call-template name="generate_zeros">
                <xsl:with-param name="k" select="$k - 1"/>
            </xsl:call-template>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<xsl:template name="increment_seed">
    <xsl:param name="seed"/>

    <xsl:variable name="result">
        <xsl:call-template name="increment_seed_main">
            <xsl:with-param name="seed" select="$seed"/>
        </xsl:call-template>
    </xsl:variable>

    <xsl:value-of select="substring($result,3)"/>
</xsl:template>

<xsl:template name="increment_seed_main">
    <xsl:param name="seed"/>

    <xsl:param name="tmp_a" select="number(substring-before($seed, ' '))"/>
    <xsl:param name="tail"  select="substring-after($seed, ' ')"/>

    <xsl:choose>
        <xsl:when test="not($tmp_a = $tmp_a)">
            <xsl:variable name="a" select="number($seed)"/>
            <xsl:value-of select="concat(string(number($a = ($n - 1))),',',string(($a + 1) mod $n))"/>
        </xsl:when>
        <xsl:otherwise>
            <xsl:variable name="a" select="$tmp_a"/>
            <xsl:variable name="lower_res">
                <xsl:call-template name="increment_seed_main">
                    <xsl:with-param name="seed" select="$tail"/>
                </xsl:call-template>
            </xsl:variable>

            <xsl:variable name="carry" select="substring-before($lower_res, ',')"/>
            <xsl:variable name="lower_list" select="substring-after($lower_res, ',')"/>

            <xsl:choose>
                <xsl:when test="$carry = '1'">
                    <xsl:value-of select="concat(string(number($a = ($n - 1))), ',', string(($a + 1) mod $n), ' ', $lower_list)"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="concat('0', ',', string($a), ' ', $lower_list)"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<!-- Find a cycle given a starting seed -->
<xsl:template name="find_cycle">
    <xsl:param name="orig_seed"/>
    <xsl:param name="xs" select="$orig_seed"/>

    <xsl:param name="xs_sum">
        <xsl:call-template name="sum">
            <xsl:with-param name="accumulator" select="0"/>
            <xsl:with-param name="list" select="$xs"/>
        </xsl:call-template>
    </xsl:param>
    <xsl:param name="x_prime" select="$xs_sum mod $n"/>
    <xsl:param name="new_list" select="concat(substring-after($xs,' '),' ',string($x_prime))"/>

    <xsl:choose>
        <xsl:when test="$new_list = $orig_seed">
            <!-- <xsl:value-of select="substring-before($xs,' ')"/> -->
            <xsl:value-of select="$xs"/>
            <xsl:text>&#xa;</xsl:text>
        </xsl:when>
        <xsl:otherwise>
            <xsl:value-of select="substring-before($xs,' ')"/>
            <xsl:text> </xsl:text>
            <xsl:call-template name="find_cycle">
                <xsl:with-param name="orig_seed" select="$orig_seed"/>
                <xsl:with-param name="xs" select="$new_list"/>
            </xsl:call-template>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<xsl:template name="find_all_cycles">
    <xsl:param name="k"/>
    <xsl:variable name="zeros">
        <xsl:call-template name="generate_zeros">
            <xsl:with-param name="k" select="$k"/>
        </xsl:call-template>
    </xsl:variable>

    <xsl:call-template name="find_all_cycles_main">
        <xsl:with-param name="seed" select="$zeros"/>
    </xsl:call-template>
</xsl:template>

<xsl:template name="find_all_cycles_main">
    <xsl:param name="seed"/>
    <xsl:param name="already_found" select="''"/>

    <xsl:variable name="new_cycle">
        <xsl:call-template name="find_cycle">
            <xsl:with-param name="orig_seed" select="$seed"/>
        </xsl:call-template>
    </xsl:variable>

    <xsl:variable name="new_already_found" select="concat($new_cycle, $already_found)"/>

    <xsl:variable name="new_seed">
        <xsl:call-template name="next_seed">
            <xsl:with-param name="candidate_seed" select="$seed"/>
            <xsl:with-param name="already_found" select="$new_already_found"/>
        </xsl:call-template>
    </xsl:variable>

    <xsl:choose>
        <xsl:when test="$new_seed = 'false'">
            <xsl:value-of select="$new_already_found"/>
        </xsl:when>
        <xsl:otherwise>
            <xsl:call-template name="find_all_cycles_main">
                <xsl:with-param name="seed" select="$new_seed"/>
                <xsl:with-param name="already_found" select="$new_already_found"/>
            </xsl:call-template>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<xsl:template name="next_seed">
    <xsl:param name="candidate_seed"/>
    <xsl:param name="already_found"/>

    <xsl:variable name="seed_sum">
        <xsl:call-template name="sum">
            <xsl:with-param name="list" select="$candidate_seed"/>
        </xsl:call-template>
    </xsl:variable>

    <xsl:choose>
        <xsl:when test="$seed_sum = ($n * $k - $k)">
            <xsl:text>false</xsl:text>
        </xsl:when>
        <xsl:when test="contains($already_found, $candidate_seed)">
            <xsl:variable name="new_candidate">
                <xsl:call-template name="increment_seed">
                    <xsl:with-param name="seed" select="$candidate_seed"/>
                </xsl:call-template>
            </xsl:variable>

            <xsl:call-template name="next_seed">
                <xsl:with-param name="candidate_seed" select="$new_candidate"/>
                <xsl:with-param name="already_found" select="$already_found"/>
            </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
            <xsl:value-of select="$candidate_seed"/>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>
</xsl:stylesheet>

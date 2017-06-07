<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:r="http://www.r-project.org">
  <xsl:output method="html" indent="yes"/>

  <xsl:include href="common.xsl"/>

  <xsl:template name="echo-attr">
    <xsl:choose>
      <xsl:when test="name() = 'id'">
        <xsl:value-of select="current()"/>   
      </xsl:when>
      <xsl:when 
          test="name() = 'echo' or 
                name() = 'eval' or 
                name() = 'fig.width' or 
                name() = 'fig.height' or 
                name() = 'warning' or
                name() = 'include' or
                name() = 'message'">
        <xsl:value-of select="name()"/>=<xsl:value-of select="current()"/>   
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="name()"/>="<xsl:value-of select="current()"/><xsl:text>"</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="position() != last()">
      <xsl:text>, </xsl:text>
    </xsl:if>    
  </xsl:template>

  <xsl:template match="//rcode">
    <xsl:comment>begin.rcode <xsl:for-each select="@*">
      <xsl:call-template name="echo-attr"/>
    </xsl:for-each>
    <xsl:apply-templates/>end.rcode</xsl:comment>
  </xsl:template>

</xsl:stylesheet>

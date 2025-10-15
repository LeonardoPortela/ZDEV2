*&---------------------------------------------------------------------*
*& Include          ZMMR189_DADOS
*&---------------------------------------------------------------------*

FORM select_data.


ENDFORM.

FORM seleciona_dados_p21.
  SELECT * FROM zfit0201 INTO TABLE it_dados_p21
  WHERE cd_indic IS NOT NULL.

  SORT it_dados_p21 BY cd_indic ASCENDING.

  LOOP AT it_dados_p21 ASSIGNING FIELD-SYMBOL(<it_dados_p21>).
    <it_dados_p21>-itens = icon_change.
    <it_dados_p21>-ccustos = icon_change.
    <it_dados_p21>-depara = icon_change.
    <it_dados_p21>-relatorio = icon_view_list.
    <it_dados_p21>-EXPORT = icon_xls.
*    <it_dados_p21>-api = icon_add_row.
*    <it_dados_p21>-enviados = icon_view_list.
    <it_dados_p21>-grupos = icon_change.
    <it_dados_p21>-clcustos = icon_change.
    <it_dados_p21>-ano = p_ano.
  ENDLOOP.

ENDFORM.

FORM get_dados_mseg .

  CLEAR: l_budat1, l_budat2,l_tcode,l_kokrs.

  l_budat1 = |{ p_ano }0101|.
  l_budat2 = |{ p_ano }1231|.
  l_tcode = 'KSB1'.
  l_kokrs = 'MAGI'.

  CLEAR: resultado, w_resultado.

SELECT
  DISTINCT
  ",aa~matkl
   a~matnr
  ,a~werks
  ,a~bukrs
  ,a~mjahr
  ,a~sakto AS kstar
  ,aaaa~kostl AS KOSTL
  ,a~meins AS meinb
  ,a~menge AS mbgbtr
  ,a~budat_mkpf AS budat
  ,aaa~maktx AS ebtxt
  ",'261' as tipo
  FROM mseg  AS a
  LEFT JOIN mara AS aa ON a~matnr = aa~matnr
  LEFT JOIN makt AS aaa ON a~matnr = aaa~matnr
  LEFT JOIN caufv AS aaaa ON a~aufnr = aaaa~aufnr
  WHERE 1 = 1
  AND LTRIM( a~sakto,'0' ) IN ( SELECT DISTINCT LTRIM( kstar,'0' ) AS kstar FROM zfit0207 WHERE cd_indic = @vlr_indic )
  AND LTRIM( aaaa~kostl,'0' ) IN ( SELECT DISTINCT LTRIM( kostl,'0' ) AS KOSTL FROM zfit0203 WHERE cd_indic = @vlr_indic )
  AND LTRIM( aa~matkl,'0' ) IN ( SELECT DISTINCT LTRIM( matkl,'0' ) AS matkl FROM zfit0206 WHERE cd_indic = @vlr_indic )
  AND a~matnr IN ( SELECT DISTINCT matnr FROM zfit0202 WHERE cd_indic = @vlr_indic )
  AND a~mjahr = @p_ano
  AND a~budat_mkpf BETWEEN @l_budat1 AND @l_budat2
  AND a~kokrs = @l_kokrs
  AND a~bwart = '261'
  AND a~mblnr NOT IN (
  SELECT DISTINCT b~smbln FROM mseg AS b
  WHERE 1 = 1
  AND b~smbln = a~mblnr
  AND b~bwart = '262' )
  INTO TABLE @DATA(resultado_261)."@resultado.

  APPEND LINES OF resultado_261 TO resultado.


  SELECT
  DISTINCT
  ",aa~matkl
  a~matnr
  ,a~werks
  ,a~bukrs
  ,a~mjahr
  ,a~sakto AS kstar
  ,a~kostl
  ,a~meins AS meinb
  ,a~menge AS mbgbtr
  ,a~budat_mkpf AS budat
  ,aaa~maktx AS ebtxt
  ",'201' as tipo
  FROM mseg  AS a
  LEFT JOIN mara AS aa ON a~matnr = aa~matnr
  LEFT JOIN makt AS aaa ON a~matnr = aaa~matnr
  LEFT JOIN caufv AS aaaa ON a~aufnr = aaaa~aufnr
  WHERE 1 = 1
  AND LTRIM( a~sakto,'0' ) IN ( SELECT DISTINCT LTRIM( kstar,'0' ) AS kstar FROM zfit0207 WHERE cd_indic = @vlr_indic )
  AND LTRIM( A~kostl,'0' ) IN ( SELECT DISTINCT LTRIM( kostl,'0' ) AS KOSTL FROM zfit0203 WHERE cd_indic = @vlr_indic )
  AND LTRIM( aa~matkl,'0' ) IN ( SELECT DISTINCT LTRIM( matkl,'0' ) AS matkl FROM zfit0206 WHERE cd_indic = @vlr_indic )
  AND a~matnr IN ( SELECT DISTINCT matnr FROM zfit0202 WHERE cd_indic = @vlr_indic )
  AND a~mjahr = @p_ano
  AND a~budat_mkpf BETWEEN @l_budat1 AND @l_budat2
  AND a~kokrs = @l_kokrs
  AND a~bwart = '201'
  AND a~mblnr NOT IN (
  SELECT DISTINCT b~smbln FROM mseg AS b
  WHERE 1 = 1
  AND b~smbln = a~mblnr
  AND b~bwart = '202')
  INTO TABLE @DATA(resultado_201)."@resultado.

  APPEND LINES OF resultado_201 TO resultado.

  SELECT
  DISTINCT
  ",aa~matkl
   a~matnr
  ,a~werks
  ,a~bukrs
  ,a~mjahr
  ,a~sakto AS kstar
  ,a~kostl
  ,a~meins AS meinb
  ,a~menge AS mbgbtr
  ,a~budat_mkpf AS budat
  ,aaa~maktx AS ebtxt
  ",'101' as tipo
  FROM mseg  AS a
  LEFT JOIN mara AS aa ON a~matnr = aa~matnr
  LEFT JOIN makt AS aaa ON a~matnr = aaa~matnr
  LEFT JOIN caufv AS aaaa ON a~aufnr = aaaa~aufnr
  WHERE 1 = 1
  AND LTRIM( a~sakto,'0' ) IN ( SELECT DISTINCT LTRIM( kstar,'0' ) AS kstar FROM zfit0207 WHERE cd_indic = @vlr_indic )
  AND LTRIM( A~kostl,'0' ) IN ( SELECT DISTINCT LTRIM( kostl,'0' ) AS KOSTL FROM zfit0203 WHERE cd_indic = @vlr_indic )
  AND LTRIM( aa~matkl,'0' ) IN ( SELECT DISTINCT LTRIM( matkl,'0' ) AS matkl FROM zfit0206 WHERE cd_indic = @vlr_indic )
  AND a~matnr IN ( SELECT DISTINCT matnr FROM zfit0202 WHERE cd_indic = @vlr_indic )
  AND a~mjahr = @p_ano
  AND a~budat_mkpf BETWEEN @l_budat1 AND @l_budat2
  AND a~kokrs = @l_kokrs
  AND a~bwart = '101'
  AND a~mblnr NOT IN (
  SELECT DISTINCT b~smbln FROM mseg AS b
  WHERE 1 = 1
  AND b~smbln = a~mblnr
  AND b~bwart = '102' )
  AND A~kostl <> ''
  INTO TABLE @DATA(resultado_101)."@resultado.

  APPEND LINES OF resultado_101 TO resultado.

ENDFORM.

*----------------------------------------------------------------------*
***INCLUDE LZ_FIF01.
*----------------------------------------------------------------------*

TYPES: BEGIN OF ty_saida,
         bukrs             TYPE bsad-bukrs,
         gsber             TYPE bsad-gsber,
         kunnr             TYPE bsad-kunnr,
         name1             TYPE kna1-name1,
         tipo              TYPE c LENGTH 13,
         belnr             TYPE bsad-belnr,
         augbl             TYPE bsad-augbl,
         budat             TYPE bsad-budat,
         augdt             TYPE bsad-augdt,
         vbel2             TYPE bsad-vbel2,
         auart             TYPE vbak-auart,
         vbeln             TYPE bsad-vbeln,
         nr_sol            TYPE zsdt0053-nro_sol_ov,
         tp_venda          TYPE zsdt0051-tp_venda,
         dmbtr             TYPE bsad-dmbtr,
         dmbe2             TYPE bsad-dmbe2,
         tx_camb           TYPE zlest0061-tax_dolar,
         banco_liq         TYPE skat-txt50,
         zfbdt             TYPE bsad-zfbdt,
         butxt             TYPE t001-butxt,
         belnr_bx          TYPE bsad-belnr,
         budat_bx          TYPE bsad-budat,
         augdt_bx          TYPE bsad-augdt,
         dmbtr_bx          TYPE bsad-dmbtr,
         dmbe2_bx          TYPE bsad-dmbe2,
         vbel2_bx          TYPE bsad-vbel2,
         vbeln_bx          TYPE bsad-vbeln,
         matnr             TYPE mara-matnr,
         maktx             TYPE makt-maktx,
         buzei             TYPE bsad-buzei,
         charg             TYPE vbap-charg,
         waers             TYPE bsad-waers,
         tpsim             TYPE char2,
         spart             TYPE vbak-spart,
         jr_ds             TYPE c,
         matkl             TYPE mara-matkl,
         ktokd             TYPE kna1-ktokd,
         ajuste_financeiro TYPE zfit0087-ajuste_financeiro, "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
       END OF ty_saida.

FORM get_vlr_dolar USING p_origem TYPE zchar02
                         p_opcao  TYPE zchar02
                CHANGING p_saida TYPE ty_saida.

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.

  DATA: vl_tx_cambio TYPE ukurs_curr,
        vl_gdatu     TYPE gdatu_inv.

  CREATE OBJECT obj_zcl_util_sd.

  CHECK p_saida-augdt IS NOT INITIAL.

  "Busca Taxa Data Compensação
  CLEAR: vl_tx_cambio.

  IF p_opcao = 'TR' AND p_saida-ajuste_financeiro IS INITIAL. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
    MOVE p_saida-budat TO vl_gdatu.
  ELSE.
    MOVE p_saida-augdt TO vl_gdatu.
  ENDIF.

  obj_zcl_util_sd->set_kurst('B').
  obj_zcl_util_sd->set_waerk('USD').
  obj_zcl_util_sd->set_tcurr('BRL').
  obj_zcl_util_sd->set_data( vl_gdatu ).

  vl_tx_cambio = abs( obj_zcl_util_sd->taxa_cambio( ) ).

  IF vl_tx_cambio > 0.

    "IF P_SAIDA-WAERS = 'USD'.
    "  P_SAIDA-DMBTR = P_SAIDA-DMBE2 * VL_TX_CAMBIO.
    "ELSE.
    p_saida-dmbe2 = p_saida-dmbtr / vl_tx_cambio.
    "ENDIF.

    CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
      p_saida-tx_camb = vl_tx_cambio.
    ENDCATCH.

  ENDIF.



ENDFORM.

FORM f_get_auart_tipo TABLES t_tvak STRUCTURE tvak
                       USING p_tipo.

  DATA: lit_zfit0221 TYPE TABLE OF zfit0221. "FI - ZFI0064 - Transação Parametros US #149772 - WPP -->>>

  CLEAR: t_tvak[].

  DATA: wl_tvak TYPE tvak,
        t_value TYPE rgsb4 OCCURS 0 WITH HEADER LINE.

  "FI - ZFI0064 - Transação Parametros US #149772 - WPP -->>>
  CLEAR: lit_zfit0221[].
  IF p_tipo IS NOT INITIAL.
    SELECT *
      FROM zfit0221 INTO TABLE lit_zfit0221
      WHERE operacao EQ p_tipo.
  ELSE.
    SELECT *
      FROM zfit0221 INTO TABLE lit_zfit0221.
  ENDIF.

  DELETE lit_zfit0221 WHERE active EQ abap_false.

  LOOP AT lit_zfit0221 INTO DATA(lwa_zfit0221).
    APPEND INITIAL LINE TO t_tvak ASSIGNING FIELD-SYMBOL(<fs_tvak>).
    <fs_tvak>-auart = lwa_zfit0221-auart.
  ENDLOOP.

*   CASE p_tipo.
*    WHEN 'MI'. "Mercado Interno
*
*      PERFORM f_get_auart TABLES t_tvak USING 'ZFI0064_AUART_MI'.
*
**      wl_tvak-auart    = 'ZCOP'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZCPV'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZMIT'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZREB'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZTRI'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZFEX'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZFUT'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZREM'. APPEND wl_tvak TO t_tvak.
*
*    WHEN 'IN'. "Insumos
*
*      PERFORM f_get_auart TABLES t_tvak USING 'ZFI0064_AUART_IN'.
*
**      wl_tvak-auart    = 'ZDEF'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZSEM'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZFTE'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZOSM'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZODF'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZOFE'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZFUT'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZREM'. APPEND wl_tvak TO t_tvak.
*
*    WHEN 'AQ'. "Frete Aquav./Transb./Serv.Port.-Hermasa
*
*      PERFORM f_get_auart TABLES t_tvak USING 'ZFI0064_AUART_AQ'.
*
**      wl_tvak-auart    = 'ZTAG'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZTRG'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZTRF'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZPOR'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZTAF'. APPEND wl_tvak TO t_tvak.
*
*    WHEN 'PA'. "Portochuelo-Amaggi
*
*      PERFORM f_get_auart TABLES t_tvak USING 'ZFI0064_AUART_PA'.
**      wl_tvak-auart    = 'ZTRG'. APPEND wl_tvak TO t_tvak.
*
*    WHEN OTHERS.
*
*      PERFORM f_get_auart TABLES t_tvak USING 'ZFI0064_AUART_MI'.
*      PERFORM f_get_auart TABLES t_tvak USING 'ZFI0064_AUART_IN'.
*      PERFORM f_get_auart TABLES t_tvak USING 'ZFI0064_AUART_AQ'.
*      PERFORM f_get_auart TABLES t_tvak USING 'ZFI0064_AUART_PA'.
*
**      wl_tvak-auart    = 'ZCOP'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZCPV'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZMIT'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZREB'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZOFE'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZODF'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZREM'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZTRI'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZFEX'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZDEF'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZSEM'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZFTE'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZOSM'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZFUT'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZTAG'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZTRG'. APPEND wl_tvak TO t_tvak.
**      wl_tvak-auart    = 'ZTRF'. APPEND wl_tvak TO t_tvak.
*
*  ENDCASE.


  "FI - ZFI0064 - Transação Parametros US #149772 - WPP -->>>




ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_AUART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_TVAK  text
*      -->P_0280   text
*----------------------------------------------------------------------*
FORM f_get_auart  TABLES   p_tvak STRUCTURE tvak
                  USING    set.

  DATA: wl_tvak TYPE tvak,
        t_value TYPE rgsb4 OCCURS 0 WITH HEADER LINE.

  FREE: t_value.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr           = set
      class           = '0000'
      no_descriptions = ''
    TABLES
      set_values      = t_value
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  LOOP AT t_value.
    wl_tvak-auart  = t_value-from.
    APPEND wl_tvak TO p_tvak.
  ENDLOOP.

ENDFORM.



FORM f_email_aviso_param TABLES p_saida STRUCTURE gt_saida_tmp
                          USING p_tipo TYPE string
                                p_operacao TYPE char02.

  DATA: objpack     LIKE sopcklsti1 OCCURS  2 WITH HEADER LINE.
  DATA: objhead     LIKE solisti1   OCCURS  1 WITH HEADER LINE.
  DATA: objbin_ord  LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: objbin_log  LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: objbin_ann  TYPE solisti1.
  DATA: objbin    LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        wa_objbin LIKE LINE OF objbin.
  DATA: content_hex TYPE STANDARD TABLE OF solix WITH HEADER LINE.
  DATA: objtxt      LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: reclist     LIKE somlreci1  OCCURS  5 WITH HEADER LINE.
  DATA: doc_chng    LIKE sodocchgi1.
  DATA: tab_lines   LIKE sy-tabix.
  DATA: it_html     TYPE TABLE OF w3html INITIAL SIZE 0 WITH HEADER LINE.

  DATA: vl_titulo   TYPE string.
  DATA: vl_tipo     TYPE string.
  DATA: lva_value_html TYPE string.

  DEFINE conc_html.

    lva_value_html = &1.

    CALL FUNCTION 'ZHTML_ADD'
      EXPORTING
        i_texto = lva_value_html
      TABLES
        it_html = it_html.

  END-OF-DEFINITION.


  DATA(lit_saida) = p_saida[].
  DELETE lit_saida WHERE augdt EQ sy-datum OR budat EQ sy-datum.

  CHECK lit_saida[] IS NOT INITIAL.

  CHECK sy-batch eq abap_true.

  SELECT  *
    FROM zfit0087 INTO TABLE @DATA(lit_zfit0087)
     FOR ALL ENTRIES IN @lit_saida
   WHERE bukrs EQ @lit_saida-bukrs
     AND belnr EQ @lit_saida-belnr.

  SORT lit_zfit0087 BY bukrs belnr.

  LOOP AT lit_saida ASSIGNING FIELD-SYMBOL(<fs_saida_tmp>).
    READ TABLE lit_zfit0087 WITH KEY bukrs = <fs_saida_tmp>-bukrs
                                     belnr = <fs_saida_tmp>-belnr BINARY SEARCH TRANSPORTING NO FIELDS.
    CHECK sy-subrc EQ 0.

    CLEAR: <fs_saida_tmp>-belnr.
  ENDLOOP.

  DELETE lit_saida WHERE belnr IS INITIAL.

  CHECK lit_saida[] IS NOT INITIAL.

  SELECT *
    FROM zmail INTO TABLE @DATA(lit_email_alert)
   WHERE tcode   EQ 'ZFI0064_ALERTA'.

  CHECK lit_email_alert[] IS NOT INITIAL.

  "US 175263 - WPP Ajuste Disparo Email Alerta - Ini
  CHECK ( sy-uzeit GE '040000' AND sy-uzeit LE  '043000' ) OR ( sy-uzeit GE '160000' AND sy-uzeit LE  '163000' ). "Só enviar emails nesses horarios do dia...
  "US 175263 - WPP Ajuste Disparo Email Alerta - Fim

  LOOP AT lit_email_alert INTO DATA(lwa_email).
    TRANSLATE lwa_email-email TO LOWER CASE.
    reclist-receiver = lwa_email-email.
    reclist-rec_type = 'U'.
    APPEND reclist.
  ENDLOOP.

  CASE p_tipo.
    WHEN 'AUART'.
      vl_titulo = 'ZFI0064 - Parametrização Lançamentos - Tipo OV'.
      vl_tipo  = 'ZFI0064 - Lançamentos sem parametrização por Tipo de OV'.
    WHEN 'MATKL'.
      vl_titulo = 'ZFI0064 - Parametrização Lançamentos - Grupo Mercadoria'.
      vl_tipo  = 'ZFI0064 - Lançamentos sem parametrização por grupo de Mercadoria'.
  ENDCASE.

  "Monta Corpo Email
  conc_html '<html>'.
  conc_html '<head><title>'.
  conc_html     vl_tipo.
  conc_html '  </title><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1"></head>'.
  conc_html '<body bgcolor="#f5f1ff" leftmargin="0" topmargin="0" marginwidth="0" marginheight="0">'.
  conc_html '<DIV align=center><FONT face=Verdana color=#ff0000 size=4><STRONG>'.
  conc_html     vl_titulo.
  conc_html '</STRONG></FONT></DIV><BR>'.
  conc_html '<FONT face=Verdana color=#0000ff size=2>'.
  conc_html '<BR>'.

  conc_html '<table cellspacing="0" border="1" bordercolor="FFFFFF" width="100%">'.

  conc_html    '<tr bgcolor="B0C4DE">'.
  conc_html       '<td width="20%"><p align="center"><font color="#000030" size=1><b>Doc. Contabil</b></font></p></td>'.
  conc_html       '<td width="20%"><p align="center"><font color="#000030" size=1><b>Ano. Doc.</b></font></p></td>'.
  conc_html       '<td width="20%"><p align="center"><font color="#000030" size=1><b>Empresa</b></font></p></td>'.
  conc_html       '<td width="20%"><p align="center"><font color="#000030" size=1><b>Operação</b></font></p></td>'.

  CASE p_tipo.
    WHEN 'AUART'.
      conc_html       '<td width="30%"><p align="center"><font color="#000030" size=1><b>Tipo OV</b></font></p></td>'.
    WHEN 'MATKL'.
      conc_html       '<td width="30%"><p align="center"><font color="#000030" size=1><b>Grupo Mercadoria</b></font></p></td>'.
  ENDCASE.

  conc_html    '</tr>'.

  LOOP AT lit_saida INTO DATA(lwa_saida).

    conc_html  '<tr bordercolor="black">'.

    conc_html     '<td width="20%"><p align="left"> <font color="000" size=1><b>'.
    conc_html        lwa_saida-belnr.
    conc_html     '</b></font></p></td>'.

    conc_html     '<td width="20%"><p align="left"> <font color="000" size=1><b>'.
    conc_html        lwa_saida-budat(4).
    conc_html     '</b></font></p></td>'.

    conc_html     '<td width="20%"><p align="left"> <font color="000" size=1><b>'.
    conc_html        lwa_saida-bukrs.
    conc_html     '</b></font></p></td>'.

    conc_html     '<td width="20%"><p align="left"> <font color="000" size=1><b>'.
    conc_html        p_operacao.
    conc_html     '</b></font></p></td>'.

    conc_html     '<td width="30%"><p align="left"> <font color="000" size=1><b>'.

    CASE p_tipo.
      WHEN 'AUART'.
        conc_html        lwa_saida-auart.
      WHEN 'MATKL'.
        conc_html        lwa_saida-matkl.
    ENDCASE.

    conc_html     '</b></font></p></td>'.

    conc_html     '</b></font></p></td>'.
    conc_html  '</tr>'.

  ENDLOOP.

  conc_html '</table>'.

  conc_html '<BR>'.
  conc_html '<BR>'.
  conc_html '<DIV align=left>'.

  conc_html '<DIV align=center><FONT face=Verdana color=#ffaaaa size=1><STRONG>E-mail gerado automáticamente pelo sistema</STRONG></FONT></DIV>'.

  conc_html '</DIV>'.
  conc_html '<BR>'.
  conc_html '</body>'.
  conc_html '</html>'.

  "Corpo
  doc_chng-obj_name = 'ZFI0064 - Parametrização Lançamentos'.
  doc_chng-obj_descr = 'Aviso de Parametrização Lançamentos - ZFI0064'.
  doc_chng-no_change = 'X'.

  CLEAR objpack-transf_bin.
  objpack-head_start = 1.
  objpack-head_num = 0.
  objpack-body_start = 1.
  objpack-body_num = 99999.
  objpack-doc_type = 'HTM'.
  APPEND objpack.

  "Enviar
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = doc_chng
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      packing_list               = objpack
      contents_txt               = it_html
      receivers                  = reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      operation_no_authorization = 4
      OTHERS                     = 99.


ENDFORM.                    " EMAIL_AVISO_ENC

"FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
FORM f_converte_lcto_ajuste_zfis26 USING p_bukrs TYPE bsad-bukrs
                                         p_belnr TYPE bsad-belnr
                                         p_waers TYPE bsad-waers
                                         p_vbel2 TYPE bsad-vbel2
                                CHANGING c_dmbtr TYPE bsad-dmbtr
                                         c_dmbe2 TYPE bsad-dmbe2
                                         c_ok   TYPE c.

  DATA: lva_tx_camb_aux  TYPE zlest0061-tax_dolar.

  c_ok = abap_false.

  CHECK r_zterm_ajuste_finan[] IS NOT INITIAL.

  CHECK ( p_waers EQ 'BRL' OR p_waers EQ 'USD' ) .

  CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
    lva_tx_camb_aux = abs( c_dmbtr / c_dmbe2 ).
  ENDCATCH.

  CHECK lva_tx_camb_aux IS NOT INITIAL.

  SELECT SUM( mont_rbdo ) INTO @DATA(lva_montante_rec)
    FROM zfit0026
   WHERE bukrs  EQ @p_bukrs
     AND docnum EQ @p_belnr
     AND zterm  IN @r_zterm_ajuste_finan
     AND vbeln  EQ @p_vbel2.

  CHECK ( sy-subrc EQ 0 ) AND lva_montante_rec IS NOT INITIAL.

  CASE p_waers.
    WHEN 'BRL'.
      c_dmbtr = lva_montante_rec.
      c_dmbe2 = lva_montante_rec / lva_tx_camb_aux.
    WHEN 'USD'.
      c_dmbe2 = lva_montante_rec.
      c_dmbtr = lva_montante_rec * lva_tx_camb_aux.
  ENDCASE.

  c_ok = abap_true.

ENDFORM.
"FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<<-----

"FI - ZFI0064 - Transação Parametros US #149772 - WPP  --->>>
FORM f_descartar_lancamentos TABLES p_saida STRUCTURE gt_saida_tmp
                                    p_zfit0087 STRUCTURE zfit0087
                              USING p_tipo TYPE char02.

  DATA: v_aquav_ini TYPE bsak-augdt.

  RANGES  r_bukrs_tmp FOR zfit0087-bukrs.
  RANGES: r_matkl_graos FOR mara-matkl.

  CLEAR: r_matkl_graos[].

  APPEND VALUE #( sign = 'I' option = 'EQ' low = '700110' ) TO r_matkl_graos.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = '700170' ) TO r_matkl_graos.

  DELETE p_saida    WHERE bukrs = '0032' AND ( ktokd EQ 'ZCIC' OR matkl NOT IN r_matkl_graos ).
  DELETE p_zfit0087 WHERE bukrs = '0032' AND ( ktokd EQ 'ZCIC' OR matkl NOT IN r_matkl_graos ).

  CASE p_tipo.
    WHEN 'PA' OR 'AQ'.

      SELECT SINGLE *
        FROM setleaf INTO @DATA(_wl_zfi0064_ini_aquav)
       WHERE setname = 'ZFI0064_INI_AQUAV_2'.

      IF ( sy-subrc EQ 0 ) AND ( _wl_zfi0064_ini_aquav-valfrom IS NOT INITIAL ).
        v_aquav_ini = _wl_zfi0064_ini_aquav-valfrom.

        LOOP AT p_zfit0087 INTO DATA(gw_zfit0087) WHERE augdt < v_aquav_ini.
          DELETE FROM zfit0087 WHERE belnr   = gw_zfit0087-belnr
                                 AND buzei   = gw_zfit0087-buzei
                                 AND bukrs   = gw_zfit0087-bukrs
                                 AND augbl   = gw_zfit0087-augbl.
        ENDLOOP.

        DELETE p_saida    WHERE augdt < v_aquav_ini.
        DELETE p_zfit0087 WHERE augdt < v_aquav_ini.
      ENDIF.

      "Eliminar Clientes Intercompany
      CHECK p_saida[] IS NOT INITIAL.

      CLEAR: r_bukrs_tmp.

      r_bukrs_tmp-sign   = 'I'.
      r_bukrs_tmp-option = 'EQ'.
      r_bukrs_tmp-low    = '0041'.  APPEND r_bukrs_tmp.
      r_bukrs_tmp-low    = '0039'.  APPEND r_bukrs_tmp.

      LOOP AT p_zfit0087 INTO gw_zfit0087 WHERE ktokd EQ 'ZCIC'
                                            AND bukrs NOT IN r_bukrs_tmp.
        DELETE FROM zfit0087 WHERE belnr   = gw_zfit0087-belnr
                               AND buzei   = gw_zfit0087-buzei
                               AND bukrs   = gw_zfit0087-bukrs
                               AND augbl   = gw_zfit0087-augbl.
      ENDLOOP.

      DELETE p_saida    WHERE ktokd EQ 'ZCIC' AND bukrs NOT IN r_bukrs_tmp.
      DELETE p_zfit0087 WHERE ktokd EQ 'ZCIC' AND bukrs NOT IN r_bukrs_tmp.

  ENDCASE.


ENDFORM.

"FI - ZFI0064 - Transação Parametros US #149772 - WPP  <<<---
*&---------------------------------------------------------------------*
*& Form recebimento_juros
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GW_SAIDA_DMBTR
*&      <-- GW_SAIDA_DMBE2
*&---------------------------------------------------------------------*
FORM recebimento_juros  TABLES pt_bsis_rec TYPE tt_bsis
                               pt_hkont_param TYPE zfitt_0233
                        USING  p_bukrs
                               p_augbl
                        CHANGING p_dmbtr
                                 p_dmbe2.


  LOOP AT pt_hkont_param INTO DATA(ls_hkont).

      READ TABLE pt_bsis_rec INTO DATA(ls_bsis_rec)
        WITH KEY  bukrs = p_bukrs
                  belnr = p_augbl
                  hkont = ls_hkont-hkont.

      IF sy-subrc EQ 0.

        CASE ls_bsis_rec-shkzg.
          WHEN 'H'.

            SUBTRACT ls_bsis_rec-dmbtr FROM p_dmbtr .
            SUBTRACT ls_bsis_rec-dmbe2 FROM p_dmbe2.

          WHEN 'S'.

            ADD ls_bsis_rec-dmbtr TO p_dmbe2.
            ADD ls_bsis_rec-dmbe2 TO p_dmbe2.

          WHEN OTHERS.
        ENDCASE.

      ENDIF.

  ENDLOOP.

ENDFORM.

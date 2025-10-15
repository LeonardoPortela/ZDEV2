*----------------------------------------------------------------------*
***INCLUDE LZGFS_TRACE_COTTONF02.
*----------------------------------------------------------------------*

************************************************************************
* selecao dados
************************************************************************
FORM f_selecao_dados  USING  p_ch_referencia.

  DATA: wl_cont        TYPE sy-tabix,
        msg(150),
        resposta,
        wl_cont_aux(6).

  REFRESH: tg_fardos.
  CLEAR: wg_header, w_zsdt0001, w_zsdt0330.

  SELECT SINGLE *
    FROM zsdt0001
    INTO w_zsdt0001
   WHERE ch_referencia = p_ch_referencia.

  IF sy-subrc <> 0.
    RAISE romaneio_nao_encontrado.
  ENDIF.

  ""Projeto Reestruturação Algodao 2024
*  SELECT *
*    FROM zsdt0330
*    INTO TABLE t_zsdt0330
*   WHERE ch_referencia = w_zsdt0001-ch_referencia
*     AND werks         = w_zsdt0001-branch
*     AND safra         = w_zsdt0001-nr_safra
*     AND cancelado     = abap_off.
*
*  IF sy-subrc <> 0.
*    RAISE romaneio_nao_encontrado.
*  ENDIF.


*  LOOP AT t_zsdt0330  INTO w_zsdt0330.
*    w_zsdt0330-charg     = w_zsdt0330-safra.
*    MODIFY t_zsdt0330 FROM w_zsdt0330 INDEX sy-tabix.
*  ENDLOOP.
"Projeto Reestruturação Algodao 2024
*
*  SELECT a~mandt a~werks a~lgort a~charg a~nr_romaneio a~nfnum
*         a~vbeln a~vbeln_vf a~placa_cav a~menge a~motorista a~kunnr a~integ_rondonline
*         a~dt_inicial_integ a~status a~id_hist_rondon a~werks_orig a~cd_sai a~tipo_fardo
*    FROM zmmt0008 AS a
*   INNER JOIN vbap AS b ON a~vbeln = b~vbeln
*    INTO CORRESPONDING FIELDS OF TABLE t_zmmt0008
*     FOR ALL ENTRIES IN t_zsdt0330
*   WHERE a~werks       EQ t_zsdt0330-werks
*     AND a~lgort       EQ t_zsdt0330-lgort
*     AND a~nr_romaneio EQ w_zsdt0001-nr_romaneio
*     AND b~charg       EQ t_zsdt0330-charg.


  SELECT a~mandt a~werks a~lgort a~charg a~nr_romaneio a~nfnum
         a~vbeln a~vbeln_vf a~placa_cav a~menge a~motorista a~kunnr a~integ_rondonline
         a~dt_inicial_integ a~status a~id_hist_rondon a~werks_orig a~cd_sai a~tipo_fardo
    FROM zmmt0008 AS a
   INNER JOIN vbap AS b ON a~vbeln = b~vbeln
    INTO CORRESPONDING FIELDS OF TABLE t_zmmt0008
   WHERE a~werks       EQ w_zsdt0001-branch
     AND a~nr_romaneio EQ w_zsdt0001-nr_romaneio
     AND b~charg       EQ w_zsdt0001-nr_safra
     AND a~safra       EQ w_zsdt0001-nr_safra.

  IF sy-subrc NE 0.
    RAISE romaneio_nao_encontrado.
  ENDIF.

ENDFORM.

************************************************************************
* prepara impressao
************************************************************************
FORM f_prepara_impressao.

  FREE: tg_fardos.

  READ TABLE tl_0008 INDEX 1.

  IF tl_0008-status = '3'.
    RAISE romaneio_em_estorno.
  ENDIF.

  wg_header-veiculo   = tl_0008-placa_cav.
  wg_header-romaneio  = tl_0008-nr_romaneio.
  wg_header-motorista = tl_0008-motorista.
  wg_header-bloco     = tl_0008-lgort.

  SELECT SINGLE name1
    FROM kna1
    INTO wg_header-cliente
   WHERE kunnr EQ tl_0008-kunnr.

  SELECT SINGLE *
    FROM zsdt0001
   WHERE vbeln        EQ tl_0008-vbeln
     AND tp_movimento EQ 'S'
     AND nr_romaneio  EQ w_zsdt0001-nr_romaneio
     AND branch       EQ w_zsdt0001-branch
     AND vbeln        EQ tl_0008-vbeln.

  IF sy-subrc IS INITIAL.
    wg_header-safra = zsdt0001-nr_safra.

**  Destinatario
    SELECT SINGLE name1
      FROM kna1
      INTO wg_header-destinatario
     WHERE kunnr EQ zsdt0001-id_cli_dest.

    SELECT SINGLE *
      FROM j_1bnflin
     WHERE refkey EQ tl_0008-vbeln_vf.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE *
        FROM j_1bnfe_active
       WHERE docnum EQ j_1bnflin-docnum.

      IF sy-subrc IS INITIAL.
**      Nota Fiscal
        wg_header-nfnum = j_1bnfe_active-nfnum9.

        SELECT SINGLE *
          FROM j_1bbranch
         WHERE bukrs  EQ j_1bnfe_active-bukrs
           AND branch EQ j_1bnfe_active-branch.
** Remetente
        wg_header-remetente = j_1bbranch-name.
        SELECT SINGLE butxt
          FROM t001
          INTO wg_header-empresa
         WHERE bukrs EQ j_1bnfe_active-bukrs.
      ENDIF.
    ENDIF.

  ENDIF.

  SELECT vbeln vgbel matnr
    FROM vbrp
    INTO TABLE tg_vbrp
     FOR ALL ENTRIES IN tl_0008
   WHERE vbeln EQ tl_0008-vbeln_vf.

  IF sy-subrc IS INITIAL.
    SELECT matnr normt
      FROM mara
      INTO TABLE tg_mara
       FOR ALL ENTRIES IN tg_vbrp
     WHERE matnr EQ tg_vbrp-matnr.
  ENDIF.

  DATA: fardo TYPE zppt0002-cd_sai.

  LOOP AT tl_0008.
    LOOP AT tg_mara INTO wg_mara.
      CONDENSE tl_0008-charg NO-GAPS.

      IF tl_0008-cd_sai IS NOT INITIAL.
        fardo = tl_0008-cd_sai.
      ELSE.
        SELECT SINGLE cd_sai
          FROM zppt0002
          INTO fardo WHERE acharg = tl_0008-charg.
      ENDIF.

      tg_fardos-fardo = fardo. "TL_0008-CHARG+2.
      tg_fardos-tipo  = wg_mara-normt(5).
      tg_fardos-peso  = tl_0008-menge.
      APPEND tg_fardos.
    ENDLOOP.

    CLEAR: tg_fardos.
  ENDLOOP.

ENDFORM.

************************************************************************
* impressao formulario
************************************************************************
FORM f_imprime_smartforms CHANGING p_xstring.

  DATA: vl_form    TYPE tdsfname,
        vl_name    TYPE rs38l_fnam,
        vl_safra   TYPE char4,
        sl_rodape  TYPE zmmr0001_rod,
        tl_fardos1 TYPE TABLE OF zmms004 WITH HEADER LINE,
        tl_fardos2 TYPE TABLE OF zmms004,
        tl_fardos3 TYPE TABLE OF zmms004,
        tl_fardos4 TYPE TABLE OF zmms004,
        wl_cont    TYPE sy-tabix,
        wl_menge   TYPE zmmt0008-menge,
        tg_total   TYPE TABLE OF zmms006 WITH HEADER LINE,
        wl_tipo    TYPE mara-normt,
        wl_flag.
*
  FREE: p_xstring.

  vl_form = 'ZMMS002'.
*
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_form
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    RAISE erro_impressao_formulario.
  ENDIF.

  SORT: tg_fardos BY tipo.

  LOOP AT tg_fardos.
    MOVE:                    1 TO tg_total-total,
                tg_fardos-tipo TO tg_total-tipo,
                tg_fardos-peso TO tg_total-peso.
    COLLECT tg_total.
  ENDLOOP.

  ls_control-no_dialog = abap_true.
  ls_control-langu     = sy-langu.
  ls_control-getotf    = abap_true.

  CALL FUNCTION vl_name
    EXPORTING
      header             = wg_header
      control_parameters = ls_control
    IMPORTING
      job_output_info    = ls_job_output_info
    TABLES
      tg_fardos          = tg_fardos
      tg_fardos1         = tl_fardos1
      tg_fardos2         = tl_fardos2
      tg_fardos3         = tl_fardos3
      tg_fardos4         = tl_fardos4
      tg_total           = tg_total
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    RAISE erro_impressao_formulario.
  ENDIF.

  MOVE ls_job_output_info-otfdata[] TO ls_otfdata[].

*-------------------------------------------
*-format xstring
*-------------------------------------------
  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
    IMPORTING
      bin_filesize          = ls_bin_fsize
      bin_file              = ls_xstring_document
    TABLES
      otf                   = ls_otfdata[]
      lines                 = t_lines
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      err_bad_otf           = 4
      OTHERS                = 5.

  IF sy-subrc = 0.
    p_xstring = ls_xstring_document.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
***INCLUDE LZGF002F01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout  USING p_table.
  REFRESH estrutura.
  IF p_table EQ 'TL_SAIDA'.
    PERFORM montar_estrutura USING:
     1  'BSID'   'KUNNR'            'TL_SAIDA' 'KUNNR'     ' '  ' ',
     1  'BSID'   'BELNR'            'TL_SAIDA' 'BELNR'     ' '  ' ',
     2  'BSID'   'BLDAT'            'TL_SAIDA' 'BLDAT'     ' '  ' ',
     3  'BSID'   'BUDAT'            'TL_SAIDA' 'BUDAT'     ' '  ' ',
     4  'BSID'   'BLART'            'TL_SAIDA' 'BLART'     ' '  ' ',
     4  'BSID'   'ZFBDT'            'TL_SAIDA' 'ZFBDT'     'Vencimento'  ' ',
     4  'BSID'   'VBEL2'            'TL_SAIDA' 'VBEL2'     ' '  ' ',
     4  'BSID'   'VBELN'            'TL_SAIDA' 'VBELN'     ' '  ' ',
     4  'BSID'   'DMBTR'            'TL_SAIDA' 'DMBTR'     'Valor R$'  ' ',
     4  'BSID'   'DMBE2'            'TL_SAIDA' 'DMBE2'     'Valor $'  ' ',
     4  'BSID'   'SGTXT'            'TL_SAIDA' 'SGTXT'     ' '  ' '.
  ELSEIF p_table EQ 'TL_SAIDA_EXEC'.
    PERFORM montar_estrutura USING:
    1  'ZSDT0053'   'NRO_SOL_OV'       'TL_SAIDA_EXEC' 'NRO_SOL_OV'     ' '  ' ',
    1  'ZSDT0053'   'POSNR'            'TL_SAIDA_EXEC' 'POSNR'     ' '  ' ',
    1  'ZSDT0053'   'ZMENG'            'TL_SAIDA_EXEC' 'ZMENG'     ' '  ' ',
    2  'ZSDT0053'   'VALDT'            'TL_SAIDA_EXEC' 'VALDT'     ' '  ' ',
    3  'ZSDT0053'   'VLRTOT'           'TL_SAIDA_EXEC' 'VLRTOT'    ' '  ' ',
    4  'VBAK'       'VBELN'            'TL_SAIDA_EXEC' 'VBELN'     ' '  ' ',
    4  ' '          ' '                'TL_SAIDA_EXEC' 'MSG'       'Msg de bapi'  '80'.
  ELSEIF p_table EQ '<FS_TABLE>'.
    PERFORM montar_estrutura_oo USING:
      space 'ZSDT0059'  'BEZEI'          p_table 'BEZEI'        ' '              '20'  ' ' ' ' ' ',
      space 'BSID'      'WAERS'          p_table 'WAERS'        ' '              '20'  ' ' ' ' ' ',
      space 'ZSDT0059'  'CBOT'           p_table 'CBOT'         'REF. CBOT'      '20'  ' ' ' ' ' ',
      space 'ZSDT0059'  'MONAT'          p_table 'MONAT'        'Mês Fixação'    '20'  ' ' ' ' ' ',
*      1 'ZSDT0059'  'ZMENG'          P_TABLE 'ZMENG'        'Qtd. Fixada' '20' ' ' ' ' ' ',
      space 'ZSDT0059' 'POSNR1'          p_table 'POSNR1'       'Item Prod.'     '20'  ' ' ' ' ' '.

    IF tg_tp_venda EQ 'Z'.
      PERFORM montar_estrutura_oo USING:
      space 'ZSDT0059'  'SAFRA'          p_table 'SAFRA'        'Safra'          '06'  ' ' ' ' ' '. "RJF
    ENDIF.

    PERFORM montar_estrutura_oo USING:
         space 'ZSDT0059' 'VALDT'          p_table 'VALDT'        'Dt. Fixação'    '20'  ' ' ' ' ' ',
         space 'ZSDT0059'  'VALDT_HEDGE'    p_table 'VALDT_HEDGE'  'Dt. Lib. HEDGE' '30'  ' ' ' ' ' '.

  ENDIF.

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen).

  DATA: x_contador TYPE string.
  CLEAR: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-reptext_ddic  = p_scrtext_l.
  IF p_outputlen IS INITIAL.
    wa_estrutura-outputlen     = x_contador.
  ELSE.
    wa_estrutura-outputlen     =  p_outputlen.
  ENDIF.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-ddictxt = 'L'.
  ENDIF.

  IF p_field EQ 'DMBTR'
  OR p_field EQ 'DMBE2'.
    wa_estrutura-do_sum = 'X'.
  ENDIF.

*  CASE P_FIELD.
*    WHEN 'BELNR'
*      OR 'AWKEY'.
*      WA_ESTRUTURA-HOTSPOT = 'X'.
*      WA_ESTRUTURA-KEY = 'X'.
*
*  ENDCASE.


  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM montar_estrutura_oo USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     "type DD03D-SCRTEXT_L
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

  CLEAR w_fieldcatalog.
  IF p_col_pos IS INITIAL.
    ADD 1 TO wl_col_pos.
    p_col_pos = wl_col_pos.
  ELSE.
    w_fieldcatalog-just = 'R'.
  ENDIF.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
*  w_fieldcatalog-key_sel       = 'X'.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.
  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                     " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM definir_eventos.
  PERFORM f_carregar_eventos USING:
*                                 SLIS_EV_USER_COMMAND  'XUSER_COMMAND',
                                 slis_ev_pf_status_set 'XPF_STATUS_SET',
                                 slis_ev_top_of_page   'XTOP_OF_PAGE'.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0299   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " f_carregar_eventos
*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED
  REFRESH t_top.
  PERFORM f_construir_cabecalho.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top.
*            I_LOGO             = 'CLARO_50'.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0510   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho.

  DATA: ls_line TYPE slis_listheader.
  CLEAR: ls_line.
  SELECT SINGLE butxt
      FROM t001
      INTO ls_line-info
       WHERE bukrs EQ wg_bukrs.
  ls_line-key = 'Empresa:'.
  CONCATENATE 'Empresa: ' ls_line-info INTO ls_line-info SEPARATED BY space.
  ls_line-typ =  'A'.
*  CONCATENATE 'EMPRESA:' S_BUKRS-LOW INTO LS_LINE-INFO SEPARATED BY SPACE.
  APPEND ls_line TO t_top.

  CLEAR ls_line.
  SELECT SINGLE name1
     FROM kna1
     INTO ls_line-info
      WHERE kunnr EQ wg_kunnr.
  CONCATENATE 'Cliente:' ls_line-info INTO ls_line-info SEPARATED BY space.
  ls_line-typ  = 'A'.
  APPEND ls_line TO t_top.

*  CLEAR LS_LINE.
*  LS_LINE-TYP  = 'A'.
*  LS_LINE-KEY = 'QUEBRA'.
*  LS_LINE-INFO = ' '.
*  APPEND LS_LINE TO T_TOP.
*
*  CLEAR LS_LINE.
*  LS_LINE-TYP  = 'A'.
*  LS_LINE-KEY = 'QUEBRA'.
*  LS_LINE-INFO = ' '.
*  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*---------------------------------------------------------------------*
*       FORM XPF_STATUS_SET                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xpf_status_set USING extab TYPE kkblo_t_extab.         "#EC CALLED
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM. "XPF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  f_preencher_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0380   text
*      -->P_0381   text
*      -->P_0382   text
*----------------------------------------------------------------------*
FORM f_preencher_dynpro USING l_start TYPE c l_name TYPE c l_value.

  MOVE l_start TO wg_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO wg_bdc-program,
  l_value TO wg_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO wg_bdc-fnam,
      l_value TO wg_bdc-fval.
  ENDIF.
  APPEND wg_bdc TO tg_bdc.
  CLEAR: wg_bdc.

ENDFORM.                    " f_preencher_dynpro
*&---------------------------------------------------------------------*
*&      Form  exibe_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exibe_log TABLES te_return STRUCTURE zsds008.
*               USING TL_PGT_ANT. " TYPE ZSDT0054.

  DATA: x_msgid   LIKE sy-msgid,
        x_msgno   LIKE sy-msgno,
        x_msgty   LIKE sy-msgty,
        x_msgv1   LIKE sy-msgv1,
        x_msgv2   LIKE sy-msgv2,
        x_msgv3   LIKE sy-msgv3,
        x_msgv4   LIKE sy-msgv4,
        x_message LIKE message.


  LOOP AT tg_msg INTO wg_msg.
    CLEAR:
      x_msgid,
      x_msgno,
      x_msgty,
      x_msgv1,
      x_msgv2,
      x_msgv3,
      x_msgv4,
      x_message.

    MOVE:
      wg_msg-msgid  TO x_msgid,
      wg_msg-msgnr  TO x_msgno,
      wg_msg-msgtyp TO x_msgty,
      wg_msg-msgv1  TO x_msgv1,
      wg_msg-msgv2  TO x_msgv2,
      wg_msg-msgv3  TO x_msgv3,
      wg_msg-msgv4  TO x_msgv4.

    CALL FUNCTION 'WRITE_MESSAGE_NEW'
      EXPORTING
        msgid = x_msgid
        msgno = x_msgno
        msgty = x_msgty
        msgv1 = x_msgv1
        msgv2 = x_msgv2
        msgv3 = x_msgv3
        msgv4 = x_msgv4
        msgv5 = x_msgv4
      IMPORTING
        messg = x_message.

    IF (  wg_msg-msgtyp = 'S'
      AND x_msgno = '312' )
    OR wg_msg-msgtyp = 'E'.
      MOVE: x_message-msgtx TO te_return-msg.

      APPEND te_return.
      CLEAR: te_return-msg.



    ENDIF.

  ENDLOOP.


ENDFORM.                    " exibe_log
*&---------------------------------------------------------------------*
*&      Form  BUSCA_SALDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_0053  text
*----------------------------------------------------------------------*
FORM busca_saldo  TABLES   t_0053 STRUCTURE zsdt0053
                   USING p_tipo_sol.
  "Introduzir nome correto para <...>.

  DATA: tl_0053     LIKE TABLE OF zsdt0053 WITH HEADER LINE,
        tl_vbfa     TYPE TABLE OF vbfa WITH HEADER LINE,
        tl_vbfa_aux TYPE TABLE OF vbfa WITH HEADER LINE,
        wl_matnr    TYPE mara-matnr,
        wl_zieme    TYPE mara-meins,
        wl_pmein    TYPE mara-meins,
        wl_menge    TYPE ekpo-menge,
        wl_tabix    TYPE sy-tabix.

  REFRESH: tg_saldo.
*  TL_ITENS[] = TG_ITENS[].
*
*  DELETE TL_ITENS WHERE VBELN IS INITIAL.

  IF t_0053[] IS NOT INITIAL.
    IF p_tipo_sol EQ 'VN'.
      SELECT *
        FROM zsdt0053
        INTO TABLE tl_0053
          FOR ALL ENTRIES IN t_0053
          WHERE nro_sol_ov EQ t_0053-nro_sol_ov
            AND vbeln NE space.
    ELSEIF p_tipo_sol EQ 'FL'.
      SELECT *
        FROM zsdt0066
        INTO CORRESPONDING FIELDS OF TABLE tl_0053
          FOR ALL ENTRIES IN t_0053
          WHERE nro_sol_ov EQ t_0053-nro_sol_ov
            AND vbeln NE space.

    ENDIF.

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM vbfa
        INTO TABLE tl_vbfa
         FOR ALL ENTRIES IN tl_0053
         WHERE vbelv EQ tl_0053-vbeln
           AND vbtyp_n EQ 'J'
           AND vbtyp_v EQ 'C'.

      IF tl_vbfa[] IS NOT INITIAL.
        SELECT *
          FROM vbfa
          INTO TABLE tl_vbfa_aux
           FOR ALL ENTRIES IN tl_vbfa
           WHERE vbeln EQ tl_vbfa-vbeln
             AND vbtyp_n EQ 'J'
             AND vbtyp_v EQ 'J'.

        LOOP AT tl_vbfa_aux.
          DELETE tl_vbfa WHERE vbeln EQ tl_vbfa_aux-vbeln.
        ENDLOOP.
      ENDIF.

      SORT tl_vbfa BY vbelv.
      REFRESH: tg_saldo.
      LOOP AT tl_0053.
        CLEAR: wg_saldo.
        LOOP AT tl_vbfa WHERE vbelv EQ tl_0053-vbeln.

          wg_saldo-vbeln = tl_vbfa-vbelv.
          wg_saldo-werks = tl_0053-werks.
          wg_saldo-total = tl_vbfa-rfmng.

          COLLECT wg_saldo INTO tg_saldo .
          wl_tabix = sy-tabix.
*        CLEAR TG_SALDO.
        ENDLOOP.
        IF sy-subrc IS INITIAL.
          READ TABLE tg_saldo INTO wg_saldo INDEX wl_tabix.
        ENDIF.

        wl_matnr = tl_0053-matnr.
        wl_zieme = tl_0053-zieme.
        wl_pmein = tl_vbfa-meins.
        wl_menge = tl_0053-zmeng.

        IF tl_vbfa IS NOT INITIAL.
          CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
            EXPORTING
              i_matnr              = wl_matnr
              i_in_me              = wl_zieme
              i_out_me             = wl_pmein
              i_menge              = wl_menge
            IMPORTING
              e_menge              = wl_menge
            EXCEPTIONS
              error_in_application = 1
              error                = 2
              OTHERS               = 3.
        ENDIF.

        wg_saldo-zmeng = wl_menge.
        wg_saldo-nro_sol_ov = tl_0053-nro_sol_ov.
        wg_saldo-saldo = wg_saldo-zmeng - wg_saldo-total.

        IF wl_tabix IS NOT INITIAL.
          MODIFY tg_saldo FROM wg_saldo INDEX wl_tabix.
        ELSE.
          wg_saldo-vbeln = tl_0053-vbeln.
          wg_saldo-werks = tl_0053-werks.
          APPEND wg_saldo TO tg_saldo.
        ENDIF.
        CLEAR: wl_tabix.
*      ENDIF.
      ENDLOOP.

    ENDIF.
  ENDIF.
ENDFORM.                    " BUSCA_SALDO

*&---------------------------------------------------------------------*
*&      Form  ENVIA_QUANTIDADE_PNL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_ITENS_NRO_SOL_OV  text
*      -->P_TL_ITENS_POSNR  text
*      -->P_TL_ITENS_VBELN  text
*      -->P_TL_ITENS_ZMENG  text
*----------------------------------------------------------------------*
FORM envia_quantidade_pnl  USING    p_tl_itens_nro_sol_ov
                                    p_tl_itens_posnr
                                    p_tl_itens_vbeln
                                    p_tl_itens_zmeng.

  DATA : wa_zsdt0051 TYPE zsdt0051,
         wa_saida    TYPE zsd_pnl_premio,
         it_saida    TYPE TABLE OF zsd_pnl_premio WITH HEADER LINE.

  SELECT SINGLE * FROM zsdt0051 INTO wa_zsdt0051 WHERE nro_sol_ov = p_tl_itens_nro_sol_ov.

  IF wa_zsdt0051-tp_venda = '00091' OR wa_zsdt0051-tp_venda = '00027'."Apenas para vendas Frame

    wa_saida-nr_sol_ov       = wa_zsdt0051-nro_sol_ov.
    wa_saida-tp_venda	       = wa_zsdt0051-tp_venda.
    wa_saida-material        = wa_zsdt0051-matnr.
    wa_saida-dt_ini_embarque = wa_zsdt0051-dtde_logist.
    wa_saida-dt_fim_embarque = wa_zsdt0051-dtate_logist.
    wa_saida-dt_venda	       = wa_zsdt0051-data_venda.
    wa_saida-dt_solicitacao  = wa_zsdt0051-data_venda.
    wa_saida-status          = 'E'.
    wa_saida-quantidade	     = p_tl_itens_zmeng.
    wa_saida-nr_ordem	       = p_tl_itens_vbeln.
    wa_saida-posnr           = p_tl_itens_posnr.
    wa_saida-doc_dev         = '0000000001'.
    wa_saida-dt_fixacao_premio = sy-datum.

    CALL FUNCTION 'Z_SD_OUTBOUND_PNL_PREMIO' IN BACKGROUND TASK
      DESTINATION 'XI_PNL_PREMIO'
      TABLES
        t_zsd_pnl_premio = it_saida[].

    COMMIT WORK.
  ENDIF.

ENDFORM.                    " ENVIA_QUANTIDADE_PNL
*&---------------------------------------------------------------------*
*& Form f_preenche_x
*&---------------------------------------------------------------------*
FORM f_preenche_x USING us_data TYPE any
                        uv_upt_flag TYPE updkz_d
               CHANGING cs_datax TYPE any.

  DATA: datatype TYPE REF TO cl_abap_datadescr,
        field(5) TYPE c.
  DATA: linetype TYPE REF TO cl_abap_structdescr,
        mystruc  TYPE spfli.

  DATA lv_length TYPE i.

  ASSIGN ('US_DATA') TO FIELD-SYMBOL(<fs_data>).

  CHECK sy-subrc EQ 0.

  linetype ?= cl_abap_typedescr=>describe_by_data( <fs_data> ).

  LOOP AT linetype->components ASSIGNING FIELD-SYMBOL(<fs_component>).

    DATA(lv_campo) = 'US_DATA-' && <fs_component>-name.
    DATA(lv_campox) = 'CS_DATAX-' && <fs_component>-name.

    ASSIGN (lv_campo) TO FIELD-SYMBOL(<fs_campo>).

    CHECK sy-subrc EQ 0.

    ASSIGN (lv_campox) TO FIELD-SYMBOL(<fs_campox>).

    CHECK <fs_campox> IS ASSIGNED.

    IF <fs_campo> IS NOT INITIAL.

      DESCRIBE FIELD <fs_campox> LENGTH lv_length IN CHARACTER MODE.

      IF lv_length = 1.
        <fs_campox> = 'X'.
      ELSE.
        <fs_campox> = <fs_campo>.
      ENDIF.

    ENDIF.

  ENDLOOP.

  CHECK uv_upt_flag IS NOT INITIAL.

  lv_campox = 'CS_DATAX-UPDATEFLAG'.

  ASSIGN (lv_campox) TO <fs_campox>.

  CHECK sy-subrc EQ 0.

  <fs_campox> = uv_upt_flag.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_bapi_confirm_process
*&---------------------------------------------------------------------*
FORM f_bapi_confirm_process USING iv_commit TYPE c
                         CHANGING ct_return TYPE bapiret2_tt
                                  cv_erro TYPE c.

  READ TABLE ct_return TRANSPORTING NO FIELDS
    WITH KEY type = 'E'.

  IF sy-subrc NE 0 AND iv_commit IS NOT INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    cv_erro = abap_false.

  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    cv_erro = abap_true.

  ENDIF.

ENDFORM.

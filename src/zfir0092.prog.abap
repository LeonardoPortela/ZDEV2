*----------------------------------------------------------------------*
* Programa..: ZFIR0092.                                                *
* Tipo......: R - Report                                               *
* Transação.: ZFI0131                                                  *
* Descrição.: RELATORIO ACOMPANHAMENTO AL5 BANK CONTA A RECEBER        *
* Autor.....: CBRAND                                                   *
* Data......: 21.04.2021                                               *
*----------------------------------------------------------------------*
*                     Controle de Alterações                           *
*----------------------------------------------------------------------*
* Data       | Change     | Autor        | Alteração                   *
*----------------------------------------------------------------------*
* 21.04.2021   |  |CBRAND     | Codificação Inicial                    *
*----------------------------------------------------------------------*
REPORT zfir0092.
TABLES: zfit0170.
*---------------------------------------------------------------------*
* Declaração de Types
*---------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_bsik,
    bukrs     TYPE bsik-bukrs,
    gjahr     TYPE bsik-gjahr,
    zuonr     TYPE bsik-zuonr,
    belnr     TYPE bsik-belnr,
    budat     TYPE bsik-budat,
    bldat     TYPE bsik-bldat,
    xblnr     TYPE bsik-xblnr,
    dmbtr     TYPE bsik-dmbtr,
    sgtxt     TYPE bsik-sgtxt,
    zbd1t     TYPE bsik-zbd1t,
    zfbdt     TYPE bsik-zfbdt,
    augbl     TYPE bsik-augbl,
    remove(1) TYPE c,
  END OF ty_bsik,

  BEGIN OF ty_bsak,
    bukrs     TYPE bsak-bukrs,
    zuonr     TYPE bsak-zuonr,
    belnr     TYPE bsak-belnr,
    budat     TYPE bsak-budat,
    bldat     TYPE bsak-bldat,
    xblnr     TYPE bsak-xblnr,
    dmbtr     TYPE bsak-dmbtr,
    sgtxt     TYPE bsak-sgtxt,
    augbl     TYPE bsak-augbl,
    gjahr     TYPE bsak-gjahr,
    augdt     TYPE bsak-augdt,
    remove(1) TYPE c,
  END OF ty_bsak,


  BEGIN OF ty_saida,
    icon(5)     TYPE c,
    bukrs       TYPE bsik-bukrs,
    belnr       TYPE bsik-belnr,
    bldat       TYPE bsik-bldat,
    id_oper_al5 TYPE zfit0170-id_oper_al5,
    dt_oper_al5 TYPE zfit0170-dt_oper_al5,
    sgtxt       TYPE bsik-sgtxt,
    xblnr       TYPE bsak-xblnr,
    budat       TYPE bsak-budat,
    zuonr       TYPE bsik-zuonr,
    dmbtr       TYPE bsak-dmbtr,
    zfbdt       TYPE bsik-zfbdt,
    augbl       TYPE zfit0170-augbl,

  END OF ty_saida.
*----------------------------------------------------------------------*
* Declaração de Tabelas
*----------------------------------------------------------------------*
DATA: git_zfit0170 TYPE TABLE OF zfit0170,
      git_bsik     TYPE TABLE OF ty_bsik,
      git_bsak     TYPE TABLE OF ty_bsak,
      git_bkpf     TYPE TABLE OF bkpf,
      git_codal5   TYPE TABLE OF setleaf WITH HEADER LINE,
      git_saida    TYPE TABLE OF ty_saida.

*----------------------------------------------------------------------*
* Declaração de Estruturas
*----------------------------------------------------------------------*
DATA: gwa_zfit0170 LIKE LINE OF  git_zfit0170,
      gwa_bsik     LIKE LINE OF  git_bsik,
      gwa_bsak     LIKE LINE OF  git_bsak,
      gwa_bkpf     LIKE LINE OF  git_bkpf,
      gwa_saida    LIKE LINE OF  git_saida.

DATA: fcode     TYPE TABLE OF sy-ucomm,
      gwa_fcode TYPE sy-ucomm.

*----------------------------------------------------------------------*
* Declaração de Ranges
*----------------------------------------------------------------------*
RANGES: gra_gjahr FOR bsik-gjahr,
        gra_lifnr FOR lfa1-lifnr.

*----------------------------------------------------------------------*
* Declaração de Constantes
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Parâmetros de seleção
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_bukrs FOR zfit0170-bukrs  NO-EXTENSION
                                            OBLIGATORY DEFAULT '0001',
                p_date  FOR zfit0170-dt_envio OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.


*---------------------------------------------------------------------*
* START-OF-SELECTION                                                  *
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM fm_seleciona_dados.
  PERFORM fm_manipula_dados.
  PERFORM fm_chama_alv.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  FM_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM fm_seleciona_dados .

  DATA: lva_year01  TYPE i,
        lva_year02  TYPE i,
        lva_times   TYPE i,
        lva_line    TYPE sy-tabix,
        lva_xdtvcto TYPE sy-datum.

  SELECT *
    FROM setleaf INTO TABLE git_codal5
   WHERE setname EQ 'MAGGI_CODFORAL5'.

  IF git_codal5[] IS NOT INITIAL.
    LOOP AT git_codal5.
      gra_lifnr-sign = 'I'.
      gra_lifnr-option = 'EQ'.
      gra_lifnr-low  =  git_codal5-valfrom.
      APPEND gra_lifnr.
    ENDLOOP.
  ENDIF.

  lva_year01 =  p_date-low(4).
  lva_year02 =  p_date-high(4).

  lva_times = lva_year02 - lva_year01.

  IF lva_times = 0.
    gra_gjahr-sign = 'I'.
    gra_gjahr-option = 'EQ'.
    gra_gjahr-low    = lva_year01.
    APPEND gra_gjahr.
  ELSE.
    gra_gjahr-sign = 'I'.
    gra_gjahr-option = 'EQ'.
    gra_gjahr-low    = lva_year01.
    APPEND gra_lifnr.

    DO  lva_times  TIMES.
      gra_gjahr-sign = 'I'.
      gra_gjahr-option = 'EQ'.
      gra_gjahr-low    = lva_year01 + 1.
      APPEND gra_gjahr.
    ENDDO.

  ENDIF.

  SELECT bukrs
         gjahr
         zuonr
         belnr
         budat
         bldat
         xblnr
         dmbtr
         sgtxt
         zbd1t
         zfbdt
         augbl
  INTO TABLE git_bsik
  FROM bsik
    WHERE bukrs IN p_bukrs
    AND lifnr   IN gra_lifnr
    AND gjahr   IN gra_gjahr.

  CHECK sy-subrc = 0.

  LOOP AT git_bsik INTO gwa_bsik.
    CLEAR: lva_line.
    MOVE sy-tabix TO lva_line.
    lva_xdtvcto =  gwa_bsik-zbd1t + gwa_bsik-zfbdt.
    IF lva_xdtvcto >= p_date-low AND lva_xdtvcto <= p_date-high .
      CONTINUE.
    ELSE.
      gwa_bsik-remove = 'X'.
      MODIFY git_bsik FROM gwa_bsik INDEX lva_line TRANSPORTING remove.
    ENDIF.
  ENDLOOP.

  DELETE git_bsik WHERE remove = 'X'.

  IF git_bsik IS NOT INITIAL.
    SELECT *
     APPENDING TABLE git_bkpf
     FROM bkpf
       FOR ALL ENTRIES IN git_bsik
       WHERE bukrs EQ git_bsik-bukrs
         AND gjahr EQ git_bsik-gjahr
         AND belnr EQ git_bsik-belnr.
  ENDIF.

  "Busca de Dados Compensados
  SELECT bukrs
         zuonr
         belnr
         budat
         bldat
         xblnr
         dmbtr
         sgtxt
         augbl
         gjahr
         augdt
  INTO TABLE git_bsak
  FROM bsak
    WHERE bukrs IN p_bukrs
     AND lifnr  IN gra_lifnr
     AND augdt  IN p_date
     AND gjahr  IN gra_gjahr.

  CHECK sy-subrc = 0.

  LOOP AT git_bsak INTO gwa_bsak.
    CLEAR: lva_line.
    MOVE sy-tabix TO lva_line.

    IF gwa_bsak-augbl+0(2) <> '20' OR gwa_bsak-augbl+0(2) <> '15' .
      gwa_bsak-remove = 'X'.
      MODIFY git_bsak FROM gwa_bsak INDEX lva_line TRANSPORTING remove.
    ENDIF.

    IF gwa_bsak-belnr(2) = '20' OR gwa_bsak-belnr(2)  = '15' .
      gwa_bsak-remove = 'X'.
      MODIFY git_bsak FROM gwa_bsak INDEX lva_line TRANSPORTING remove.
    ENDIF.
  ENDLOOP.

  DELETE git_bsak WHERE remove = 'X'.

  IF git_bsak IS NOT INITIAL.
    SELECT *
      APPENDING TABLE git_bkpf
    FROM bkpf
      FOR ALL ENTRIES IN git_bsak
    WHERE bukrs EQ git_bsak-bukrs
      AND gjahr EQ git_bsak-gjahr
      AND belnr EQ git_bsak-belnr.
  ENDIF.

  IF git_bkpf IS NOT INITIAL.
    SELECT *
     INTO TABLE git_zfit0170
     FROM zfit0170
       FOR ALL ENTRIES IN git_bkpf
       WHERE bukrs EQ git_bkpf-bukrs
         AND belnr EQ git_bkpf-belnr.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_MANIPULA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_manipula_dados .

  LOOP AT git_bsik INTO gwa_bsik.
    gwa_saida-bukrs  =   gwa_bsik-bukrs.
    gwa_saida-belnr  =   gwa_bsik-belnr.
    gwa_saida-bldat  =   gwa_bsik-bldat.
    gwa_saida-sgtxt  =   gwa_bsik-sgtxt.
    gwa_saida-xblnr  =   gwa_bsik-xblnr.
    gwa_saida-budat  =   gwa_bsik-budat.
    gwa_saida-zuonr  =   gwa_bsik-zuonr.
    gwa_saida-dmbtr  =   gwa_bsik-dmbtr.
    gwa_saida-zfbdt  =   gwa_bsik-zbd1t + gwa_bsik-zfbdt.
    gwa_saida-augbl  =   gwa_bsik-augbl.

    IF gwa_saida-augbl IS NOT INITIAL.
      MOVE icon_complete TO  gwa_saida-icon.
    ENDIF.

    IF gwa_saida-zfbdt < sy-datum AND gwa_saida-augbl IS INITIAL.
      MOVE icon_defect TO gwa_saida-icon.
    ENDIF.

    IF gwa_saida-zfbdt >= sy-datum AND gwa_saida-augbl IS INITIAL.
      MOVE icon_led_yellow TO gwa_saida-icon.
    ENDIF.


    READ TABLE git_bkpf INTO gwa_bkpf WITH KEY  bukrs = gwa_bsik-bukrs
                                                gjahr = gwa_bsik-gjahr
                                                belnr = gwa_bsik-belnr.


    READ TABLE git_zfit0170 INTO gwa_zfit0170 WITH KEY  bukrs = gwa_bkpf-bukrs
                                                        belnr = gwa_bkpf-belnr.

    gwa_saida-id_oper_al5 = gwa_zfit0170-id_oper_al5.
    gwa_saida-dt_oper_al5 = gwa_zfit0170-dt_oper_al5.

    APPEND gwa_saida TO git_saida.
    CLEAR: gwa_bkpf, gwa_zfit0170, gwa_saida.
  ENDLOOP.



  LOOP AT git_bsak INTO gwa_bsak.
    gwa_saida-bukrs  =   gwa_bsak-bukrs.
    gwa_saida-belnr  =   gwa_bsak-belnr.
    gwa_saida-bldat  =   gwa_bsak-bldat.
    gwa_saida-sgtxt  =   gwa_bsak-sgtxt.
    gwa_saida-xblnr  =   gwa_bsak-xblnr.
    gwa_saida-budat  =   gwa_bsak-budat.
    gwa_saida-zuonr  =   gwa_bsak-zuonr.
    gwa_saida-dmbtr  =   gwa_bsak-dmbtr.
    gwa_saida-zfbdt  =   gwa_bsak-augdt.
    gwa_saida-augbl  =   gwa_bsak-augbl.


    IF gwa_saida-augbl IS NOT INITIAL.
      MOVE icon_complete TO  gwa_saida-icon.
    ENDIF.

    IF gwa_saida-zfbdt < sy-datum AND gwa_saida-augbl IS INITIAL.
      MOVE icon_led_red TO gwa_saida-icon.
    ENDIF.

    IF gwa_saida-zfbdt >= sy-datum AND gwa_saida-augbl IS INITIAL.
      MOVE icon_workflow_process TO gwa_saida-icon.
    ENDIF.


    READ TABLE git_bkpf INTO gwa_bkpf WITH KEY  bukrs = gwa_bsak-bukrs
                                                gjahr = gwa_bsak-gjahr
                                                belnr = gwa_bsak-belnr.


    READ TABLE git_zfit0170 INTO gwa_zfit0170 WITH KEY  bukrs = gwa_bkpf-bukrs
                                                        belnr = gwa_bkpf-belnr.
    gwa_saida-id_oper_al5 = gwa_zfit0170-id_oper_al5.
    gwa_saida-dt_oper_al5 = gwa_zfit0170-dt_oper_al5.

    APPEND gwa_saida TO git_saida.
    CLEAR: gwa_bkpf, gwa_zfit0170, gwa_saida.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CHAMA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_chama_alv .
  DATA: lwa_layout   TYPE  slis_layout_alv,
        lwa_print    TYPE  slis_print_alv,
        lwa_keyinfo  TYPE  slis_keyinfo_alv,
        lit_fieldcat TYPE  slis_t_fieldcat_alv,
        lit_exctab   TYPE  slis_t_extab,
        lit_sorttab  TYPE  slis_t_sortinfo_alv,
        lit_events   TYPE  slis_t_event.

  PERFORM alv_init_report_layout TABLES  lit_fieldcat
                                         lit_exctab
                                         lit_sorttab
                                CHANGING lwa_layout
                                         lwa_print
                                         lwa_keyinfo.

  PERFORM alv_init_report_events TABLES lit_events.

  "lwa_layout-box_fieldname = 'MARK'.
  lwa_layout-box_tabname =  'GIT_SAIDA'.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program          = sy-cprog
      i_callback_user_command     = 'USER_COMMAND'
      i_structure_name            = 'GIT_SAIDA'
      i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
      i_background_id             = ' '
      is_layout                   = lwa_layout
      it_fieldcat                 = lit_fieldcat
      it_excluding                = lit_exctab
      it_sort                     = lit_sorttab
      i_save                      = 'A'
      is_print                    = lwa_print
      i_callback_pf_status_set    = 'SET_STATUS'
    TABLES
      t_outtab                    = git_saida
    EXCEPTIONS
      program_error               = 1
      OTHERS                      = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_INIT_REPORT_LAYOUT
*&---------------------------------------------------------------------*
FORM alv_init_report_layout  TABLES   ot_fieldcat TYPE slis_t_fieldcat_alv
                                      ot_exctab   TYPE slis_t_extab
                                      ot_sorttab  TYPE slis_t_sortinfo_alv
                             CHANGING os_layout   TYPE slis_layout_alv
                                      os_print    TYPE slis_print_alv
                                      os_keyinfo  TYPE slis_keyinfo_alv.

  REFRESH: ot_fieldcat,
           ot_exctab,
           ot_sorttab .

  PERFORM alv_build_fieldcat TABLES ot_fieldcat.

  os_layout-colwidth_optimize = 'X'.
  os_print-no_print_selinfos  = 'X'.
  os_print-no_print_listinfos = 'X'.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_INIT_REPORT_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_init_report_events  TABLES  ot_events TYPE slis_t_event.
  CLEAR:  ot_events.
  REFRESH:  ot_events.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM alv_build_fieldcat  TABLES ot_fieldcat.
  DATA: lwa_fieldcat TYPE slis_fieldcat_alv.


  CLEAR: lwa_fieldcat.
  lwa_fieldcat-tabname   = 'git_saida'.
  lwa_fieldcat-fieldname = 'ICON'.
  lwa_fieldcat-seltext_m = 'Status'.
  lwa_fieldcat-ddictxt   = 'M'.
  lwa_fieldcat-just      = 'C'.
  APPEND lwa_fieldcat TO ot_fieldcat.

  CLEAR: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'ID_OPER_AL5'.
  lwa_fieldcat-seltext_m     = 'ID.Oper.AL5'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-ref_tabname   = 'ZFIT0170'.
  lwa_fieldcat-ref_fieldname = 'ID_OPER_AL5'.
  APPEND lwa_fieldcat TO ot_fieldcat.

  CLEAR: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'DT_OPER_AL5'.
  lwa_fieldcat-seltext_m     = 'Dt.Oper.AL5'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-ref_tabname   = 'ZFIT0170'.
  lwa_fieldcat-ref_fieldname = 'DT_OPER_AL5'.
  APPEND lwa_fieldcat TO ot_fieldcat.

  CLEAR: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'BELNR'.
  lwa_fieldcat-seltext_m     = 'Documento'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-hotspot       = 'X'.
  lwa_fieldcat-ref_tabname   = 'BSIK'.
  lwa_fieldcat-ref_fieldname = 'BELNR'.
  APPEND lwa_fieldcat TO ot_fieldcat.

  CLEAR: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'SGTXT'.
  lwa_fieldcat-seltext_m     = 'Fornecedor'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-ref_tabname   = 'BSAK'.
  lwa_fieldcat-ref_fieldname = 'SGTXT'.
  lwa_fieldcat-no_zero       = 'X'.
  APPEND lwa_fieldcat TO ot_fieldcat.

  CLEAR: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'XBLNR'.
  lwa_fieldcat-seltext_m     = 'Nota Fiscal'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-ref_tabname   = 'BSIK'.
  lwa_fieldcat-ref_fieldname = 'XBLNR'.
  APPEND lwa_fieldcat TO ot_fieldcat.

  CLEAR: lwa_fieldcat.
  lwa_fieldcat-tabname        = 'git_saida'.
  lwa_fieldcat-fieldname      = 'BLDAT'.
  lwa_fieldcat-seltext_m      = 'Dt.Nota'.
  lwa_fieldcat-ddictxt        = 'M'.
  lwa_fieldcat-ref_tabname    = 'BSAK'.
  lwa_fieldcat-ref_fieldname  = 'BLDAT'.
  APPEND lwa_fieldcat TO ot_fieldcat.

  CLEAR: lwa_fieldcat.
  lwa_fieldcat-tabname        = 'git_saida'.
  lwa_fieldcat-fieldname      = 'ZUONR'.
  lwa_fieldcat-seltext_m      = 'Pedido'.
  lwa_fieldcat-ddictxt        = 'M'.
  lwa_fieldcat-ref_tabname    = 'BSAK'.
  lwa_fieldcat-ref_fieldname  = 'ZUONR'.
  APPEND lwa_fieldcat TO ot_fieldcat.

  CLEAR: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'DMBTR'.
  lwa_fieldcat-seltext_m     = 'Valor BRL'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-ref_tabname   = 'BSAK'.
  lwa_fieldcat-ref_fieldname = 'DMBTR'.
  APPEND lwa_fieldcat TO ot_fieldcat.

  CLEAR: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'ZFBDT'.
  lwa_fieldcat-seltext_m     = 'Dt.Vencimento'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-ref_tabname   = 'BSAK'.
  lwa_fieldcat-ref_fieldname = 'ZFBDT'.
  APPEND lwa_fieldcat TO ot_fieldcat.

  CLEAR: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'AUGBL'.
  lwa_fieldcat-seltext_m     = 'Doc.Comp.'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-hotspot       = 'X'.
  lwa_fieldcat-ref_tabname   = 'ZFIT0170'.
  lwa_fieldcat-ref_fieldname = 'AUGBL'.
  APPEND lwa_fieldcat TO ot_fieldcat.
ENDFORM.

FORM html_top_of_page USING o_dd_left TYPE REF TO cl_dd_document.

  DATA: o_dd_right TYPE REF TO cl_dd_area.
  DATA: o_dd_middle TYPE REF TO cl_dd_area.

  CALL METHOD o_dd_left->vertical_split
    EXPORTING
      split_area  = o_dd_left
      split_width = '70%'
    IMPORTING
      right_area  = o_dd_right.

  CALL METHOD o_dd_left->vertical_split
    EXPORTING
      split_area  = o_dd_left
      split_width = '50%'
    IMPORTING
      right_area  = o_dd_middle.

  DATA: lit_txt TYPE sdydo_text_table,
        lwa_txt LIKE LINE OF lit_txt.


  CALL METHOD o_dd_left->add_text
    EXPORTING
      text         = 'Parâmetros:'
      sap_style    = space
      sap_color    = space
      sap_fontsize = cl_dd_document=>medium
      sap_emphasis = cl_dd_document=>strong
      style_class  = space.

  CALL METHOD o_dd_left->new_line.

  CONCATENATE 'Empresa Pagadora: '  space p_bukrs-low INTO lwa_txt.
  APPEND  lwa_txt TO lit_txt.

  IF p_date-high IS INITIAL.
    CONCATENATE 'Data  Recebimento' space p_date-low+6(2) '.' p_date-low+4(2) '.' p_date-low(4) INTO lwa_txt.
    APPEND  lwa_txt TO lit_txt.
  ELSE.
    CONCATENATE 'Data  Recebimento:'  space ' De: ' space p_date-low+6(2) '.' p_date-low+4(2) '.' p_date-low(4) space
    ' até :' space p_date-high+6(2) '.' p_date-high+4(2) '.' p_date-high(4) INTO lwa_txt.
    APPEND  lwa_txt TO lit_txt.
  ENDIF.

  CALL METHOD o_dd_left->add_text
    EXPORTING
      text_table = lit_txt
      fix_lines  = 'X'.

  CALL METHOD o_dd_right->new_line.

  CALL METHOD o_dd_right->add_icon(
    EXPORTING
      tabindex = 1
      sap_icon = 'ICON_COMPLETE' ).

  CALL METHOD o_dd_right->add_text
    EXPORTING
      text = ' - Faturas enviadas para Pagamento'.

  CALL METHOD o_dd_right->new_line.

  CALL METHOD o_dd_right->add_icon(
    EXPORTING
      tabindex = 1
      sap_icon = 'ICON_LED_YELLOW' ).

  CALL METHOD o_dd_right->add_text
    EXPORTING
      text = ' - Faturas aguardando Gerar Arquivo de Pagamento – dentro do prazo de vencimento'.

  CALL METHOD o_dd_right->new_line.

  CALL METHOD o_dd_right->add_icon(
    EXPORTING
      tabindex = 1
      sap_icon = 'ICON_DEFECT' ).

  CALL METHOD o_dd_right->add_text
    EXPORTING
      text = ' - Faturas Faturas aguardando Gerar Arquivo de Pagamento – Fora do prazo de vencimento'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SET_STATUS
*&---------------------------------------------------------------------*
FORM set_status USING t_extab TYPE slis_t_extab.
  SET PF-STATUS 'PF_ZFIR0092_S' EXCLUDING fcode.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command USING VALUE(iv_ucom) LIKE sy-ucomm
                             is_selfield TYPE slis_selfield.
  CASE iv_ucom.
    WHEN '&IC1'.
      PERFORM display_records USING is_selfield.
    WHEN 'BACK'.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      LEAVE SCREEN.

  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RECORDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_records  USING is_selfield TYPE slis_selfield.

  DATA: lva_gjahr TYPE bsik-gjahr.
  CLEAR: gwa_saida.

  IF is_selfield-fieldname = 'BELNR'.
    READ TABLE git_saida INTO gwa_saida INDEX is_selfield-tabindex.

    CHECK gwa_saida-belnr IS NOT INITIAL.

    lva_gjahr = gwa_saida-budat(4).

    SET PARAMETER ID 'BUK'  FIELD  gwa_saida-bukrs.
    SET PARAMETER ID 'BLN'  FIELD  gwa_saida-belnr.
    SET PARAMETER ID 'GJR'  FIELD  lva_gjahr .
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

  ELSE.
    IF is_selfield-fieldname = 'AUGBL'.
      READ TABLE git_saida INTO gwa_saida INDEX is_selfield-tabindex.

      CHECK gwa_saida-augbl IS NOT INITIAL.

      lva_gjahr = gwa_saida-budat(4).

      SET PARAMETER ID 'BUK'  FIELD  gwa_saida-bukrs.
      SET PARAMETER ID 'BLN'  FIELD  gwa_saida-augbl.
      SET PARAMETER ID 'GJR'  FIELD  lva_gjahr .
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

    ENDIF.
  ENDIF.
ENDFORM.

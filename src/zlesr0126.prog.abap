*&---------------------------------------------------------------------*
*& Report  ZLESR0126
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zlesr0126.

TABLES: j_1bnfdoc.


TYPES: BEGIN OF ty_saida,
         docdat                     TYPE j_1bnfdoc-docdat,
         tknum                      TYPE zcte_ciot-tknum,
         nfenum                     TYPE j_1bnfdoc-nfenum,
         nucontrato                 TYPE zcte_ciot-nucontrato,
         tp_plano_administradora(5) TYPE c,
         vlr_frete                  TYPE zcte_ciot-vlr_frete,
         seguro_iof                 TYPE zcte_ciot-vlr_seguro,
         imp_retidos                TYPE zcte_ciot-vlr_inss,
         patio_triagem              TYPE zcte_ciot-vlr_inss,
         vlr_frete_pagar            TYPE zcte_ciot-vlr_frete,
         vcto_itiner                TYPE j_1bnfdoc-docdat, "data de emissão + dias de percurso
         dt_entrega                 TYPE zpfe_lote_item-dt_chegada,
         dt_pgto_adto               TYPE zpfe_lote-augdt,
         quebra_perda               TYPE zpfe_lote_item-vl_pago_lote,
         vlr_adto                   TYPE zpfe_lote_item-vl_pago_lote,
         dt_pgto_saldo              TYPE zpfe_lote-augdt,
         vl_saldo                   TYPE zpfe_lote_item-vl_pago_lote,
         saldo_pagar                TYPE zcte_ciot-vlr_frete,
       END OF ty_saida.

TYPES: BEGIN OF ty_j_1bnfdoc,
         bukrs  TYPE j_1bnfdoc-bukrs,
         branch TYPE j_1bnfdoc-branch,
         model  TYPE j_1bnfdoc-model,
         docdat TYPE j_1bnfdoc-docdat,
         cancel TYPE j_1bnfdoc-cancel,
         doctyp TYPE j_1bnfdoc-doctyp,
         docnum TYPE j_1bnfdoc-docnum,
         nfenum TYPE j_1bnfdoc-nfenum,
       END OF ty_j_1bnfdoc.

TYPES: BEGIN OF ty_zcte_ciot,
         docnum                  TYPE zcte_ciot-docnum,
         nucontrato              TYPE zcte_ciot-nucontrato,
         tknum                   TYPE zcte_ciot-tknum,
         tp_plano_administradora TYPE zcte_ciot-tp_plano_administradora,
         vlr_frete               TYPE zcte_ciot-vlr_frete,
         vlr_seguro              TYPE zcte_ciot-vlr_seguro,
         vlr_iof                 TYPE zcte_ciot-vlr_iof,
         vlr_inss                TYPE zcte_ciot-vlr_inss,
         vlr_iss                 TYPE zcte_ciot-vlr_iss,
         vlr_irpf                TYPE zcte_ciot-vlr_irpf,
         vlr_sest                TYPE zcte_ciot-vlr_sest,
         vlr_triagem             TYPE zcte_ciot-vlr_triagem,
       END OF ty_zcte_ciot.

TYPES: BEGIN OF ty_vttk,
         tknum  TYPE vttk-tknum,
         route  TYPE vttk-route,
         gesztd TYPE vttk-gesztd,
       END OF ty_vttk.

TYPES: BEGIN OF ty_zpfe_lote_item,
         nucontrato              TYPE zpfe_lote_item-nucontrato,
         chvid                   TYPE zpfe_lote_item-chvid,
         vl_pago_lote            TYPE zpfe_lote_item-vl_pago_lote,
         dt_chegada              TYPE zpfe_lote_item-dt_chegada,
         nm_lote                 TYPE zpfe_lote_item-nm_lote,
         nr_lote_adm             TYPE zpfe_lote_item-nr_lote_adm,
         dt_transacao            TYPE zpfe_lote_item-dt_transacao,
         tp_plano_administradora TYPE zpfe_lote_item-tp_plano_administradora,
       END OF ty_zpfe_lote_item.

TYPES: BEGIN OF ty_zpfe_lote,
         nm_lote     TYPE zpfe_lote-nm_lote,
         nr_lote_adm TYPE zpfe_lote-nr_lote_adm,
         belnr       TYPE zpfe_lote-belnr,
         gjahr       TYPE zpfe_lote-gjahr,
         bukrs       TYPE zpfe_lote-bukrs,
       END OF ty_zpfe_lote.


TYPES: BEGIN OF ty_tab_aux,
         nucontrato     TYPE  zpfe_lote_item-nucontrato,
         chvid          TYPE zpfe_lote_item-chvid,
         vl_pago_lote   TYPE zpfe_lote_item-vl_pago_lote,
         nat_chvid      TYPE  zlest0025-naturezachvid,
         vl_pago_lote_2 TYPE zcte_ciot-vlr_frete,
         ctlglancto     TYPE zlest0025-ctlglancto,
         dt_chegada     TYPE zpfe_lote_item-dt_chegada,
         nm_lote        TYPE zpfe_lote_item-nm_lote,
         nr_lote_adm    TYPE zpfe_lote_item-nr_lote_adm,
         tipoconta_d    TYPE zlest0018-tipoconta_d,
         tipoconta_c    TYPE zlest0018-tipoconta_c,
       END OF ty_tab_aux.


DATA: it_saida          TYPE TABLE OF ty_saida,
      wa_saida          TYPE ty_saida,
      it_j_1bnfdoc      TYPE TABLE OF ty_j_1bnfdoc,
      wa_j_1bnfdoc      TYPE ty_j_1bnfdoc,
      it_zcte_ciot      TYPE TABLE OF ty_zcte_ciot,
      wa_zcte_ciot      TYPE ty_zcte_ciot,
      it_vttk           TYPE TABLE OF ty_vttk,
      wa_vttk           TYPE ty_vttk,
      it_zpfe_lote_item TYPE TABLE OF ty_zpfe_lote_item,
      wa_zpfe_lote_item TYPE ty_zpfe_lote_item,
      it_zpfe_lote      TYPE TABLE OF ty_zpfe_lote,
      wa_zpfe_lote      TYPE ty_zpfe_lote,
      it_tab_aux        TYPE TABLE OF ty_tab_aux,
      wa_tab_aux        TYPE  ty_tab_aux.


DATA: it_fieldcatalog TYPE lvc_t_fcat,
      wa_fieldcatalog TYPE lvc_s_fcat,
      wa_stable       TYPE lvc_s_stbl.

DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
      dg_splitter_1      TYPE REF TO cl_gui_splitter_container,
      dg_parent_1        TYPE REF TO cl_gui_container,
      dg_splitter_2      TYPE REF TO cl_gui_splitter_container,
      dg_parent_2        TYPE REF TO cl_gui_container,
      dg_parent_2a       TYPE REF TO cl_gui_container,
      dg_parent_alv      TYPE REF TO cl_gui_container,
      gs_layout          TYPE lvc_s_layo,
      gs_variant         TYPE disvariant,
      it_exclude_fcode   TYPE ui_functions,
      wa_exclude_fcode   LIKE LINE OF it_exclude_fcode,
      dg_dyndoc_id       TYPE REF TO cl_dd_document,
      ctl_alv            TYPE REF TO cl_gui_alv_grid,
      ctl_alv02          TYPE REF TO cl_gui_alv_grid,
      table_element      TYPE REF TO cl_dd_table_element,
      column             TYPE REF TO cl_dd_area,
      table_element2     TYPE REF TO cl_dd_table_element,
      column_1           TYPE REF TO cl_dd_area,
      column_2           TYPE REF TO cl_dd_area,
      dg_html_cntrl      TYPE REF TO cl_gui_html_viewer.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:   p_bukrs   FOR j_1bnfdoc-bukrs  OBLIGATORY NO-EXTENSION NO INTERVALS,
                  p_branch  FOR j_1bnfdoc-branch OBLIGATORY NO-EXTENSION NO INTERVALS,
                  p_data    FOR j_1bnfdoc-docdat OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.


INITIALIZATION.

START-OF-SELECTION.

  PERFORM busca_dados.
  PERFORM tratar_dados.
  PERFORM alv.

END-OF-SELECTION.


FORM busca_dados.

  SELECT bukrs branch model docdat cancel doctyp docnum
         nfenum
    FROM j_1bnfdoc INTO TABLE it_j_1bnfdoc
    WHERE bukrs  IN p_bukrs
    AND   branch IN p_branch
    AND   model  EQ '57'
    AND   docdat IN p_data
    AND   cancel EQ ' '
    AND   doctyp <> '5'.

  IF it_j_1bnfdoc[] IS NOT INITIAL.

    SELECT  docnum
            nucontrato
            tknum
            tp_plano_administradora
            vlr_frete
            vlr_seguro
            vlr_iof
            vlr_inss
            vlr_iss
            vlr_irpf
            vlr_sest
            vlr_triagem
      FROM zcte_ciot INTO TABLE it_zcte_ciot
      FOR ALL ENTRIES IN it_j_1bnfdoc
      WHERE docnum EQ it_j_1bnfdoc-docnum.


    IF it_zcte_ciot[] IS NOT INITIAL.

      SELECT tknum
             route
             gesztd
        FROM vttk INTO TABLE it_vttk
        FOR ALL ENTRIES IN it_zcte_ciot
      WHERE  tknum EQ  it_zcte_ciot-tknum.


      SELECT  nucontrato  chvid  vl_pago_lote  dt_chegada   nm_lote
              nr_lote_adm dt_transacao tp_plano_administradora
        FROM zpfe_lote_item INTO TABLE it_zpfe_lote_item
        FOR ALL ENTRIES IN it_zcte_ciot
        WHERE nucontrato EQ it_zcte_ciot-nucontrato.


      IF it_zpfe_lote_item[] IS NOT INITIAL.

        SELECT  nm_lote
                nr_lote_adm
                belnr
                gjahr
                bukrs
          FROM zpfe_lote INTO TABLE it_zpfe_lote
          FOR ALL ENTRIES IN it_zpfe_lote_item
        WHERE nm_lote     EQ it_zpfe_lote_item-nm_lote
         AND  nr_lote_adm EQ it_zpfe_lote_item-nr_lote_adm.


        SELECT  * FROM zlest0025 INTO TABLE @DATA(it_zlest0025)
          FOR ALL ENTRIES IN @it_zpfe_lote_item
          WHERE chvid EQ @it_zpfe_lote_item-chvid.

        SELECT * FROM zlest0018 INTO TABLE @DATA(it_zlest0018)
          FOR ALL ENTRIES IN @it_zlest0025
          WHERE chvid       EQ @it_zlest0025-chvid
          AND   tiptransp   EQ ' '
          AND   ctlglancto  IN ('VA', 'P', 'Q')
          AND   tipcontabil EQ 'FS'
          AND   bukrs       EQ ' '.


        LOOP AT it_zpfe_lote_item INTO wa_zpfe_lote_item.

          READ TABLE it_zlest0025 INTO DATA(wa_zlest0025) WITH KEY chvid = wa_zpfe_lote_item-chvid.
          IF sy-subrc = 0.
            wa_tab_aux-ctlglancto        = wa_zlest0025-ctlglancto.
            wa_tab_aux-nucontrato        = wa_zpfe_lote_item-nucontrato.
            wa_tab_aux-chvid             = wa_zpfe_lote_item-chvid.
            wa_tab_aux-vl_pago_lote      = wa_zpfe_lote_item-vl_pago_lote.
            wa_tab_aux-nat_chvid         = wa_zlest0025-naturezachvid.
            wa_tab_aux-vl_pago_lote_2    = wa_zpfe_lote_item-vl_pago_lote.
            wa_tab_aux-dt_chegada        = wa_zpfe_lote_item-dt_chegada.
            wa_tab_aux-nm_lote           = wa_zpfe_lote_item-nm_lote.
            wa_tab_aux-nr_lote_adm       = wa_zpfe_lote_item-nr_lote_adm.

            READ TABLE it_zlest0018 INTO DATA(wa_zlest0018) WITH KEY chvid = wa_zlest0025-chvid.
            IF sy-subrc = 0.
              wa_tab_aux-tipoconta_c  = wa_zlest0018-tipoconta_c.
              wa_tab_aux-tipoconta_d  = wa_zlest0018-tipoconta_d.
            ENDIF.

            APPEND wa_tab_aux TO it_tab_aux.
            CLEAR: wa_zpfe_lote_item,wa_tab_aux,wa_zlest0025, wa_zlest0018.
          ENDIF.

        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

FORM tratar_dados.
  DATA vdias TYPE c LENGTH 100.

  LOOP AT  it_j_1bnfdoc INTO wa_j_1bnfdoc.

    wa_saida-docdat   = wa_j_1bnfdoc-docdat.
    wa_saida-nfenum   = wa_j_1bnfdoc-nfenum.

    READ TABLE it_zcte_ciot INTO wa_zcte_ciot WITH KEY docnum = wa_j_1bnfdoc-docnum.
    IF sy-subrc = 0.
      wa_saida-tknum           =  wa_zcte_ciot-tknum.
      wa_saida-nucontrato      =  wa_zcte_ciot-nucontrato.
      wa_saida-vlr_frete       =  wa_zcte_ciot-vlr_frete.
      wa_saida-seguro_iof      = ( wa_zcte_ciot-vlr_seguro + wa_zcte_ciot-vlr_iof  ) * -1.
      wa_saida-imp_retidos     = ( wa_zcte_ciot-vlr_inss   + wa_zcte_ciot-vlr_irpf  + wa_zcte_ciot-vlr_sest + wa_zcte_ciot-vlr_iss ) * -1.
      wa_saida-patio_triagem   = wa_zcte_ciot-vlr_triagem.
      wa_saida-vlr_frete_pagar = wa_saida-vlr_frete +   wa_saida-seguro_iof +   wa_saida-imp_retidos.

      IF  wa_zcte_ciot-tp_plano_administradora = '02'.
        wa_saida-tp_plano_administradora = 'Pré'.
      ELSE.
        wa_saida-tp_plano_administradora = 'Pós'.
      ENDIF.

      READ TABLE it_vttk INTO wa_vttk WITH KEY tknum = wa_zcte_ciot-tknum.
      IF sy-subrc = 0.

        PERFORM f_atrib_dt_saldo_frete USING wa_vttk-gesztd
                                     CHANGING  wa_saida-vcto_itiner.
      ENDIF.


      READ TABLE it_zpfe_lote_item INTO wa_zpfe_lote_item WITH KEY nucontrato = wa_zcte_ciot-nucontrato.
      IF sy-subrc = 0.
        wa_saida-dt_entrega  = wa_zpfe_lote_item-dt_chegada.

        LOOP AT  it_tab_aux INTO wa_tab_aux WHERE nucontrato = wa_zcte_ciot-nucontrato.

          IF ( wa_tab_aux-ctlglancto = 'Q' OR  wa_tab_aux-ctlglancto = 'P' ) .
            wa_saida-quebra_perda =  wa_saida-quebra_perda + wa_tab_aux-vl_pago_lote_2.
          ENDIF.

          IF  ( wa_tab_aux-chvid <> '1' ).
            IF ( wa_tab_aux-tipoconta_d = 'FS' OR   wa_tab_aux-tipoconta_c = 'FS' ).
              wa_saida-vl_saldo =  wa_saida-vl_saldo + wa_tab_aux-vl_pago_lote_2.
            ENDIF.
          ENDIF.

        ENDLOOP.

        IF wa_zpfe_lote_item-chvid = '1'.
          wa_saida-vlr_adto = wa_zpfe_lote_item-vl_pago_lote.
        ENDIF.

        READ TABLE it_zpfe_lote INTO wa_zpfe_lote WITH KEY  nm_lote       = wa_zpfe_lote_item-nm_lote
                                                            nr_lote_adm   = wa_zpfe_lote_item-nr_lote_adm.
        IF sy-subrc = 0.
          SELECT SINGLE * FROM bsak INTO @DATA(wa_bsak)
            WHERE bukrs EQ @wa_zpfe_lote-bukrs
            AND   gjahr EQ @wa_zpfe_lote-gjahr
            AND   belnr EQ @wa_zpfe_lote-belnr
            AND   shkzg EQ 'H'.
          wa_saida-dt_pgto_adto   = wa_bsak-augdt.
        ENDIF.
      ENDIF.


      LOOP AT it_zpfe_lote_item INTO DATA(wa_zpfe_lote_itemv) WHERE  nucontrato = wa_zcte_ciot-nucontrato.

        IF wa_zpfe_lote_itemv-chvid EQ '1'.
          READ TABLE it_zpfe_lote INTO DATA(ws_zpfe_lote) WITH KEY nm_lote = wa_zpfe_lote_itemv-nm_lote
                                                               nr_lote_adm = wa_zpfe_lote_itemv-nr_lote_adm.
          IF sy-subrc EQ 0.
            IF wa_zpfe_lote_itemv-tp_plano_administradora EQ '02'.
              wa_saida-dt_pgto_adto   = wa_zpfe_lote_itemv-dt_transacao.
            ENDIF.
          ENDIF.
        ENDIF.


        IF wa_zpfe_lote_itemv-chvid <> '1'.

          READ TABLE it_zpfe_lote INTO wa_zpfe_lote WITH KEY  nm_lote       = wa_zpfe_lote_itemv-nm_lote
                                                              nr_lote_adm   = wa_zpfe_lote_itemv-nr_lote_adm.
          IF sy-subrc = 0.
            IF wa_zpfe_lote_itemv-tp_plano_administradora EQ '02'.
              wa_saida-dt_pgto_saldo   = wa_zpfe_lote_itemv-dt_transacao.

            ELSE.

              SELECT SINGLE * FROM bsak INTO wa_bsak
                WHERE bukrs EQ wa_zpfe_lote-bukrs
                AND   gjahr EQ wa_zpfe_lote-gjahr
                AND   belnr EQ wa_zpfe_lote-belnr
                AND   shkzg EQ 'H'.
              wa_saida-dt_pgto_saldo  = wa_bsak-augdt.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      wa_saida-saldo_pagar = ( wa_saida-vlr_frete_pagar -  ( wa_saida-vlr_adto - wa_saida-quebra_perda + wa_saida-vl_saldo ) ).

      APPEND wa_saida TO it_saida.
    ENDIF.

    CLEAR: wa_saida, wa_zpfe_lote, wa_zpfe_lote_item, wa_zcte_ciot, wa_vttk , wa_j_1bnfdoc, wa_tab_aux.

  ENDLOOP.

ENDFORM.

FORM f_atrib_dt_saldo_frete USING vtraztd     TYPE tvro-traztd
                         CHANGING vcto_itiner TYPE sy-datum.

  DATA: v_traztd   TYPE c LENGTH 100,
        v_traztd_i TYPE c LENGTH 100,
        v_dias_i   TYPE i,
        v_dias     TYPE c LENGTH 100.


  v_traztd_i  = vtraztd.

  CHECK v_traztd_i IS NOT INITIAL.

  REPLACE ALL OCCURRENCES OF '.' IN v_traztd_i WITH  ''.
  CONDENSE v_traztd_i NO-GAPS.

  CALL FUNCTION 'CONVERSION_EXIT_TSTRG_OUTPUT'
    EXPORTING
      input  = v_traztd_i
    IMPORTING
      output = v_dias.

  REPLACE ALL OCCURRENCES OF  ',' IN v_dias WITH '.'.
  CONDENSE v_dias NO-GAPS.

  v_dias_i = v_dias.

  CHECK v_dias_i > 0.

  vcto_itiner = wa_saida-docdat + v_dias_i.

ENDFORM.


FORM alv.

  PERFORM preenche_cat USING:

       'DOCDAT'                     'Data Emissão'           '12'    ''    ''    ''    '',
       'TKNUM'                      'Doc.Transp.'            '12'    ''    ''    ''    '',
       'NFENUM'                     'Dacte'                  '10'    ''    ''    ''    '',
       'NUCONTRATO'                 'Contrato'               '12'    ''    ''    ''    '',
       'TP_PLANO_ADMINISTRADORA'    'Plano Adm.'             '10'    ''    ''    ''    '',
       'VLR_FRETE'                  'Valor Frete'            '12'    ''    ''    ''    '',
       'SEGURO_IOF'                 'Seguro/IOF'             '12'    ''    ''    ''    '',
       'IMP_RETIDOS'                'Impostos Retidos'       '12'    ''    ''    ''    '',
       'VLR_FRETE_PAGAR'            'Valor Frete a Pagar'    '14'    ''    ''    ''    '',
       'PATIO_TRIAGEM'              'Pátio Triagem'          '12'    ''    ''    ''    '',
       'VCTO_ITINER'                'Venc.Itinerario'        '15'    ''    ''    ''    '',
       'DT_ENTREGA'                 'Data Entrega'           '12'    ''    ''    ''    '',
       'DT_PGTO_ADTO'               'Data Pagt. Adto'        '15'    ''    ''    ''    '',
       'VLR_ADTO'                   'Valor Adto.'            '12'    ''    ''    ''    '',
       'DT_PGTO_SALDO'              'Data Pagt. Saldo'       '15'    ''    ''    ''    '',
       'QUEBRA_PERDA'               'Quebra Perda'           '15'    ''    ''    ''    '',
       'VL_SALDO'                   'Valor Saldo'            '12'    ''    ''    ''    '',
       'SALDO_PAGAR'                'Saldo a Pagar'          '12'    ''    ''    ''    ''.

  CALL SCREEN 0100.

ENDFORM.



FORM preenche_cat USING VALUE(p_campo)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_zero)
                        VALUE(p_hot)
                        VALUE(p_sum)
                        VALUE(p_just).

  wa_fieldcatalog-fieldname = p_campo.
  wa_fieldcatalog-coltext   = p_desc.
  wa_fieldcatalog-scrtext_l = p_desc.
  wa_fieldcatalog-scrtext_m = p_desc.
  wa_fieldcatalog-scrtext_s = p_desc.


  wa_fieldcatalog-outputlen = p_tam.
  wa_fieldcatalog-hotspot   = p_hot.
  wa_fieldcatalog-no_zero   = p_zero.
  wa_fieldcatalog-do_sum    = p_sum.
  wa_fieldcatalog-just      = p_just.

  APPEND wa_fieldcatalog TO it_fieldcatalog.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0100'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.

ENDFORM.
*&--------------


MODULE status_0100 OUTPUT.
  DATA: data_ini(10)            TYPE c,
        data_fim(10)            TYPE c,
        p_text                  TYPE sdydo_text_element,
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table,
        vl_cont                 TYPE i,
        vl_gtext                TYPE tgsbt-gtext.

  DATA: vl_butxt LIKE t001-butxt.

  SET PF-STATUS 'STATUS'.
  SET TITLEBAR 'TITULO'.


  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_1
        rows    = 1
        columns = 1.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 16.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 0.

    PERFORM fill_gs_variant.

    gs_layout-sel_mode   = 'A'.
    gs_layout-stylefname = 'CELLSTYLES'.

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = dg_parent_alv.

    "SET HANDLER: LCL_EVENT_RECEIVER=>ZM_HANDLE_HOTSPOT_REPORT FOR CTL_ALV.

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
        is_variant      = gs_variant
        "IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog
        it_outtab       = it_saida.

    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

    CALL METHOD dg_dyndoc_id->initialize_document.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 1
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element.

    CALL METHOD table_element->add_column
      IMPORTING
        column = column.

    CALL METHOD table_element->set_column_style
      EXPORTING
        col_no    = 1
        sap_align = 'LEFT'
        sap_style = cl_dd_document=>heading.

    p_text = text-002.

    CALL METHOD column->add_text
      EXPORTING
        text      = p_text
        sap_style = 'HEADING'.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 2
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element2.

    CALL METHOD table_element2->add_column
      EXPORTING
        sap_style   = 'SAP_BOLD'
        style_class = 'SAP_BOLD'
      IMPORTING
        column      = column_1.

    CLEAR: p_text_table.

    CALL METHOD column_1->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD table_element2->add_column
      IMPORTING
        column = column_2.

    CALL METHOD table_element2->set_column_style
      EXPORTING
        col_no       = 2
        sap_align    = 'LEFT'
        sap_fontsize = cl_dd_document=>medium.

    CLEAR: p_text_table.

    LOOP AT p_bukrs.
      IF p_bukrs-option NE 'EQ' AND p_bukrs-option NE 'BT'.
        sdydo_text_element = 'Empresa: Multiplas Seleções'.
        EXIT.
      ELSEIF p_bukrs-option EQ 'BT'.

        SELECT SINGLE butxt FROM t001  INTO vl_butxt
          WHERE bukrs EQ p_bukrs-low
          AND spras EQ sy-langu.

        CONCATENATE 'Empresa:' p_bukrs-low vl_butxt '-' INTO sdydo_text_element SEPARATED BY space.
        CLEAR: vl_butxt.

        SELECT SINGLE butxt  FROM t001  INTO vl_butxt
          WHERE bukrs EQ p_bukrs-low
          AND spras EQ sy-langu.

        CONCATENATE sdydo_text_element p_bukrs-high vl_butxt INTO sdydo_text_element SEPARATED BY space.

        EXIT.
      ELSE.
        vl_cont = vl_cont + 1.
        IF vl_cont GT 1.
          sdydo_text_element = 'Empresa: Multiplas Seleções'.
        ELSE.
          SELECT SINGLE butxt  FROM t001 INTO vl_butxt
          WHERE bukrs EQ p_bukrs-low
          AND spras EQ sy-langu.
          CONCATENATE 'Empresa:' p_bukrs-low vl_butxt INTO sdydo_text_element SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDLOOP.

    APPEND sdydo_text_element TO p_text_table.
    CLEAR: vl_cont, vl_butxt, sdydo_text_element.

    LOOP AT p_branch.
      IF p_branch-option NE 'EQ' AND p_branch-option NE 'BT'.
        sdydo_text_element = 'Filial: Multiplas Seleções'.
        EXIT.
      ELSEIF p_branch-option EQ 'BT'.

        SELECT SINGLE * FROM t001w INTO @DATA(w_t001w)
          WHERE werks EQ @p_branch-low.

        CONCATENATE 'Filial:' p_branch-low  w_t001w-name1 '-' INTO sdydo_text_element SEPARATED BY space.
        CLEAR: w_t001w.

        SELECT SINGLE * FROM t001w INTO w_t001w
          WHERE werks EQ p_branch-high.

        CONCATENATE sdydo_text_element p_branch-high  w_t001w-name1 INTO sdydo_text_element SEPARATED BY space.
        EXIT.
      ELSE.
        vl_cont = vl_cont + 1.
        IF vl_cont GT 1.
          sdydo_text_element = 'Filial: Multiplas Seleções'.
        ELSE.
          SELECT SINGLE * FROM t001w INTO w_t001w
            WHERE werks EQ p_branch-low.

          CONCATENATE 'Filial:' p_branch-low  w_t001w-name1 INTO sdydo_text_element SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDLOOP.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR: vl_cont, w_t001w, sdydo_text_element.


    CONCATENATE  p_data-low+6(2)  '.' p_data-low+4(2)  '.' p_data-low(4)   INTO data_ini.
    CONCATENATE  p_data-high+6(2) '.' p_data-high+4(2) '.' p_data-high(4)  INTO data_fim.

    CONCATENATE 'Data Emissão:' data_ini '-' data_fim INTO sdydo_text_element SEPARATED BY space.
    APPEND sdydo_text_element TO p_text_table.

    CALL METHOD column_2->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD dg_dyndoc_id->merge_document.

    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_2.

    dg_dyndoc_id->html_control = dg_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = dg_parent_2
      EXCEPTIONS
        html_display_error = 1.

  ELSE.
    CALL METHOD ctl_alv->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

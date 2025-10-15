*&---------------------------------------------------------------------*
*& Report  ZMMR029
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zmmr029.

TABLES: ekko,
        lfa1,
        rbkp,
        j_1bbranch.

CONSTANTS: c_x TYPE c LENGTH 1 VALUE 'X'.

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler_alv DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click_alv FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler_alv IMPLEMENTATION.
  METHOD handle_hotspot_click_alv.
    PERFORM handle_hotspot_click_alv
       USING es_row_no-row_id
             e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

TYPES: BEGIN OF tp_saida.
         INCLUDE STRUCTURE zmmt_ee_zgr.
TYPES:   av_vbeln     TYPE vbeln_vl,
         mm_mblnr	    TYPE mblnr,
         mm_mjahr	    TYPE mjahr,
         ft_belnr	    TYPE re_belnr,
         ft_gjahr	    TYPE gjahr,
         docnum	      TYPE j_1bdocnum,
         name1        TYPE name1_gp,
         data_usuario TYPE bldat,
         data_lavto   TYPE budat,
         tknum        TYPE tknum,
         fknum        TYPE fknum,
         netpr        TYPE netpr.
TYPES: END OF tp_saida.

DATA: it_interface      TYPE TABLE OF zmmt_ee_zgr      WITH HEADER LINE,
      it_interface_docs TYPE TABLE OF zmmt_ee_zgr_docs WITH HEADER LINE,
      it_saida          TYPE TABLE OF tp_saida         WITH HEADER LINE,
      ok_code           TYPE sy-ucomm,
      prim_alv          TYPE c LENGTH 1,
      catalogo_alv      TYPE lvc_t_fcat,
      container_alv     TYPE REF TO cl_gui_custom_container,
      alv               TYPE REF TO cl_gui_alv_grid,
      gs_layout         TYPE lvc_s_layo,
      scroll_col        TYPE lvc_s_col,
      scroll_row        TYPE lvc_s_roid,
      event_handler_alv TYPE REF TO lcl_event_handler_alv,
      vg_tem_permi      TYPE c LENGTH 1,
      it_tipo_operacao  TYPE TABLE OF lxhme_range_c2 WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_empre  FOR j_1bbranch-bukrs  OBLIGATORY,
                  p_filia  FOR j_1bbranch-branch OBLIGATORY,
                  p_pedid  FOR ekko-ebeln,
                  p_forne  FOR lfa1-lifnr,
                  p_miro   FOR rbkp-belnr,
                  p_miroa  FOR rbkp-gjahr OBLIGATORY NO-EXTENSION NO INTERVALS ,
                  p_nunser FOR rbkp-xblnr,
                  p_data   FOR rbkp-bldat OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.

  CLEAR vg_tem_permi.

  p_data-sign	  = 'I'.
  p_data-option	= 'BT'.

  CONCATENATE sy-datum(6) '01' INTO p_data-low.
  p_data-high =  p_data-low + 32.
  CONCATENATE p_data-high(6) '01' INTO p_data-high.
  p_data-high = p_data-high - 1.
  APPEND p_data.

  p_miroa-sign    = 'I'.
  p_miroa-option  = 'EQ'.
  p_miroa-low = sy-datum(4).
  APPEND p_miroa.

  it_tipo_operacao-sign    = 'I'.
  it_tipo_operacao-option  = 'EQ'.
  it_tipo_operacao-low     = '01'.
  APPEND it_tipo_operacao.

  it_tipo_operacao-sign    = 'I'.
  it_tipo_operacao-option  = 'EQ'.
  it_tipo_operacao-low     = '08'.
  APPEND it_tipo_operacao.

  "  it_tipo_operacao-low     = '02'.
  "  append it_tipo_operacao.

  AUTHORITY-CHECK OBJECT 'ZAVISOXI' ID 'ZAVISOXI' FIELD c_x.
  IF sy-subrc IS INITIAL.
    vg_tem_permi = 'X'.
  ENDIF.

START-OF-SELECTION.

  PERFORM pesquisar.

  CALL SCREEN 0001.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  DATA: wa_okcode TYPE sy-ucomm,
        it_okcode TYPE TABLE OF sy-ucomm.

  CLEAR: it_okcode.

  IF vg_tem_permi IS INITIAL.
    wa_okcode = 'AVISO'.
    APPEND wa_okcode TO it_okcode.
  ENDIF.

  SET PF-STATUS 'PF0001' EXCLUDING it_okcode.
  SET TITLEBAR 'TL0001'.

  PERFORM cria_alv.

ENDMODULE.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001_ext INPUT.

  CASE ok_code.
    WHEN 'BACK' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0001_EXT  INPUT

*&---------------------------------------------------------------------*
* Alimentar a tabela interna de estrutura fieldcat.
*----------------------------------------------------------------------*
FORM z_estrutura_fieldcat TABLES it_catalogo TYPE lvc_t_fcat
                           USING p_tab_name
                                 p_fieldname
                                 p_texto_grande
                                 p_hot
                                 p_posicao
                                 p_outputlen
                                 p_fix_column
                                 p_convexit
                                 p_do_sum
                                 p_icon
                                 p_just
                                 p_emphasize
                                 p_edit.
  DATA catalog TYPE lvc_s_fcat.
  CLEAR catalog.
  catalog-tabname     = p_tab_name.
  catalog-fieldname   = p_fieldname.
  catalog-scrtext_l   = p_texto_grande.
  catalog-scrtext_m   = p_texto_grande.
  catalog-scrtext_s   = p_texto_grande.
  catalog-hotspot     = p_hot.
  catalog-col_pos     = p_posicao.
  catalog-outputlen   = p_outputlen.
  catalog-fix_column  = p_fix_column.
  catalog-convexit    = p_convexit.
  catalog-do_sum      = p_do_sum.
  catalog-icon        = p_icon.
  catalog-just        = p_just.
  catalog-emphasize   = p_emphasize.
  catalog-edit        = p_edit.
  APPEND catalog TO it_catalogo.

ENDFORM.                    " Z_ESTRUTURA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  CRIA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM cria_alv .

  CONSTANTS: tabela TYPE string VALUE 'IT_SAIDA'.

  DATA: text_n001        TYPE c LENGTH 50 VALUE 'Referência',
        text_n017        TYPE c LENGTH 50 VALUE 'Pedido',
        text_n002        TYPE c LENGTH 50 VALUE 'Id. Forn.',
        text_n003        TYPE c LENGTH 50 VALUE 'Nome Fornecedor',
        text_n004        TYPE c LENGTH 50 VALUE 'Data Doc.',
        text_n005        TYPE c LENGTH 50 VALUE 'Data Lac.',
        text_n006        TYPE c LENGTH 50 VALUE 'Nr/Série',
        text_n007        TYPE c LENGTH 50 VALUE 'Quantidade',
        text_n008        TYPE c LENGTH 50 VALUE 'Valor',
        text_n009        TYPE c LENGTH 50 VALUE 'Empresa',
        text_n010        TYPE c LENGTH 50 VALUE 'Filial',
        text_n011        TYPE c LENGTH 50 VALUE 'Doc. Miro',
        text_n012        TYPE c LENGTH 50 VALUE 'Doc. Material',
        text_n013        TYPE c LENGTH 50 VALUE 'Doc. Fiscal',
        text_n014        TYPE c LENGTH 50 VALUE 'Doc. Aviso',
        text_n015        TYPE c LENGTH 50 VALUE 'Data Doc.',
        text_n016        TYPE c LENGTH 50 VALUE 'Data Lac.',
        text_n018        TYPE c LENGTH 50 VALUE 'Doc. Transporte',
        text_n019        TYPE c LENGTH 50 VALUE 'Nº VI',
        text_n020        TYPE c LENGTH 50 VALUE 'Valor Unitário',
        it_exclude_fcode TYPE ui_functions,
        wa_exclude_fcode LIKE LINE OF it_exclude_fcode,
        gs_variant_c     TYPE disvariant.

  IF prim_alv IS INITIAL.

    CREATE OBJECT container_alv
      EXPORTING
        container_name = 'CTN_ALV'.

    CREATE OBJECT alv
      EXPORTING
        i_parent = container_alv.

    PERFORM z_estrutura_fieldcat TABLES catalogo_alv USING:
        tabela 'OBJ_KEY'          text_n001 ' ' 01 20 space space space space space space space,
        "tabela 'DATA_USUARIO'     text_n015 ' ' 02 10 space space space space space space 'X',
        tabela 'PO_NUMBER'        text_n017 'X' 02 10 space space space space space space space,
        tabela 'DATA_LAVTO'       text_n016 ' ' 03 10 space space space space space space 'X',
        tabela 'AV_VBELN'         text_n014 'X' 04 10 space space space space space space space,
        tabela 'LIFNR'            text_n002 ' ' 05 10 space space space space space space space,
        tabela 'NAME1'            text_n003 ' ' 06 30 space space space space space space space,
        tabela 'DOC_DATE'         text_n004 ' ' 07 10 space space space space space space space,
        tabela 'PSTNG_DATE'       text_n005 ' ' 08 10 space space space space space space space,
        tabela 'NT_REMESSA'       text_n006 ' ' 09 10 space space space space space space space,
        tabela 'ENTRY_QNT'        text_n007 ' ' 10 15 space space 'X'   space space space space,
        tabela 'VR_BRUTO'         text_n008 ' ' 11 15 space space 'X'   space space space space,
        tabela 'COMP_CODE'        text_n009 ' ' 12 05 space space space space space space space,
        tabela 'PLANT'            text_n010 ' ' 13 05 space space space space space space space,
        tabela 'FT_BELNR'         text_n011 'X' 14 10 space space space space space space space,
        tabela 'MM_MBLNR'         text_n012 'X' 15 10 space space space space space space space,
        tabela 'DOCNUM'           text_n013 'X' 16 10 space space space space space space space,
        tabela 'TKNUM'            text_n018 'X' 17 10 space space space space space space space,
        tabela 'FKNUM'            text_n019 'X' 17 10 space space space space space space space,
        tabela 'NETPR'            text_n020 ' ' 17 10 space space space space space space space.


    CLEAR: gs_layout.
    gs_layout-zebra     = 'X'.
    gs_layout-sel_mode  = 'A'.
    gs_layout-edit_mode = 'X'.

    gs_variant_c-report =  sy-repid.

    CREATE OBJECT event_handler_alv.
    SET HANDLER event_handler_alv->handle_hotspot_click_alv FOR alv.

    wa_exclude_fcode = '&LOCAL&CUT'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&INSERT_ROW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&DELETE_ROW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&MOVE_ROW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&PASTE'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&PASTE_NEW_ROW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&UNDO'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&VARI_ADMIN'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&APPEND'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&COPY'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&COPY_ROW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&VLOTUS'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&AQW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    wa_exclude_fcode = '&PRINT'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&MB_SUM'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&AVERAGE'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&MB_VIEW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&MB_EXPORT'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    "wa_exclude_fcode = '&MB_FILTER'.
    "append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&GRAPH'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&INFO'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    CALL METHOD alv->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        is_variant           = gs_variant_c
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
      CHANGING
        it_fieldcatalog      = catalogo_alv
        it_outtab            = it_saida[].

    prim_alv = 'X'.

  ENDIF.

  CALL METHOD alv->refresh_table_display.

  CALL METHOD alv->set_scroll_info_via_id
    EXPORTING
      is_col_info = scroll_col
      is_row_no   = scroll_row.

ENDFORM.                    " CRIA_ALV

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  DATA: vg_nota_d TYPE zdoc_memo_nota_d.

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row,
        vg_gerar_migo    TYPE char01,
        it_saida_sel     TYPE TABLE OF tp_saida WITH HEADER LINE,
        wa_mov_estq      TYPE zmmt_ee_zgr,
        doc_gerados      TYPE zmmt_ee_zgr_docs.

  CLEAR: it_selected_rows, it_saida_sel[].

  IF ok_code EQ 'AVISO'.

    CALL METHOD alv->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_rows.

    LOOP AT it_selected_rows INTO wa_selected_rows.
      READ TABLE it_saida INDEX wa_selected_rows-index.
      MOVE-CORRESPONDING it_saida TO it_saida_sel.
      APPEND it_saida_sel.
    ENDLOOP.

    LOOP AT it_saida_sel.

      IF ( it_saida_sel-av_vbeln IS INITIAL ) AND ( ( NOT it_saida_sel-ft_belnr IS INITIAL ) OR ( it_saida_sel-mm_mblnr IS NOT INITIAL ) ).

        IF it_saida_sel-mm_mblnr IS INITIAL.
          vg_gerar_migo = 'X'.
        ELSE.
          vg_gerar_migo = space.
        ENDIF.

        MOVE-CORRESPONDING it_saida_sel TO wa_mov_estq.

        SELECT SINGLE * INTO doc_gerados
          FROM zmmt_ee_zgr_docs
         WHERE obj_key EQ wa_mov_estq-obj_key.

        IF NOT sy-subrc IS INITIAL.
          CONTINUE.
        ENDIF.

        CALL FUNCTION 'Z_MM_CRIAR_AVISO'
          EXPORTING
            wa_mov_estq = wa_mov_estq
            invoice_doc = COND #( WHEN it_saida_sel-tp_operacao EQ '08' THEN it_saida_sel-mm_mblnr ELSE it_saida_sel-ft_belnr )
            invoice_ano = COND #( WHEN it_saida_sel-tp_operacao EQ '08' THEN it_saida_sel-mm_mjahr ELSE it_saida_sel-ft_gjahr )
            data_lacto  = it_saida_sel-data_lavto            "data_aviso = it_saida_sel-data_usuario
            gerar_migo  = vg_gerar_migo
          IMPORTING
            doc_gerados = doc_gerados
          EXCEPTIONS
            error       = 1
            OTHERS      = 2.

        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      ENDIF.

    ENDLOOP.

    PERFORM pesquisar.

  ENDIF.

ENDMODULE.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Form  PESQUISAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pesquisar .

  DATA: it_interface_aux  TYPE TABLE OF zmmt_ee_zgr      WITH HEADER LINE,
        it_lfa1           TYPE TABLE OF lfa1     WITH HEADER LINE,
        it_saida_aux      TYPE TABLE OF tp_saida WITH HEADER LINE,
        it_ekko           TYPE TABLE OF ekko     WITH HEADER LINE,
        it_ekko_aux       TYPE TABLE OF ekko     WITH HEADER LINE,
        it_rbkp           TYPE TABLE OF rbkp     WITH HEADER LINE,
        it_mkpf           TYPE TABLE OF mkpf     WITH HEADER LINE,
        it_interface_doca TYPE TABLE OF zmmt_ee_zgr_docs WITH HEADER LINE,
        it_vttp           TYPE TABLE OF vttp     WITH HEADER LINE,
        it_vfkp           TYPE TABLE OF vfkp     WITH HEADER LINE,
        it_vfsi           TYPE TABLE OF vfsi    WITH HEADER LINE.

  CLEAR: it_interface[], it_interface_docs[], it_saida[], it_interface_doca[].

  SELECT * INTO TABLE it_interface
    FROM zmmt_ee_zgr AS z
   WHERE po_number   IN p_pedid
     AND comp_code   IN p_empre
     AND plant       IN p_filia
     AND nt_remessa  IN p_nunser
     AND doc_date    IN p_data
     AND tp_operacao IN it_tipo_operacao
     AND EXISTS ( SELECT *
                    FROM ekko AS e
                   WHERE e~ebeln EQ z~po_number
                     AND e~lifnr IN p_forne )
     AND EXISTS ( SELECT *
                    FROM zmmt_ee_zgr_docs AS d
                   WHERE d~obj_key  EQ z~obj_key
                     AND ( ( d~ft_belnr IN p_miro AND
                             d~ft_gjahr IN p_miroa ) OR

                           ( d~mm_mblnr IN p_miro AND
                             d~mm_mjahr IN p_miroa )
                         )
                 ).

  CLEAR: it_interface_aux[].
  MOVE it_interface[] TO it_interface_aux[].
  SORT it_interface_aux BY po_number.
  DELETE ADJACENT DUPLICATES FROM it_interface_aux COMPARING po_number.
  DELETE it_interface_aux WHERE po_number EQ space.

  IF NOT it_interface_aux[] IS INITIAL.

    SELECT * INTO TABLE it_ekko
      FROM ekko
       FOR ALL ENTRIES IN it_interface_aux
     WHERE ebeln EQ it_interface_aux-po_number.

    IF NOT it_ekko[] IS INITIAL.
      CLEAR: it_ekko_aux[].
      MOVE it_ekko[] TO it_ekko_aux[].
      SORT it_ekko_aux BY lifnr.
      DELETE ADJACENT DUPLICATES FROM it_ekko_aux COMPARING lifnr.
      DELETE it_ekko_aux WHERE lifnr EQ space.

      IF NOT it_ekko_aux[] IS INITIAL.
        SELECT * INTO TABLE it_lfa1
          FROM lfa1
           FOR ALL ENTRIES IN it_ekko_aux
         WHERE lifnr EQ it_ekko_aux-lifnr.
      ENDIF.

    ENDIF.

  ENDIF.

  IF NOT it_interface[] IS INITIAL.
    SELECT * INTO TABLE it_interface_docs[]
      FROM zmmt_ee_zgr_docs
       FOR ALL ENTRIES IN it_interface
     WHERE obj_key  EQ it_interface-obj_key
*       AND ft_belnr NE space
      .

    LOOP AT it_interface_docs ASSIGNING FIELD-SYMBOL(<f_interface_docs>).
      READ TABLE it_interface INTO DATA(wa_interface) WITH KEY obj_key = <f_interface_docs>-obj_key.
      IF sy-subrc IS INITIAL.
        IF wa_interface-tp_operacao EQ '08'.
          IF <f_interface_docs>-mm_mblnr IS INITIAL.
            <f_interface_docs>-obj_key = 'W'.
          ENDIF.
        ENDIF.
        IF wa_interface-tp_operacao NE '08'.
          IF <f_interface_docs>-ft_belnr IS INITIAL.
            <f_interface_docs>-obj_key = 'W'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    DELETE it_interface_docs WHERE obj_key EQ 'W'.

    MOVE it_interface_docs[] TO it_interface_doca[].
    DELETE it_interface_doca WHERE av_vbeln EQ space.
    IF NOT it_interface_doca[] IS INITIAL.
      SELECT * INTO TABLE it_vttp
        FROM vttp
         FOR ALL ENTRIES IN it_interface_doca
       WHERE vbeln EQ it_interface_doca-av_vbeln.

      IF it_vttp[] IS NOT INITIAL.

        SELECT * FROM vfkp INTO TABLE it_vfkp
          FOR ALL ENTRIES IN it_vttp
        WHERE rebel EQ it_vttp-tknum.


        SELECT * FROM vfsi INTO TABLE it_vfsi
           FOR ALL ENTRIES IN it_vttp
         WHERE vbeln EQ it_vttp-vbeln.

      ENDIF.

    ENDIF.

    SELECT * INTO TABLE it_rbkp
      FROM rbkp
      FOR ALL ENTRIES IN it_interface_docs
        WHERE belnr NE space
          AND ( ( gjahr EQ it_interface_docs-ft_gjahr AND belnr EQ it_interface_docs-ft_belnr )
             OR ( gjahr EQ it_interface_docs-mm_mjahr AND belnr EQ it_interface_docs-mm_mblnr ) ).

    SELECT * INTO TABLE it_mkpf
      FROM mkpf
      FOR ALL ENTRIES IN it_interface_docs
        WHERE mblnr NE space
          AND ( ( mjahr EQ it_interface_docs-ft_gjahr AND mblnr EQ it_interface_docs-ft_belnr )
             OR ( mjahr EQ it_interface_docs-mm_mjahr AND mblnr EQ it_interface_docs-mm_mblnr ) ).

  ENDIF.

  LOOP AT it_interface_docs.
    CLEAR: it_saida, it_vttp, it_vfkp, it_vfsi.
    READ TABLE it_interface WITH KEY obj_key = it_interface_docs-obj_key.
    IF NOT sy-subrc IS INITIAL.
      CONTINUE.
    ENDIF.
    MOVE-CORRESPONDING it_interface TO it_saida.

    it_saida-av_vbeln     = it_interface_docs-av_vbeln.
    it_saida-mm_mblnr     = it_interface_docs-mm_mblnr.
    it_saida-mm_mjahr     = it_interface_docs-mm_mjahr.
    it_saida-ft_belnr     = it_interface_docs-ft_belnr.
    it_saida-ft_gjahr     = it_interface_docs-ft_gjahr.
    it_saida-docnum       = it_interface_docs-docnum.
    it_saida-data_usuario = it_interface-doc_date.
    it_saida-data_lavto   = it_interface-pstng_date.

    IF it_interface_docs-av_vbeln IS NOT INITIAL.
      READ TABLE it_vttp WITH KEY vbeln = it_interface_docs-av_vbeln.
      IF sy-subrc IS INITIAL.
        it_saida-tknum = it_vttp-tknum.
      ENDIF.

      READ TABLE it_vfkp WITH KEY rebel = it_vttp-tknum.
      IF sy-subrc = 0.
        it_saida-fknum = it_vfkp-fknum.
      ENDIF.

      READ TABLE it_vfsi WITH KEY vbeln =  it_vttp-vbeln.
      IF sy-subrc = 0 .
        it_saida-netpr = it_vfsi-netpr.
      ENDIF.
    ENDIF.

    READ TABLE it_ekko WITH KEY ebeln = it_saida-po_number.
    IF sy-subrc IS INITIAL.
      it_saida-lifnr = it_ekko-lifnr.
      READ TABLE it_lfa1 WITH KEY lifnr = it_ekko-lifnr.
      IF sy-subrc IS INITIAL.
        it_saida-name1 = it_lfa1-name1.
      ENDIF.
    ENDIF.

    IF it_interface-tp_operacao NE '08'.
      READ TABLE it_rbkp WITH KEY belnr = it_saida-ft_belnr
                                  gjahr = it_saida-ft_gjahr.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ELSE.
        IF it_rbkp-stblg IS NOT INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.

      APPEND it_saida.

    ELSE.
      READ TABLE it_mkpf WITH KEY mblnr = it_saida-mm_mblnr
                                  mjahr = it_saida-mm_mjahr.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      APPEND it_saida.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " PESQUISAR

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_hotspot_click_alv
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
             VALUE(fieldname) LIKE lvc_s_col-fieldname.

  DATA: vg_valid TYPE char01.

  READ TABLE it_saida INDEX row_id.

  CASE fieldname.
    WHEN 'PO_NUMBER'.
      PERFORM authority_check USING 'ME23N' vg_valid.
      IF ( vg_valid IS INITIAL ) AND ( it_saida-po_number IS NOT INITIAL )..
        SET PARAMETER ID 'BES' FIELD it_saida-po_number.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'AV_VBELN'.
      PERFORM authority_check USING 'VL33N' vg_valid.
      IF ( vg_valid IS INITIAL ) AND ( it_saida-av_vbeln IS NOT INITIAL ).
        SET PARAMETER ID 'VL' FIELD it_saida-av_vbeln.
        CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'FT_BELNR'.
      PERFORM authority_check USING 'MIR4' vg_valid.
      IF ( vg_valid IS INITIAL ) AND ( it_saida-ft_belnr IS NOT INITIAL ).
        SET PARAMETER ID 'RBN' FIELD it_saida-ft_belnr.
        SET PARAMETER ID 'GJR' FIELD it_saida-ft_gjahr.
        CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'MM_MBLNR'.
      PERFORM authority_check USING 'MB03' vg_valid.
      IF ( vg_valid IS INITIAL )  AND ( it_saida-mm_mblnr IS NOT INITIAL ).

* ---> S4 Migration - 19/07/2023 - DG
*        SET PARAMETER ID 'MBN' FIELD it_saida-mm_mblnr.
*        SET PARAMETER ID 'MJA' FIELD it_saida-mm_mjahr.
*        CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING
            i_action            = 'A04'
            i_refdoc            = 'R02'
            i_notree            = 'X'
            i_no_auth_check     = ' '
            i_deadend           = 'X'
            i_skip_first_screen = 'X'
            i_okcode            = 'OK_GO'
            i_mblnr             = it_saida-mm_mblnr
            i_mjahr             = it_saida-mm_mjahr.
        "I_ZEILE = I_FINAL-ZEILE.

* <--- S4 Migration - 19/07/2023 - DG
      ENDIF.
    WHEN 'TKNUM'.
      PERFORM authority_check USING 'VT03N' vg_valid.
      IF ( vg_valid IS INITIAL ) AND ( it_saida-tknum IS NOT INITIAL ).
        SET PARAMETER ID: 'TNR' FIELD it_saida-tknum.
        CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'DOCNUM'.
      CHECK NOT it_saida-docnum IS INITIAL.
      PERFORM nf_writer USING it_saida-docnum.

    WHEN 'FKNUM'.
      PERFORM authority_check USING 'VI03' vg_valid.
      IF ( vg_valid IS INITIAL ) AND ( it_saida-tknum IS NOT INITIAL ).
        SET PARAMETER ID: 'FKK' FIELD it_saida-fknum.
        CALL TRANSACTION 'VI03' AND SKIP FIRST SCREEN.
      ENDIF.

  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK_ALV

*&---------------------------------------------------------------------*
*&      Form  NF_WRITER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZNOM_PROG_REME_ALV_DOCNUM  text
*----------------------------------------------------------------------*
FORM nf_writer  USING p_docnum TYPE j_1bdocnum.

  CHECK NOT p_docnum IS INITIAL.

  DATA: gf_nfobjn LIKE j_1binterf-nfobjn.

  CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
    EXPORTING
      doc_number         = p_docnum
    IMPORTING
      obj_number         = gf_nfobjn
    EXCEPTIONS
      document_not_found = 1
      docum_lock         = 2
      OTHERS             = 3.

  CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
    EXPORTING
      obj_number         = gf_nfobjn
    EXCEPTIONS
      object_not_found   = 1
      scr_ctrl_not_found = 2
      OTHERS             = 3.

ENDFORM.                    " nf_writer

*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM authority_check  USING u_tcd u_valid.

  CLEAR: u_valid.
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD u_tcd.
  IF sy-subrc NE 0.
    MESSAGE s172(00) WITH u_tcd.
    u_valid = 'X'.
  ENDIF.

ENDFORM.                    " AUTHORITY_CHECK

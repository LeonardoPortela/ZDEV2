*&---------------------------------------------------------------------*
*& Report  ZFIR0088
*&
*&---------------------------------------------------------------------*
* Programa..: ZFIR0088                                                 *
* Tipo......: Report                                                   *
* Transação.: ZFIS55                                                   *
* Descrição.: Parâmetros – Liberação Usuário J1BTAX                    *
*             Liberar transação J1BTAX para usuário que tenham         *
*             permissão cadastrada na tabela Z (Transação ZFIS54)      *
* Autor.....: Sara Oikawa                                              *
* Data......: 24.09.2020                                               *
*----------------------------------------------------------------------*
*                     Controle de Alterações                           *
*----------------------------------------------------------------------*
* Data       | Change     | Autor        | Alteração                   *
*----------------------------------------------------------------------*
* 24.09.20   |DEVK9A0MWX  |Sara Oikawa   | Codificação Inicial         *
*----------------------------------------------------------------------*
REPORT zfir0088.
*&---------------------------------------------------------------------*
*& Tabelas Transparentes                                               *
*&---------------------------------------------------------------------*
TABLES: zmmt0168,ekpo.

*&---------------------------------------------------------------------*
*&  Include           ZFIR0088_TOP
*&---------------------------------------------------------------------*

TYPE-POOLS: icon.

*&---------------------------------------------------------------------*
*& Declaração de Tipos                                                 *
*&---------------------------------------------------------------------*
TYPES: BEGIN OF  ty_saida,
         del(1)      TYPE c,
         icon(4)     TYPE c,
         chave_nfe   TYPE zmmt0168-chave_nfe,
         shipfrom    TYPE zmmt0168-shipfrom,
         shipto      TYPE zmmt0168-shipto,
         werks       TYPE zmmt0168-werks,
         matnr       TYPE zmmt0168-matnr,
         desc_mat    TYPE makt-maktx,
         desc_xml    TYPE zmmt0168-desc_xml,
         icms_cst    TYPE zib_nfe_dist_itm-icms_cst,
         lifnr       TYPE zmmt0168-lifnr,
         rate        TYPE zmmt0168-rate,
         base        TYPE zmmt0168-base,
         ratej       TYPE zmmt0168-rate,
         basej       TYPE zmmt0168-base,
         ebeln       TYPE zmmt0168-ebeln,
         ebelp       TYPE zmmt0168-ebelp,
         mwskz_p     TYPE zmmt0168-mwskz,
         mwskz_n     TYPE zmmt0168-mwskz,
         indcoper    TYPE zmmt0168-indcoper,
         prod_item   TYPE zmmt0168-prod_item,
         usnasm      TYPE zmmt0168-usnam,
         zdt_atual   TYPE zmmt0168-zdt_atual,
         zhr_atual   TYPE zmmt0168-zhr_atual,
         ncm_xml     TYPE marc-steuc,
         ncm_mat_sap TYPE marc-steuc,
         celltab     TYPE lvc_t_styl,
       END OF ty_saida,

       BEGIN OF  ty_ekpo,
         ebeln   TYPE ekpo-ebeln,
         ebelp   TYPE ekpo-ebelp,
         matnr   TYPE ekpo-matnr,
         txz01   TYPE ekpo-txz01,
         loekz   TYPE ekpo-loekz,
         j_1bnbm TYPE ekpo-j_1bnbm,
       END OF ty_ekpo.

*&---------------------------------------------------------------------*
*& Declaração de Tabelas Internas / Estruturas                         *
*&---------------------------------------------------------------------*
DATA: it_saida        TYPE TABLE OF ty_saida,
      it_saida_aux    TYPE TABLE OF ty_saida,
      it_ekpo         TYPE TABLE OF ty_ekpo,
      wa_ekpo         TYPE ekpo,
      wa_saida        TYPE ty_saida,
      v_chave         TYPE zmmt0168-chave_nfe,

      it_zmmt0168     TYPE TABLE OF zmmt0168,
      it_zmmt0168_aux TYPE TABLE OF zmmt0168,
      wa_zmmt0169     TYPE zmmt0169,
      wa_zmmt0168     TYPE zmmt0168,
      wa_zmmt0131     TYPE zmmt0131,
      wa_j_1btxic3    TYPE j_1btxic3,
      t_branch        TYPE TABLE OF j_1bbranch.

DATA: gs_ret_messages         TYPE bapi_matreturn2,
      gs_headdata             TYPE bapimathead,
      gs_clientdata           TYPE bapi_mara,
      gs_clientdatax          TYPE bapi_marax,
      gs_forecastparameters   TYPE bapi_mpop,
      gs_forecastparametersx  TYPE bapi_mpopx,
      gs_mat_description      TYPE bapi_makt,
      gs_mat_longtext         TYPE bapi_mltx,
      gs_taxclassifications   TYPE bapi_mlan,
      gs_plantdata            TYPE bapi_marc,
      gs_plantdatax           TYPE bapi_marcx,
      gs_storagelocationdata  TYPE bapi_mard,
      gs_storagelocationdatax TYPE bapi_mardx,
      gs_storagetypedata      TYPE bapi_mlgt,
      gs_storagetypedatax     TYPE bapi_mlgtx,
      gs_salesdata            TYPE bapi_mvke,
      gs_salesdatax           TYPE bapi_mvkex,
      gs_unitsofmeasure       TYPE bapi_marm,
      gs_unitsofmeasurex      TYPE bapi_marmx,
      gs_valuationdata        TYPE bapi_mbew,
      gs_valuationdatax       TYPE bapi_mbewx,
      gs_return               TYPE bapiret2,
      gs_log                  TYPE TABLE OF bapiret2,
      gs_charactdetail        TYPE bapicharactdetail,
      gs_charactdescr         TYPE bapicharactdescr,
      gs_charactrestrict      TYPE bapicharactrestrictions,
      gs_charactvalueschar    TYPE bapicharactvalueschar,
      gs_classbasicdata       TYPE bapi1003_basic,
      gs_classdescriptions    TYPE bapi1003_catch,
      gs_classcharacterist    TYPE bapi1003_charact,
      gs_substructlist        TYPE bapi1003_tree,
      gt_info_compl           TYPE zinfo_complementar_tab,
      gt_ret_messages         TYPE TABLE OF bapi_matreturn2,
      gt_mat_description      TYPE TABLE OF bapi_makt,
      gt_mat_longtext         TYPE TABLE OF bapi_mltx,
      gt_unitsofmeasure       TYPE TABLE OF bapi_marm,
      gt_unitsofmeasurex      TYPE TABLE OF bapi_marmx,
      gt_taxclassifications   TYPE TABLE OF bapi_mlan.

DATA: it_return_ped             TYPE STANDARD TABLE OF bapiret2, "TABLE OF BAPIRET2 WITH HEADER LINE,
      wa_return_ped             TYPE bapiret2,
      it_poitem_ped             TYPE STANDARD TABLE OF bapimepoitem, "TABLE OF BAPIMEPOITEM WITH HEADER LINE,
      wa_poitem_ped             TYPE bapimepoitem,
      it_poitemx_ped            TYPE STANDARD TABLE OF bapimepoitemx,
      it_popartner              TYPE STANDARD TABLE OF bapiekkop,
      wa_poitemx_ped            TYPE bapimepoitemx,
      wa_poheader_ped           TYPE bapimepoheader,
      wa_poheaderx_ped          TYPE bapimepoheaderx,
      it_popartner_ped          TYPE STANDARD TABLE OF bapiekkop, "TABLE OF BAPIMEPOITEM WITH HEADER LINE,
      wa_popartner_ped          TYPE bapiekkop, "TABLE OF BAPIMEPOITEM WITH HEADER LINE,
      it_bapimepotextheader_ped TYPE STANDARD TABLE OF bapimepotextheader,
      wa_bapimepotextheader_ped TYPE bapimepotextheader,
      it_pocond                 TYPE STANDARD TABLE OF bapimepocond,
      wa_pocond                 TYPE bapimepocond,
      it_pocondx                TYPE STANDARD TABLE OF bapimepocondx,
      wa_pocondx                TYPE bapimepocondx,
      purchaseorder             LIKE bapimepoheader-po_number.

DATA: gc_x TYPE char1             VALUE 'X',
      gc_s TYPE char1             VALUE 'S'.


DATA: ok-code         TYPE sy-ucomm.

DATA: it_texto        TYPE STANDARD TABLE OF tline,
      wl_desactive(1),
      p_ebeln         TYPE RANGE OF ekpo-ebeln,
      w_ebeln         LIKE LINE OF p_ebeln,
      v_werks         TYPE zmmt0168-werks.


DATA: tl_tlines TYPE TABLE OF tline,
      wl_tlines TYPE tline.


*-CS2025000249-02.05.2025-#174115-JT-inicio
DATA: lc_zcl_nfe    TYPE REF TO zcl_nfe_inbound,
      lc_seqlog     TYPE zde_seq_log,
      lv_message    TYPE string,
      lc_inb_aceite TYPE char01.
*-CS2025000249-02.05.2025-#174115-JT-fim

DATA: BEGIN OF act_sellist OCCURS 10.
        INCLUDE STRUCTURE vimsellist.
DATA: END OF act_sellist.

DATA: BEGIN OF act_exclfun OCCURS 10.
        INCLUDE STRUCTURE vimexclfun.
DATA: END OF act_exclfun.
*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS:
  c_clos_msg(8) TYPE c VALUE 'CLOS_MSG',
  c_add(3)      TYPE c VALUE 'ADD',
  c_del(3)      TYPE c VALUE 'DEL'.

*Class definition for ALV toolbar
CLASS:  lcl_alv_toolbar     DEFINITION DEFERRED.
DATA: g_custom_container   TYPE REF TO cl_gui_custom_container,
      g_grid               TYPE REF TO cl_gui_alv_grid,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      it_fieldcat          TYPE lvc_t_fcat,
      wa_fieldcat          TYPE lvc_s_fcat,
      tl_function          TYPE ui_functions,
      wl_function          LIKE tl_function  WITH HEADER LINE,
      wa_layout            TYPE lvc_s_layo,
      wa_variant           TYPE disvariant,
      wa_estilo            TYPE lvc_t_styl,
      wa_stable            TYPE lvc_s_stbl VALUE 'XX'.

*Declaration for toolbar buttons
DATA: ty_toolbar TYPE stb_button.

CLASS lcl_hander DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING
          e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender,

      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells sender.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.
ENDCLASS.

CLASS lcl_hander IMPLEMENTATION.
  METHOD on_onf4.
    FIELD-SYMBOLS: <stylet> TYPE lvc_t_styl.
    FIELD-SYMBOLS: <stylel> TYPE lvc_s_styl.

    TYPES: BEGIN OF ty_field,
             tabname   TYPE dd03l-tabname,     "Nome da tabela
             fieldname TYPE dd03l-fieldname,   "Nome de campo
             s(1)      TYPE c,
           END OF ty_field,

           BEGIN OF ty_value,
             tabname    TYPE dd03l-tabname,     "Nome da tabela
             fieldname  TYPE dd03l-fieldname,   "Nome de campo
             char79(79) TYPE c,
           END OF ty_value,

           BEGIN OF ty_matnr,
             ebeln TYPE ekpo-ebeln,
             matnr TYPE makt-matnr,
             maktx TYPE makt-maktx,
           END OF ty_matnr.

    DATA: BEGIN OF wl_valuetab,
            field(50),
          END OF wl_valuetab.

    DATA: tl_valuetab LIKE TABLE OF wl_valuetab,
          tl_field    TYPE TABLE OF ty_field,
          wl_field    TYPE ty_field,
          tl_value    TYPE TABLE OF ty_value,
          wl_value    TYPE ty_value.

    DATA: wl_fieldname(30),
          wl_tabname(30),
          wl_index            TYPE sy-tabix,
          wl_char(20),
          w_makt              TYPE makt,
          w_zmmt0169          TYPE zmmt0169,
          v_indcoper          TYPE zmmt0169-indcoper,
          v_bukrs             TYPE j_1bbranch-bukrs,
          v_branch            TYPE zmmt0169-branch,
          v_regio             TYPE t001w-regio,
          v_regiof            TYPE lfa1-regio,
          wa_zib_nfe_dist_itm TYPE zib_nfe_dist_itm,
          wl_ekpo             TYPE ekpo.

    CASE e_fieldname.
      WHEN 'MATNR'.
        REFRESH it_ekpo.
        CHECK p_ebeln[] IS NOT INITIAL.
        SELECT  DISTINCT ebeln ebelp matnr txz01 loekz
          INTO TABLE it_ekpo
          FROM ekpo
          WHERE ebeln IN p_ebeln
          ORDER BY txz01.

        LOOP AT it_ekpo INTO DATA(w_ekpo).
          DATA(tabix) = sy-tabix.
          READ TABLE it_saida INTO wa_saida WITH KEY matnr = w_ekpo-matnr.
          IF sy-subrc = 0 AND wa_saida-matnr IS NOT INITIAL.
            w_ekpo-loekz = 'X'.
            MODIFY it_ekpo FROM w_ekpo INDEX tabix.
          ENDIF.
        ENDLOOP.
        DELETE it_ekpo WHERE loekz IS NOT INITIAL.
        CHECK it_ekpo IS NOT INITIAL.
        "
        wl_fieldname  = 'MATNR'.
        wl_tabname    = 'EKPO'.
        LOOP AT it_ekpo INTO w_ekpo.
          tabix = sy-tabix.
          MOVE: w_ekpo-ebeln   TO wl_valuetab-field.
          APPEND wl_valuetab   TO tl_valuetab.
          CLEAR:  wl_valuetab.

          MOVE: w_ekpo-ebelp   TO wl_valuetab-field.
          APPEND wl_valuetab   TO tl_valuetab.
          CLEAR: wl_valuetab.

          MOVE: w_ekpo-matnr   TO wl_valuetab-field.
          APPEND wl_valuetab   TO tl_valuetab.
          CLEAR:  wl_valuetab.

          MOVE:  w_ekpo-txz01  TO wl_valuetab-field.
          APPEND wl_valuetab   TO tl_valuetab.
          CLEAR:  wl_valuetab.

        ENDLOOP.
        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'EBELN'.
        wl_field-s = 'X'.
        APPEND wl_field TO tl_field.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'EBELP'.
        wl_field-s = 'X'.
        APPEND wl_field TO tl_field.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'MATNR'.
        wl_field-s = 'X'.
        APPEND wl_field TO tl_field.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'TXZ01'.
        wl_field-s = ' '.
        APPEND wl_field TO tl_field.
    ENDCASE.

    IF    wl_fieldname  IS NOT INITIAL
         AND wl_tabname    IS NOT INITIAL
         AND tl_field[]    IS NOT INITIAL
         AND tl_valuetab[] IS NOT INITIAL.
      CLEAR  wl_index.
      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
        EXPORTING
*         cucol                     = '3'
          fieldname                 = wl_fieldname
          tabname                   = wl_tabname
        IMPORTING
          index                     = wl_index
          select_value              = wl_char
        TABLES
          fields                    = tl_field
          select_values             = tl_value
          valuetab                  = tl_valuetab
        EXCEPTIONS
          field_not_in_ddic         = 001
          more_then_one_selectfield = 002
          no_selectfield            = 003.
      IF sy-subrc IS INITIAL AND wl_index GT 0.
        CASE e_fieldname.
          WHEN 'MATNR'.
            READ TABLE it_ekpo INTO w_ekpo INDEX wl_index.
        ENDCASE.
        IF es_row_no-row_id GT 0.
          IF  e_fieldname = 'MATNR'.
            READ TABLE it_saida INTO wa_saida  INDEX es_row_no-row_id.
          ENDIF.
        ENDIF.

        CASE e_fieldname.
          WHEN 'MATNR'.
            MOVE: w_ekpo-matnr TO wa_saida-matnr.
        ENDCASE.
        IF  e_fieldname = 'MATNR'.
          SELECT SINGLE *
            FROM makt
            INTO w_makt
            WHERE spras = sy-langu
            AND   matnr = w_ekpo-matnr.
          wa_saida-desc_mat = w_makt-maktx.
          MODIFY it_saida FROM wa_saida INDEX es_row_no-row_id.
        ENDIF.
      ENDIF.
      IF wa_saida-shipfrom    IS NOT INITIAL AND
          wa_saida-shipto     IS NOT INITIAL AND
          wa_saida-werks      IS NOT INITIAL AND
          wa_saida-matnr      IS NOT INITIAL AND
          wa_saida-lifnr      IS NOT INITIAL.
        SELECT SINGLE *
           INTO wa_j_1btxic3
           FROM j_1btxic3
           WHERE land1     = 'BR'
           AND   shipfrom  = wa_saida-shipfrom
           AND   shipto    = wa_saida-shipto
           AND   gruop     = '31'
           AND   value     = wa_saida-werks
           AND   value2    = wa_saida-matnr
           AND   value3    = wa_saida-lifnr.

        IF sy-subrc = 0.
          wa_saida-ratej = wa_j_1btxic3-rate.
          wa_saida-basej = wa_j_1btxic3-base.
          wa_saida-icon = icon_complete.
          MODIFY it_saida FROM wa_saida INDEX es_row_no-row_id.
        ENDIF.
      ENDIF.
      CHECK p_ebeln[] IS NOT INITIAL.
      SELECT SINGLE *
        INTO wl_ekpo
        FROM ekpo
        WHERE ebeln IN p_ebeln
        AND   matnr EQ w_makt-matnr.
      "
      IF sy-subrc = 0.
        wa_saida-ebeln   = wl_ekpo-ebeln.
        wa_saida-ebelp   = wl_ekpo-ebelp.
        wa_saida-mwskz_p = wl_ekpo-mwskz.

        SELECT SINGLE *
          INTO wa_zib_nfe_dist_itm
          FROM zib_nfe_dist_itm
         WHERE chave_nfe = v_chave
         AND   prod_item = wa_saida-prod_item.

        IF sy-subrc = 0.
          wa_saida-icms_cst =  wa_zib_nfe_dist_itm-icms_cst.
          wa_saida-ncm_xml =  |{ wa_zib_nfe_dist_itm-prod_ncm+0(4) }.{ wa_zib_nfe_dist_itm-prod_ncm+4(2) }.{ wa_zib_nfe_dist_itm-prod_ncm+6(2) }|.
        ENDIF.

        SELECT SINGLE steuc FROM marc
          INTO wa_saida-ncm_mat_sap
          WHERE werks EQ wa_saida-werks
            AND matnr EQ wa_saida-matnr.


        SELECT SINGLE bukrs
          INTO v_bukrs
          FROM j_1bbranch
          WHERE branch = wa_saida-werks.

        SELECT SINGLE regio
          INTO v_regio
          FROM t001w
          WHERE werks = wa_saida-werks.

        SELECT SINGLE regio
          INTO v_regiof
          FROM lfa1
          WHERE lifnr = wa_saida-lifnr.

        IF v_regio NE v_regiof.
          v_indcoper  = 'F'.
        ELSE.
          v_indcoper  = 'D'.
        ENDIF.
        CLEAR v_branch.
        SELECT SINGLE *
          INTO w_zmmt0169
          FROM zmmt0169
         WHERE bukrs    = v_bukrs                          "OBRIGATORIO
         AND   branch   = wa_saida-werks                   "OPCIONAL
         AND   regio    = v_regio                          "OPCIONAL
         AND   icms_cst = wa_zib_nfe_dist_itm-icms_cst     "OBRIGATORIO
         AND   indcoper = v_indcoper                       "OBRIGATORIO
         AND   mwskz_1    = wa_saida-mwskz_p+0(1).
        IF sy-subrc NE 0.
          SELECT SINGLE *
          INTO w_zmmt0169
          FROM zmmt0169
         WHERE bukrs    = v_bukrs                          "OBRIGATORIO
         AND   branch   = wa_saida-werks                   "OPCIONAL
         AND   regio    = ' '                              "OPCIONAL
         AND   icms_cst = wa_zib_nfe_dist_itm-icms_cst     "OBRIGATORIO
         AND   indcoper = v_indcoper                       "OBRIGATORIO
         AND   mwskz_1    = wa_saida-mwskz_p+0(1).
          IF sy-subrc NE 0.
            SELECT SINGLE *
             INTO w_zmmt0169
             FROM zmmt0169
            WHERE bukrs    = v_bukrs                          "OBRIGATORIO
            AND   branch   = v_branch                         "OPCIONAL
            AND   regio    = v_regio                          "OPCIONAL
            AND   icms_cst = wa_zib_nfe_dist_itm-icms_cst     "OBRIGATORIO
            AND   indcoper = v_indcoper                       "OBRIGATORIO
            AND   mwskz_1  = wa_saida-mwskz_p+0(1).
            IF sy-subrc NE 0.
              SELECT SINGLE *
                 INTO w_zmmt0169
                 FROM zmmt0169
                WHERE bukrs    = v_bukrs                          "OBRIGATORIO
                AND   branch   = v_branch                         "OPCIONAL
                AND   regio    = ' '                              "OPCIONAL
                AND   icms_cst = wa_zib_nfe_dist_itm-icms_cst     "OBRIGATORIO
                AND   indcoper = v_indcoper                       "OBRIGATORIO
                AND   mwskz_1    = wa_saida-mwskz_p+0(1).
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: wa_saida-mwskz_n, wa_saida-indcoper.
        IF sy-subrc = 0.
          IF w_zmmt0169-mwskz+0(1) = wa_saida-mwskz_p+0(1).
            wa_saida-mwskz_n =  w_zmmt0169-mwskz.
            wa_saida-indcoper =  w_zmmt0169-indcoper.
          ENDIF.
        ENDIF.

        MODIFY it_saida FROM wa_saida INDEX es_row_no-row_id.
      ENDIF.


    ENDIF.

    CALL METHOD g_grid->refresh_table_display( is_stable = wa_stable ).



  ENDMETHOD.

  METHOD on_data_changed.
    DATA: lv_value            TYPE lvc_value,
          ls_good             TYPE lvc_s_modi,
          w_makt              TYPE makt,
          w_steuc             TYPE steuc,
          w_ekpo              TYPE ekpo,
          w_zmmt0169          TYPE zmmt0169,
          v_indcoper          TYPE zmmt0169-indcoper,
          v_bukrs             TYPE j_1bbranch-bukrs,
          v_branch            TYPE zmmt0169-branch,
          v_regio             TYPE t001w-regio,
          v_regiof            TYPE lfa1-regio,
          wa_zib_nfe_dist_itm TYPE zib_nfe_dist_itm,
          v_chave2            TYPE zmmt0168-chave_nfe,
          v_comple            TYPE c.

    v_chave2 = sy-uname && sy-datum.
    "
    LOOP AT er_data_changed->mt_good_cells
                            INTO ls_good
                            WHERE fieldname = 'MATNR'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      READ TABLE it_saida INTO wa_saida INDEX ls_good-row_id.

      CLEAR w_makt.
      SELECT SINGLE *
            FROM makt
            INTO w_makt
            WHERE spras = sy-langu
            AND   matnr = lv_value.
      lv_value = w_makt-maktx.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'DESC_MAT'
          i_value     = lv_value.


      CLEAR w_steuc.
      SELECT SINGLE steuc
            FROM marc
            INTO w_steuc
            WHERE werks = wa_saida-werks
            AND   matnr = w_makt-matnr.

      lv_value = w_steuc.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NCM_MAT_SAP'
          i_value     = lv_value.

      CHECK p_ebeln[] IS NOT INITIAL.
      SELECT SINGLE *
        INTO w_ekpo
        FROM ekpo
        WHERE ebeln IN p_ebeln
        AND   matnr EQ w_makt-matnr.
      "
      lv_value =  w_ekpo-ebeln.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'EBELN'
          i_value     = lv_value.

      lv_value =  w_ekpo-ebelp.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'EBELP'
          i_value     = lv_value.

      lv_value =  w_ekpo-mwskz.
      wa_saida-mwskz_p =  w_ekpo-mwskz.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'MWSKZ_P'
          i_value     = lv_value.

      SELECT SINGLE *
        INTO wa_zib_nfe_dist_itm
        FROM zib_nfe_dist_itm
       WHERE chave_nfe = v_chave
       AND   prod_item = wa_saida-prod_item.

      IF sy-subrc = 0.
        lv_value =  wa_zib_nfe_dist_itm-icms_cst.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'ICMS_CST'
            i_value     = lv_value.

        lv_value =  |{ wa_zib_nfe_dist_itm-prod_ncm+0(4) }.{ wa_zib_nfe_dist_itm-prod_ncm+4(2) }.{ wa_zib_nfe_dist_itm-prod_ncm+6(2) }|.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'NCM_XML'
            i_value     = lv_value.
      ENDIF.


      SELECT SINGLE bukrs
        INTO v_bukrs
        FROM j_1bbranch
        WHERE branch = wa_saida-werks.

      SELECT SINGLE regio
        INTO v_regio
        FROM t001w
        WHERE werks = wa_saida-werks.

      SELECT SINGLE regio
        INTO v_regiof
        FROM lfa1
        WHERE lifnr = wa_saida-lifnr.

      IF v_regio NE v_regiof.
        v_indcoper  = 'F'.
      ELSE.
        v_indcoper  = 'D'.
      ENDIF.

      CLEAR v_branch.
      SELECT SINGLE *
        INTO w_zmmt0169
        FROM zmmt0169
       WHERE bukrs    = v_bukrs                          "OBRIGATORIO
       AND   branch   = wa_saida-werks                   "OPCIONAL
       AND   regio    = v_regio                          "OPCIONAL
       AND   icms_cst = wa_zib_nfe_dist_itm-icms_cst     "OBRIGATORIO
       AND   indcoper = v_indcoper                       "OBRIGATORIO
       AND   mwskz_1    = wa_saida-mwskz_p+0(1).
      IF sy-subrc NE 0.
        SELECT SINGLE *
        INTO w_zmmt0169
        FROM zmmt0169
       WHERE bukrs    = v_bukrs                          "OBRIGATORIO
       AND   branch   = wa_saida-werks                   "OPCIONAL
       AND   regio    = ' '                              "OPCIONAL
       AND   icms_cst = wa_zib_nfe_dist_itm-icms_cst     "OBRIGATORIO
       AND   indcoper = v_indcoper                       "OBRIGATORIO
       AND   mwskz_1    = wa_saida-mwskz_p+0(1).
        IF sy-subrc NE 0.
          SELECT SINGLE *
           INTO w_zmmt0169
           FROM zmmt0169
          WHERE bukrs    = v_bukrs                          "OBRIGATORIO
          AND   branch   = v_branch                         "OPCIONAL
          AND   regio    = v_regio                          "OPCIONAL
          AND   icms_cst = wa_zib_nfe_dist_itm-icms_cst     "OBRIGATORIO
          AND   indcoper = v_indcoper                       "OBRIGATORIO
          AND   mwskz_1    = wa_saida-mwskz_p+0(1).
          IF sy-subrc NE 0.
            SELECT SINGLE *
            INTO w_zmmt0169
            FROM zmmt0169
           WHERE bukrs    = v_bukrs                          "OBRIGATORIO
           AND   branch   = v_branch                         "OPCIONAL
           AND   regio    = ' '                              "OPCIONAL
           AND   icms_cst = wa_zib_nfe_dist_itm-icms_cst     "OBRIGATORIO
           AND   indcoper = v_indcoper                       "OBRIGATORIO
           AND   mwskz_1    = wa_saida-mwskz_p+0(1).
          ENDIF.
        ENDIF.
      ENDIF.
      CLEAR: lv_value.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'INDCOPER'
          i_value     = lv_value.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'MWSKZ_N'
          i_value     = lv_value.
      IF sy-subrc = 0.
        IF w_zmmt0169-mwskz+0(1) = wa_saida-mwskz_p+0(1).
          lv_value =  w_zmmt0169-indcoper.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'INDCOPER'
              i_value     = lv_value.
          lv_value =  w_zmmt0169-mwskz.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'MWSKZ_N'
              i_value     = lv_value.
        ENDIF.
      ENDIF.

    ENDLOOP.
    "
    LOOP AT er_data_changed->mt_good_cells
                            INTO ls_good
                            WHERE fieldname = 'RATE'  OR
                                  fieldname = 'MATNR' OR
                                  fieldname = 'LIFNR' OR
                                  fieldname = 'BASE'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      READ TABLE it_saida INTO wa_saida INDEX ls_good-row_id.

      IF  ls_good-fieldname = 'RATE'.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'RATE'
            i_value     = lv_value.
        wa_saida-rate = lv_value.
      ELSEIF  ls_good-fieldname = 'BASE'.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'BASE'
            i_value     = lv_value.
        wa_saida-base = lv_value.
      ELSEIF  ls_good-fieldname = 'MATNR'.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'MATNR'
            i_value     = lv_value.
        wa_saida-matnr = lv_value.
      ELSEIF  ls_good-fieldname = 'LIFNR'.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'LIFNR'
            i_value     = lv_value.
        wa_saida-lifnr = lv_value.
      ENDIF.

      IF wa_saida-shipfrom   IS NOT INITIAL AND
         wa_saida-shipto     IS NOT INITIAL AND
         wa_saida-werks      IS NOT INITIAL AND
         wa_saida-matnr      IS NOT INITIAL AND
         wa_saida-lifnr      IS NOT INITIAL.
        SELECT SINGLE *
           INTO wa_j_1btxic3
           FROM j_1btxic3
           WHERE land1     = 'BR'
           AND   shipfrom  = wa_saida-shipfrom
           AND   shipto    = wa_saida-shipto
           AND   gruop     = '31'
           AND   value     = wa_saida-werks
           AND   value2    = wa_saida-matnr
           AND   value3    = wa_saida-lifnr.

        IF sy-subrc = 0.
          v_comple = 'X'.
          lv_value = wa_j_1btxic3-rate.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'RATEJ'
              i_value     = lv_value.
          "
          lv_value = wa_j_1btxic3-base.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'BASEJ'
              i_value     = lv_value.
          "
          lv_value = icon_complete.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'ICON'
              i_value     = lv_value.
        ELSE.
          v_comple = ' '.
          CLEAR lv_value.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'RATEJ'
              i_value     = lv_value.
          "
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'BASEJ'
              i_value     = lv_value.
          "
        ENDIF.
      ENDIF.
      "
      IF  v_comple NE 'X'.
        IF wa_saida-shipfrom   IS NOT INITIAL AND
        wa_saida-shipto     IS NOT INITIAL AND
        wa_saida-werks      IS NOT INITIAL AND
        wa_saida-matnr      IS NOT INITIAL AND
        wa_saida-lifnr      IS NOT INITIAL AND
        wa_saida-rate       IS NOT INITIAL AND
        wa_saida-base       IS NOT INITIAL.
          lv_value = icon_checked.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'ICON'
              i_value     = lv_value.
        ELSE.
          lv_value = icon_warning.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'ICON'
              i_value     = lv_value.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_data_changed_finished.

  ENDMETHOD.


ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      constructor
        IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    ty_toolbar-icon      = icon_insert_row.
    ty_toolbar-function  = c_add.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = c_del.
    ty_toolbar-disabled  = space. "wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 5.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

*   variable for Toolbar Button
    ty_toolbar-icon      = icon_view_close.
    ty_toolbar-function  = c_clos_msg.
    ty_toolbar-disabled  = space.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.


    CASE e_ucomm.
      WHEN c_add.
        it_saida_aux[] = it_saida[].
        REFRESH: it_saida.
        LOOP AT it_saida_aux INTO wa_saida.
          APPEND wa_saida TO it_saida.
        ENDLOOP.
        CLEAR wa_saida.

        FREE wa_saida-celltab.

        wa_estilo = VALUE #( ( fieldname = 'SHIPFROM'        style = cl_gui_alv_grid=>mc_style_enabled )
                             ( fieldname = 'SHIPTO'          style = cl_gui_alv_grid=>mc_style_enabled )
                             ( fieldname = 'WERKS'           style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'MATNR'           style = cl_gui_alv_grid=>mc_style_enabled )
                             ( fieldname = 'LIFNR'           style = cl_gui_alv_grid=>mc_style_enabled )
                             ( fieldname = 'NCM_XML'         style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'NCM_MAT_SAP'     style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'RATE'            style = cl_gui_alv_grid=>mc_style_enabled )
                             ( fieldname = 'BASE'            style = cl_gui_alv_grid=>mc_style_enabled )
                             ( fieldname = 'RATEJ'           style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'BASEJ'           style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'USNAM'           style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'ZDT_ATUAL'       style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'ZHR_ATUAL'       style = cl_gui_alv_grid=>mc_style_disabled )   ).


        wa_saida-icon =  icon_warning.
        wa_saida-werks = v_werks.
        INSERT LINES OF wa_estilo INTO TABLE wa_saida-celltab.
        APPEND wa_saida TO it_saida.
      WHEN c_del.
        CALL METHOD g_grid->get_selected_rows
          IMPORTING
            et_index_rows = DATA(it_selected).
        LOOP AT it_selected INTO DATA(wa_selected).
          READ TABLE it_saida INTO wa_saida INDEX wa_selected-index.
          IF sy-subrc = 0.
            wa_saida-del = 'X'.
            MODIFY it_saida FROM wa_saida INDEX wa_selected-index TRANSPORTING del.
            DELETE FROM zmmt0168 WHERE chave_nfe = wa_saida-chave_nfe
                                 AND   shipfrom  = wa_saida-shipfrom
                                 AND   shipto    = wa_saida-shipto
                                 AND   werks     = wa_saida-werks
                                 AND   matnr     = wa_saida-matnr
                                 AND   lifnr     = wa_saida-lifnr.
            IF sy-subrc = 0.
              COMMIT WORK.
            ENDIF.
          ENDIF.
        ENDLOOP.
        DELETE it_saida WHERE del = 'X'.

        CLEAR e_ucomm.

    ENDCASE.

*** Método de atualização de dados na Tela

    CALL METHOD g_grid->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fieldcat.

    CALL METHOD g_grid->refresh_table_display( is_stable = wa_stable ).


  ENDMETHOD.

ENDCLASS.

"
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.

  PARAMETER:      p_usnam LIKE zmmt0131-usuario_lib DEFAULT sy-uname,
                  p_werks LIKE zmmt0168-werks,
                  p_chave LIKE zmmt0168-chave_nfe.
  SELECT-OPTIONS: s_ebeln  FOR ekpo-ebeln NO INTERVALS MODIF ID a.
SELECTION-SCREEN: END OF BLOCK b1.

*&---------------------------------------------------------------------*
*&       P R O C E S S A M E N T O                                     *
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  SELECT SINGLE *
      FROM zmmt0131
      INTO wa_zmmt0131
      WHERE usuario_lib EQ p_usnam
        AND werks       EQ p_werks.
  IF sy-subrc = 0.
    IF wa_zmmt0131-nao_chave IS INITIAL AND p_chave IS INITIAL.
      MESSAGE 'Informe a Chave de Acesso' TYPE 'I'.
      SET CURSOR FIELD 'P_CHAVE' .
    ENDIF.
    IF p_chave IS NOT INITIAL AND s_ebeln IS INITIAL.
      MESSAGE 'Informe ao menos um pedido' TYPE 'I'.
      SET CURSOR FIELD 'P_CHAVE' .
    ENDIF.
  ENDIF.


  "Fecha campo Usuário
  LOOP AT SCREEN.

    IF screen-name = 'P_USNAM' .
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 = 'A'.
      IF p_chave IS INITIAL.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
        MODIFY SCREEN.
      ELSE.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

  ENDLOOP.

START-OF-SELECTION.

  SELECT SINGLE *
       FROM zmmt0131
       INTO wa_zmmt0131
       WHERE usuario_lib EQ p_usnam
         AND werks       EQ p_werks.
  IF sy-subrc = 0.
    IF wa_zmmt0131-nao_chave IS INITIAL AND p_chave IS INITIAL.
      EXIT.
    ENDIF.
    IF p_chave IS NOT INITIAL AND s_ebeln IS INITIAL.
      EXIT.
    ENDIF.
  ENDIF.
  PERFORM  zf_seleciona_dados.

  "Chama tela de Parâmetros
  CALL SCREEN 0100.


FORM zf_seleciona_dados.
  DATA vlifnr      TYPE lfa1-lifnr.
  DATA vstcd3      TYPE lfa1-stcd3.

  REFRESH it_saida.
  v_werks = p_werks.

  CLEAR p_ebeln.

  LOOP AT s_ebeln INTO DATA(ws_ebeln).
    w_ebeln-sign    = ws_ebeln-sign.
    w_ebeln-option  = ws_ebeln-option.
    w_ebeln-low     = ws_ebeln-low.
    APPEND w_ebeln TO p_ebeln.
  ENDLOOP.

  SELECT SINGLE *
    FROM zmmt0131
    INTO wa_zmmt0131
    WHERE usuario_lib EQ p_usnam
      AND werks       EQ p_werks.


  IF sy-subrc IS NOT INITIAL.
*-CS2025000249-02.05.2025-#174115-JT-inicio
    CASE lc_inb_aceite.
      WHEN abap_off.
        MESSAGE i000(z01) WITH TEXT-e01 TEXT-e02 DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      WHEN abap_true.
        lc_seqlog = lc_seqlog + 1.
        lc_zcl_nfe->set_add_log_nfe( EXPORTING i_transacao = 'ZFIS55' i_type = 'E' i_id  = 'Z01' i_num = 0 i_message_v1 = CONV #( TEXT-e01 ) i_message_v2 = CONV #( TEXT-e02 )
                                      CHANGING p_lc_sequencia = lc_seqlog ).
        RETURN.
    ENDCASE.
*-CS2025000249-02.05.2025-#174115-JT-fim
  ENDIF.

  IF p_chave IS NOT INITIAL.
    v_chave = p_chave.
    wl_desactive = 'X'.
    SELECT SINGLE *
      INTO @DATA(wa_zib_nfe_dist_ter)
      FROM zib_nfe_dist_ter
      WHERE chave_nfe = @v_chave.

    IF sy-subrc IS NOT INITIAL.
*-CS2025000249-02.05.2025-#174115-JT-inicio
      CASE lc_inb_aceite.
        WHEN abap_off.
          MESSAGE i000(z01) WITH 'Chave Nfe não existe!' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        WHEN abap_true.
          lc_seqlog = lc_seqlog + 1.
          lc_zcl_nfe->set_add_log_nfe( EXPORTING i_transacao = 'ZFIS55' i_type = 'E' i_id  = 'Z01' i_num = 0 i_message_v1 = CONV #( 'Chave Nfe não existe!' )
                                        CHANGING p_lc_sequencia = lc_seqlog ).
          RETURN.
      ENDCASE.
*-CS2025000249-02.05.2025-#174115-JT-fim
    ENDIF.

    IF wa_zib_nfe_dist_ter-branch NE p_werks.
*-CS2025000249-02.05.2025-#174115-JT-inicio
      CASE lc_inb_aceite.
        WHEN abap_off.
          MESSAGE i000(z01) WITH 'Esta Chave Nfe não é desse centro!' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        WHEN abap_true.
          lc_seqlog = lc_seqlog + 1.
          lc_zcl_nfe->set_add_log_nfe( EXPORTING i_transacao = 'ZFIS55' i_type = 'E' i_id  = 'Z01' i_num = 0 i_message_v1 = CONV #( 'Esta Chave Nfe não é desse centro!' ) i_message_v2 = ''
                                        CHANGING p_lc_sequencia = lc_seqlog ).
          RETURN.
      ENDCASE.
*-CS2025000249-02.05.2025-#174115-JT-fim
    ENDIF.

    SELECT lfa1~stcd1
      FROM ekko
      INNER JOIN lfa1
      ON lfa1~lifnr = ekko~lifnr
      INTO TABLE @DATA(it_lfa1)
      WHERE ebeln IN @s_ebeln.

    DATA(_erro) = 'N'.
    IF it_lfa1[] IS INITIAL.
      _erro = 'S'.
    ENDIF.
    LOOP AT it_lfa1 INTO DATA(wa_lfa1).
      IF wa_lfa1-stcd1+0(8) NE wa_zib_nfe_dist_ter-forne_cnpj+0(8).
        _erro = 'S'.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF _erro = 'S'.
*-CS2025000249-02.05.2025-#174115-JT-inicio
      CASE lc_inb_aceite.
        WHEN abap_off.
          MESSAGE i000(z01) WITH 'Pedidos nao são do fornecedor da nota!' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        WHEN abap_true.
          lc_seqlog = lc_seqlog + 1.
          lc_zcl_nfe->set_add_log_nfe( EXPORTING i_transacao = 'ZFIS55' i_type = 'E' i_id  = 'Z01' i_num = 0 i_message_v1 = CONV #( 'Pedidos nao são do fornecedor da nota!' )
                                        CHANGING p_lc_sequencia = lc_seqlog ).
          RETURN.
      ENDCASE.
*-CS2025000249-02.05.2025-#174115-JT-fim
    ENDIF.

    vstcd3 = wa_zib_nfe_dist_ter-forne_ie.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = vstcd3
      IMPORTING
        output = vstcd3.
    CONCATENATE '%' vstcd3 '%' INTO vstcd3.
    SELECT SINGLE *
    INTO @DATA(_lfa1_for)
    FROM lfa1
    WHERE stcd1 = @wa_zib_nfe_dist_ter-forne_cnpj
    AND   stcd3 LIKE @vstcd3.


    vlifnr = p_werks.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = vlifnr
      IMPORTING
        output = vlifnr.

    SELECT SINGLE *
    INTO @DATA(_lfa1_fil)
    FROM lfa1
    WHERE lifnr = @vlifnr.

    SELECT  *
     INTO TABLE @DATA(it_zib_nfe_dist_itm)
     FROM zib_nfe_dist_itm AS it
     WHERE chave_nfe = @p_chave.

  ELSE.
    v_chave = sy-uname && sy-datum.
    CLEAR wl_desactive.
  ENDIF.

  SELECT *
    INTO TABLE @DATA(t_zmmt0169)
    FROM zmmt0169.

  LOOP AT t_zmmt0169 INTO DATA(w_zmmt0169).
    w_zmmt0169-mwskz_1 = w_zmmt0169-mwskz+0(1).
    MODIFY t_zmmt0169 FROM w_zmmt0169 INDEX sy-tabix TRANSPORTING mwskz_1.
  ENDLOOP.
  MODIFY zmmt0169 FROM TABLE t_zmmt0169.
  COMMIT WORK.

  SELECT *
    FROM zmmt0168
    INTO TABLE it_zmmt0168
    WHERE chave_nfe = v_chave.

  IF it_zmmt0168 IS NOT INITIAL.
    SELECT *
    FROM marc
    INTO TABLE @DATA(it_marc)
      FOR ALL ENTRIES IN @it_zmmt0168
    WHERE werks = @it_zmmt0168-werks
      AND matnr = @it_zmmt0168-matnr.
  ENDIF.

  IF it_zmmt0168[] IS NOT INITIAL.
    LOOP AT it_zmmt0168 INTO  wa_zmmt0168.

      READ TABLE it_marc INTO DATA(ws_marc) WITH KEY werks = wa_zmmt0168-werks
                                                     matnr = wa_zmmt0168-matnr.
      IF sy-subrc EQ 0.
        wa_saida-ncm_mat_sap = ws_marc-steuc.
      ENDIF.

      wa_saida-chave_nfe  = wa_zmmt0168-chave_nfe.
      wa_saida-prod_item  = wa_zmmt0168-prod_item.
      wa_saida-shipfrom   = wa_zmmt0168-shipfrom.
      wa_saida-shipto     = wa_zmmt0168-shipto.
      wa_saida-werks      = wa_zmmt0168-werks.
      wa_saida-matnr      = wa_zmmt0168-matnr.
      wa_saida-ebeln      = wa_zmmt0168-ebeln.
      wa_saida-ebelp      = wa_zmmt0168-ebelp.
      wa_saida-mwskz_n    = wa_zmmt0168-mwskz.
      wa_saida-ncm_xml    = wa_zmmt0168-steuc.
      "
      SELECT SINGLE mwskz
        INTO  wa_saida-mwskz_p
        FROM ekpo
      WHERE ebeln = wa_zmmt0168-ebeln
      AND   ebelp = wa_zmmt0168-ebelp.
      "
      SELECT SINGLE maktx
        FROM makt
        INTO wa_saida-desc_mat
        WHERE spras = sy-langu
        AND   matnr = wa_zmmt0168-matnr.
      wa_saida-desc_xml   = wa_zmmt0168-desc_xml.
      wa_saida-lifnr      = wa_zmmt0168-lifnr.
      wa_saida-rate       = wa_zmmt0168-rate.
      wa_saida-base       = wa_zmmt0168-base.
      wa_saida-usnasm     = sy-uname.
      wa_saida-zdt_atual  = sy-datum.
      wa_saida-zhr_atual  = sy-uzeit.
      SELECT SINGLE *
      INTO wa_j_1btxic3
      FROM j_1btxic3
      WHERE land1     = 'BR'
      AND   shipfrom  = wa_saida-shipfrom
      AND   shipto    = wa_saida-shipto
      AND   gruop     = '31'
      AND   value     = wa_saida-werks
      AND   value2    = wa_saida-matnr
      AND   value3    = wa_saida-lifnr.
      IF sy-subrc = 0.
        wa_saida-icon = icon_complete.
        wa_saida-ratej       = wa_j_1btxic3-rate.
        wa_saida-basej       = wa_j_1btxic3-base.
      ELSE.
        wa_saida-icon = icon_checked.
      ENDIF.

      FREE wa_saida-celltab.
      IF p_chave IS NOT INITIAL.
        wa_estilo =  VALUE #(  ( fieldname = 'SHIPFROM'        style = cl_gui_alv_grid=>mc_style_disabled )
                               ( fieldname = 'SHIPTO'          style = cl_gui_alv_grid=>mc_style_disabled )
                               ( fieldname = 'WERKS'           style = cl_gui_alv_grid=>mc_style_disabled )
                               ( fieldname = 'MATNR'           style = cl_gui_alv_grid=>mc_style_enabled )

                               ( fieldname = 'NCM_XML'        style = cl_gui_alv_grid=>mc_style_disabled )
                               ( fieldname = 'NCM_MAT_SAP'    style = cl_gui_alv_grid=>mc_style_disabled )

                               ( fieldname = 'LIFNR'           style = cl_gui_alv_grid=>mc_style_disabled )
                               ( fieldname = 'RATE'            style = cl_gui_alv_grid=>mc_style_disabled )
                               ( fieldname = 'BASE'            style = cl_gui_alv_grid=>mc_style_disabled )
                               ( fieldname = 'RATEJ'           style = cl_gui_alv_grid=>mc_style_disabled )
                               ( fieldname = 'BASEJ'           style = cl_gui_alv_grid=>mc_style_disabled ) ).
      ELSE.
        wa_estilo = VALUE #( ( fieldname = 'SHIPFROM'        style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'SHIPTO'          style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'WERKS'           style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'MATNR'           style = cl_gui_alv_grid=>mc_style_disabled )

                             ( fieldname = 'NCM_XML'         style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'NCM_MAT_SAP'     style = cl_gui_alv_grid=>mc_style_disabled )

                             ( fieldname = 'LIFNR'           style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'EBELN'           style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'EBELP'           style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'MWSKZ_P'         style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'MWSKZ_N'         style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'RATE'            style = cl_gui_alv_grid=>mc_style_enabled )
                             ( fieldname = 'BASE'            style = cl_gui_alv_grid=>mc_style_enabled )
                             ( fieldname = 'RATEJ'           style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'BASEJ'           style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'USNAM'           style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'ZDT_ATUAL'       style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'ZHR_ATUAL'       style = cl_gui_alv_grid=>mc_style_disabled )   ).
      ENDIF.

      INSERT LINES OF wa_estilo INTO TABLE wa_saida-celltab.
      APPEND wa_saida TO it_saida.

      CLEAR: wa_saida.
    ENDLOOP.
  ENDIF.
  SORT it_zmmt0168 BY prod_item.
  LOOP AT it_zib_nfe_dist_itm INTO  DATA(wa_zib_nfe_dist_itm).
    READ TABLE it_zmmt0168 INTO  wa_zmmt0168 WITH KEY prod_item = wa_zib_nfe_dist_itm-prod_item BINARY SEARCH.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
    wa_saida-ncm_xml    = |{ wa_zib_nfe_dist_itm-prod_ncm+0(4) }.{ wa_zib_nfe_dist_itm-prod_ncm+4(2) }.{ wa_zib_nfe_dist_itm-prod_ncm+6(2) }|.
    wa_saida-chave_nfe  = wa_zib_nfe_dist_itm-chave_nfe.
    wa_saida-prod_item  = wa_zib_nfe_dist_itm-prod_item.
    wa_saida-shipfrom   = _lfa1_for-regio.
    wa_saida-shipto     = _lfa1_fil-regio.
    wa_saida-werks      = p_werks.
    wa_saida-matnr      = 0.
    wa_saida-ebeln      = 0.
    wa_saida-ebelp      = 0.

    wa_saida-mwskz_p    = ''.
    wa_saida-mwskz_n    = ''.

    wa_saida-desc_xml   = wa_zib_nfe_dist_itm-prod_descricao.
    wa_saida-lifnr      = _lfa1_for-lifnr.
    wa_saida-rate       = ( wa_zib_nfe_dist_itm-icms_valor / wa_zib_nfe_dist_itm-icms_base ) * 100.
    wa_saida-base       = ( wa_zib_nfe_dist_itm-icms_base * 100 ) /
                          ( wa_zib_nfe_dist_itm-prod_vlr_total_b +
                            wa_zib_nfe_dist_itm-prod_vl_outro    +
                            wa_zib_nfe_dist_itm-prod_vl_frete    +
                            wa_zib_nfe_dist_itm-prod_vl_seguro   +
                            wa_zib_nfe_dist_itm-ipi_valor        +
                            wa_zib_nfe_dist_itm-icms_st_valor     -
                            wa_zib_nfe_dist_itm-prod_vl_desconto ).
    IF wa_saida-rate GT 0 AND wa_saida-base GT 0.
      wa_saida-icon       =  icon_warning.
    ELSE.
      wa_saida-icon       =  icon_incomplete.
    ENDIF.
    wa_saida-usnasm     = sy-uname.
    wa_saida-zdt_atual  = sy-datum.
    wa_saida-zhr_atual  = sy-uzeit.

    FREE wa_saida-celltab.
    wa_estilo =  VALUE #(  ( fieldname = 'SHIPFROM'        style = cl_gui_alv_grid=>mc_style_disabled )
                           ( fieldname = 'SHIPTO'          style = cl_gui_alv_grid=>mc_style_disabled )
                           ( fieldname = 'WERKS'           style = cl_gui_alv_grid=>mc_style_disabled )
                           ( fieldname = 'MATNR'           style = cl_gui_alv_grid=>mc_style_enabled )

                           ( fieldname = 'NCM_XML'         style = cl_gui_alv_grid=>mc_style_disabled )
                           ( fieldname = 'NCM_MAT_SAP'     style = cl_gui_alv_grid=>mc_style_disabled )

                           ( fieldname = 'LIFNR'           style = cl_gui_alv_grid=>mc_style_disabled )
                           ( fieldname = 'EBELN'           style = cl_gui_alv_grid=>mc_style_disabled )
                           ( fieldname = 'EBELP'           style = cl_gui_alv_grid=>mc_style_disabled )
                           ( fieldname = 'MWSKZ_P'         style = cl_gui_alv_grid=>mc_style_disabled )
                           ( fieldname = 'MWSKZ_N'         style = cl_gui_alv_grid=>mc_style_disabled )
                           ( fieldname = 'RATE'            style = cl_gui_alv_grid=>mc_style_disabled )
                           ( fieldname = 'BASE'            style = cl_gui_alv_grid=>mc_style_disabled )
                           ( fieldname = 'RATEJ'           style = cl_gui_alv_grid=>mc_style_disabled )
                           ( fieldname = 'BASEJ'           style = cl_gui_alv_grid=>mc_style_disabled ) ).

    INSERT LINES OF wa_estilo INTO TABLE wa_saida-celltab.
    APPEND wa_saida TO it_saida.

    CLEAR: wa_saida.
  ENDLOOP.

  SORT it_saida BY prod_item icon lifnr matnr desc_mat desc_xml.

  DATA(lt_saida) = it_saida.
  SORT lt_saida BY werks.
  DELETE ADJACENT DUPLICATES FROM lt_saida COMPARING werks.
  IF lt_saida IS NOT INITIAL.

    SELECT *
      FROM j_1bbranch
      INTO TABLE t_branch
      FOR ALL ENTRIES IN lt_saida
      WHERE branch = lt_saida-werks.
    IF sy-subrc IS INITIAL.
      SORT t_branch BY branch.
    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: lt_f4       TYPE lvc_t_f4     WITH HEADER LINE.

  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR  'TL_0100'.

  PERFORM zf_alv.

  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = 'CONTAINER'.

    IF g_grid IS INITIAL AND g_custom_container IS NOT INITIAL.

      CREATE OBJECT g_grid
        EXPORTING
          i_parent = g_custom_container.
    ENDIF.


    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = g_grid.

** Register event handler
    SET HANDLER obg_toolbar->on_toolbar FOR g_grid.
    SET HANDLER obg_toolbar->handle_user_command FOR g_grid.

    REFRESH tl_function.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.


    wa_layout-stylefname = 'CELLTAB'.
*    wa_layout-cwidth_opt  = 'X'.
*    wa_layout-zebra       = 'X'.
*    wa_layout-no_toolbar  = ''.
*    wa_layout-sel_mode   = 'A'.

    wa_layout-no_rowmark  = ' '.

    wa_variant-report = sy-repid.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
*       is_variant           = wa_variant
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_default            = 'X'
      CHANGING
        it_outtab            = it_saida
        it_fieldcatalog      = it_fieldcat.


    lt_f4-fieldname = 'MATNR'.
    lt_f4-register = 'X'.
    lt_f4-getbefore = 'X'.
    lt_f4-chngeafter ='X'.
    APPEND lt_f4.

    CALL METHOD g_grid->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].


    SET HANDLER: lcl_hander=>on_data_changed FOR g_grid,
                 lcl_hander=>on_data_changed_finished FOR g_grid,
                 lcl_hander=>on_onf4 FOR g_grid.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
    "
    CALL METHOD g_grid->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fieldcat.

    CALL METHOD g_grid->refresh_table_display( is_stable = wa_stable ).
  ELSE.
    CALL METHOD g_grid->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fieldcat.

    CALL METHOD g_grid->refresh_table_display( is_stable = wa_stable ).
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
    WHEN 'SAVE'.
      PERFORM zf_save.

    WHEN 'C_J1BTAX'.
      REFRESH it_zmmt0168.
      SELECT *
        FROM zmmt0168
        INTO TABLE it_zmmt0168
        WHERE chave_nfe EQ v_chave
*        AND   ( ( rate      GT 0  AND   base      GT 0 ) OR mwskz NE ' ' )
        AND   steuc NE space.


      IF sy-subrc NE 0.
        MESSAGE 'Gravar informações para atualizar o ICMS e NCM!'  TYPE 'E'.
        EXIT.
      ENDIF.

      PERFORM zj1btax.

  ENDCASE.
ENDMODULE.

FORM zj1btax.
  CONSTANTS:
    read(4) TYPE c VALUE 'READ',
    edit(4) TYPE c VALUE 'EDIT',
    save(4) TYPE c VALUE 'SAVE'.

  DATA: it_xml_lines  TYPE STANDARD TABLE OF zst_ped_lines,
        it_xml_header TYPE STANDARD TABLE OF zstped_header WITH EMPTY KEY,
        wa_xml_lines  TYPE zst_ped_lines,
        vline_num     TYPE zst_ped_lines-line_num,
        lv_ponumb     TYPE ebeln,
        lv_poitem     TYPE ebelp,
        lv_mwskz      TYPE zmmt0168-mwskz,
        vline         TYPE i.

  DATA t_filter     TYPE zif_integracao_coupa_ped_comp=>tt_filter.
  DATA go_int_ped   TYPE REF TO zcl_integracao_coupa_ped_comp.
  CONSTANTS gc_service TYPE /ui2/service_name VALUE 'COUPA_INT_ENVIA_PED_COMPRA_IVA'.

  REFRESH it_xml_lines.
  TRY.
      CREATE OBJECT go_int_ped
        EXPORTING
          i_servico = gc_service.
      it_zmmt0168_aux[] = it_zmmt0168[].
*      DELETE it_zmmt0168_aux WHERE mwskz IS INITIAL.
      SORT it_zmmt0168_aux BY ebeln.
      DELETE ADJACENT DUPLICATES FROM it_zmmt0168_aux COMPARING ebeln.
      LOOP AT it_zmmt0168_aux INTO wa_zmmt0168.

        IF wa_zmmt0168-mwskz IS INITIAL.
          CONTINUE.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_zmmt0168-ebeln
          IMPORTING
            output = wa_zmmt0168-ebeln.
        SELECT SINGLE bsart
          INTO @DATA(_bsart)
          FROM ekko
          WHERE ebeln = @wa_zmmt0168-ebeln.
        IF _bsart+0(1) = 'Y'.
          FREE: t_filter, it_xml_lines.
          APPEND INITIAL LINE TO t_filter REFERENCE INTO DATA(lo_wa_filter).
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_zmmt0168-ebeln
            IMPORTING
              output = lo_wa_filter->value.
          lo_wa_filter->field = 'po-number'.
          go_int_ped->zif_integracao_coupa_ped_comp~set_ds_url( e_metodo  = 'GET'
                                                                it_filter = t_filter ).
          go_int_ped->zif_integracao_coupa_ped_comp~set_send_msg(
            IMPORTING
              e_id_integracao = DATA(e_id_integracao_post)
              e_integracao    = DATA(e_integracao_post) ).

          go_int_ped->realiza_quebra_xml( IMPORTING et_xml_header = it_xml_header
                                                    et_xml_lines  = it_xml_lines ).
          "Aplica somente IVA Informados
          LOOP AT it_xml_lines INTO wa_xml_lines.
            DATA(tabix) = sy-tabix.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_xml_lines-po_number
              IMPORTING
                output = lv_ponumb.
            lv_poitem = wa_xml_lines-line_num * 10.

            READ TABLE it_zmmt0168 INTO wa_zmmt0168 WITH KEY  ebeln = lv_ponumb
                                                              ebelp = lv_poitem.
            lv_mwskz = wa_xml_lines-tax_code+0(2).
            IF lv_mwskz EQ wa_zmmt0168-mwskz OR sy-subrc NE 0.
              CLEAR wa_xml_lines-tax_code.
            ELSE.
              wa_xml_lines-tax_code = wa_zmmt0168-mwskz.
            ENDIF.
            MODIFY it_xml_lines FROM wa_xml_lines INDEX tabix TRANSPORTING tax_code.
          ENDLOOP.

          DELETE it_xml_lines WHERE tax_code IS INITIAL.

          IF it_xml_lines[] IS NOT INITIAL.
            go_int_ped->atualiza_iva_pedido_coupa( it_xml_lines = it_xml_lines ).
*        READ TABLE it_xml_header REFERENCE INTO DATA(wa_xml_header) INDEX 1.
*        go_int_ped->atualiza_exported_pedido_iva( iw_xml_header = wa_xml_header->* ).
          ENDIF.
        ENDIF.
      ENDLOOP.

  ENDTRY.


*&-------------------------------------------------------------------------------------------------------------------
*&  Inicio - Gravar informações do NCM da nota no cadastro do material e pedido /  USER STORY 119409 / AOENNING
*&-------------------------------------------------------------------------------------------------------------------
  IF it_zmmt0168[] IS NOT INITIAL.  "*-CS2025000249-02.05.2025-#174115-JT
    SELECT *
      FROM ekpo
      INTO TABLE @DATA(it_ekpo)
       FOR ALL ENTRIES IN @it_zmmt0168
     WHERE ebeln EQ @it_zmmt0168-ebeln.

    SELECT *
      FROM marc
      INTO TABLE @DATA(it_marc)
       FOR ALL ENTRIES IN @it_zmmt0168
     WHERE matnr EQ @it_zmmt0168-matnr
       AND werks EQ @it_zmmt0168-werks.
  ENDIF.

  LOOP AT it_zmmt0168 INTO wa_zmmt0168.
*&-------------------------------------------------------------------------------------------------------------------
*&  "Alterar NCM do cadastro material.
*&-------------------------------------------------------------------------------------------------------------------

    "Cabeçalho do material.
    gs_headdata-material = |{ wa_zmmt0168-matnr ALPHA = IN }|.

    READ TABLE it_marc INTO DATA(ws_marc) WITH KEY matnr = wa_zmmt0168-matnr
                                                   werks = wa_zmmt0168-werks.
    IF sy-subrc EQ 0.
      IF wa_zmmt0168-steuc NE ws_marc-steuc.

        "Dados diversos / NCM.
        gs_plantdata-plant     = wa_zmmt0168-werks.
        gs_plantdata-ctrl_code = wa_zmmt0168-steuc.

        IF ws_marc-indus EQ space.
          gs_plantdata-mat_cfop  = '00'.
* INICIO - IR238744 - RBRIBEIRO - 20.06.2025
          gs_plantdatax-mat_cfop    = abap_true.
* FIM - IR238744 - RBRIBEIRO - 20.06.2025
        ENDIF.

        gs_plantdata-availcheck = 'KP'.
        IF ws_marc-mtvfp IS NOT INITIAL.
          gs_plantdata-availcheck = ws_marc-mtvfp.
        ENDIF.

        gs_plantdata-mrp_type   = 'ND'.
        IF ws_marc-dismm IS NOT INITIAL.
          gs_plantdata-mrp_type   = ws_marc-dismm.
        ENDIF.

        gs_plantdatax-plant       = wa_zmmt0168-werks.
        gs_plantdatax-ctrl_code   = abap_true.
        gs_plantdatax-availcheck  = abap_true.
        gs_plantdatax-mrp_type    = abap_true.
* INICIO - IR238744 - RBRIBEIRO - 20.06.2025
*        gs_plantdatax-mat_cfop    = abap_true.
* FIM - IR238744 - RBRIBEIRO - 20.06.2025
        CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
          EXPORTING
            headdata            = gs_headdata
            clientdata          = gs_clientdata
            clientdatax         = gs_clientdatax
            plantdata           = gs_plantdata
            plantdatax          = gs_plantdatax
            valuationdata       = gs_valuationdata
            valuationdatax      = gs_valuationdatax
          IMPORTING
            return              = gs_return
          TABLES
            unitsofmeasure      = gt_unitsofmeasure
            unitsofmeasurex     = gt_unitsofmeasurex
            materialdescription = gt_mat_description
            materiallongtext    = gt_mat_longtext
            returnmessages      = gt_ret_messages.

        IF gs_return-type EQ gc_s.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = gc_x.

          READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<ws_saida>) WITH KEY matnr = wa_zmmt0168-matnr
                                                                          werks = wa_zmmt0168-werks.
          IF sy-subrc EQ 0.
            <ws_saida>-ncm_mat_sap = wa_zmmt0168-steuc.
          ENDIF.

          APPEND VALUE #( message_v1 = 'Material atualizado com sucesso' ) TO  gs_log.

        ENDIF.
* INICIO - IR238744 - RBRIBEIRO - 08.07.2025
        CLEAR: ws_marc.
      ELSE.
        IF ws_marc-indus EQ space.
          gs_plantdata-mat_cfop  = '00'.
          gs_plantdatax-mat_cfop    = abap_true.
        ENDIF.
        CLEAR: ws_marc.
* FIM - IR238744 - RBRIBEIRO - 08.07.2025
      ENDIF.
    ENDIF.


*&-------------------------------------------------------------------------------------------------------------------
*&  "Alterar NCM e IVA no tem pedido.
*&-------------------------------------------------------------------------------------------------------------------
    READ TABLE it_ekpo INTO DATA(ws_ekpo) WITH KEY ebeln = wa_zmmt0168-ebeln
                                                   ebelp = wa_zmmt0168-ebelp.
    IF sy-subrc EQ 0.
      IF wa_zmmt0168-steuc NE ws_ekpo-j_1bnbm.

        it_poitem_ped = VALUE #( ( po_item = ws_ekpo-ebelp
                                   bras_nbm = wa_zmmt0168-steuc ) ).

        it_poitemx_ped = VALUE #( ( po_item = ws_ekpo-ebelp
                                   bras_nbm = abap_true ) ).

        FREE: it_return_ped.
        CALL FUNCTION 'BAPI_PO_CHANGE'
          EXPORTING
            purchaseorder = wa_zmmt0168-ebeln
          TABLES
            return        = it_return_ped
            poitem        = it_poitem_ped
            poitemx       = it_poitemx_ped
            popartner     = it_popartner.


        READ TABLE it_return_ped TRANSPORTING NO FIELDS WITH KEY type = 'E'.
        IF sy-subrc IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          APPEND VALUE #( message_v1 = 'Pedido atualizado com sucesso' ) TO  gs_log.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF lc_inb_aceite = abap_off.  "*-CS2025000249-02.05.2025-#174115-JT
    CALL METHOD g_grid->refresh_table_display( is_stable = wa_stable ).
  ENDIF.

*&-------------------------------------------------------------------------------------------------------------------
*&  Fim - Gravar informações do NCM da nota no cadastro do material e pedido /  USER STORY 119409 / AOENNING
*&-------------------------------------------------------------------------------------------------------------------

  DATA: chdat(8)   TYPE c,
        houtput(8) TYPE n.

  DATA  function    LIKE sy-ucomm.
  DATA: variant_for_selection LIKE  tvimv-variant,
        view_name             LIKE  dd02v-tabname.

  DATA: BEGIN OF status_j_1btxic3v. "state vector
          INCLUDE STRUCTURE vimstatus.
  DATA: END OF status_j_1btxic3v.

  DATA: BEGIN OF header OCCURS 1.
          INCLUDE STRUCTURE vimdesc.
  DATA: END OF header.

  DATA: BEGIN OF namtab OCCURS 50.
          INCLUDE STRUCTURE vimnamtab.
  DATA: END OF namtab.

  DATA: rangetab    TYPE TABLE OF vimsellist INITIAL SIZE 50
         WITH HEADER LINE,
        oc_rangetab TYPE TABLE OF vimsellist INITIAL SIZE 50.

  DATA: dpl_sellist    TYPE TABLE OF vimsellist INITIAL SIZE 50 WITH HEADER LINE.

  DATA: BEGIN OF e071k_tab OCCURS 100.    "keys of changed entries
          INCLUDE STRUCTURE e071k.       "(used as parameter for VIEWPROC)
  DATA: END OF e071k_tab.

  DATA: org_crit_inst TYPE vimty_oc_type, lockuser TYPE sy-uname.
  DATA: excl_cua_funct LIKE vimexclfun OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF j_1btxic3v_extract OCCURS 0010.
          INCLUDE STRUCTURE j_1btxic3v.
          INCLUDE STRUCTURE vimflagtab.
  DATA: END OF j_1btxic3v_extract.
* Table for all entries loaded from database
  DATA: BEGIN OF j_1btxic3v_total OCCURS 0010.
          INCLUDE STRUCTURE j_1btxic3v.
          INCLUDE STRUCTURE vimflagtab.
  DATA: END OF j_1btxic3v_total.


  REFRESH namtab.
  CLEAR   namtab.
  REFRESH header.
  CLEAR   header.
  REFRESH rangetab.
  CLEAR   rangetab.
  CALL FUNCTION 'VIEW_GET_DDIC_INFO'
    EXPORTING
      viewname              = 'J_1BTXIC3V' "view_name
      variant_for_selection = variant_for_selection
    TABLES
      x_header              = header
      x_namtab              = namtab
      sellist               = rangetab
    EXCEPTIONS
      no_tvdir_entry        = 3
      table_not_found       = 5.

  "gravar GRUOP 31  rangetab
  READ TABLE rangetab ASSIGNING FIELD-SYMBOL(<rangetab>) WITH KEY viewfield = 'GRUOP'.
  IF sy-subrc = 0.
    <rangetab>-value = '31'.
  ENDIF.
  READ TABLE rangetab ASSIGNING <rangetab> WITH KEY viewfield = 'LAND1'.
  IF sy-subrc = 0.
    <rangetab>-value = 'BR'.
  ENDIF.

  READ TABLE header INDEX 1.
  CALL FUNCTION 'VIEWPROC_J_1BTXIC3V'
    EXPORTING
      fcode                     = read
      view_action               = 'U' "view_action
      view_name                 = 'J_1BTXIC3V' "view_name
    TABLES
      excl_cua_funct            = excl_cua_funct
      extract                   = j_1btxic3v_extract
      total                     = j_1btxic3v_total
      x_header                  = header
      x_namtab                  = namtab
      dba_sellist               = rangetab "dba_sellist
      dpl_sellist               = dpl_sellist
      corr_keytab               = e071k_tab
    EXCEPTIONS
      missing_corr_number       = 1
      no_value_for_subset_ident = 2.
  CASE sy-subrc.
    WHEN 1.
      RAISE missing_corr_number.
    WHEN 2.
      RAISE no_value_for_subset_ident.
  ENDCASE.
  "
  status_j_1btxic3v-upd_flag = 'X'.
*
  REFRESH:  j_1btxic3v_extract, j_1btxic3v_total.
  "
  LOOP AT it_zmmt0168 INTO wa_zmmt0168.
    IF wa_zmmt0168-rate = 0 OR wa_zmmt0168-base = 0.
      CONTINUE.
    ENDIF.
    CLEAR: j_1btxic3v_extract, j_1btxic3v_total.
    MOVE-CORRESPONDING wa_zmmt0168 TO j_1btxic3v_extract.
    j_1btxic3v_extract-land1      = 'BR'.
    j_1btxic3v_extract-gruop      = '31'.
    j_1btxic3v_extract-shipfrom   = wa_zmmt0168-shipfrom.
    j_1btxic3v_extract-shipto     = wa_zmmt0168-shipto.
    j_1btxic3v_extract-value      = wa_zmmt0168-werks.
    j_1btxic3v_extract-value2     = wa_zmmt0168-matnr.
    j_1btxic3v_extract-value3     = wa_zmmt0168-lifnr.
    j_1btxic3v_extract-rate       = wa_zmmt0168-rate.
    j_1btxic3v_extract-base       = wa_zmmt0168-base.
    IF wa_zmmt0168-base LT 100.
      j_1btxic3v_extract-taxlaw     = 'IA2'.
    ENDIF.
    SELECT SINGLE *
       INTO wa_j_1btxic3
       FROM j_1btxic3
       WHERE land1     = 'BR'
       AND   shipfrom  = wa_zmmt0168-shipfrom
       AND   shipto    = wa_zmmt0168-shipto
       AND   gruop     = '31'
       AND   value     = wa_zmmt0168-werks
       AND   value2    = wa_zmmt0168-matnr
       AND   value3    = wa_zmmt0168-lifnr.
    IF sy-subrc = 0.
      j_1btxic3v_extract-validto    = wa_j_1btxic3-validto.
      j_1btxic3v_extract-validfrom  = wa_j_1btxic3-validfrom.
      j_1btxic3v_extract-action     = 'U'.
    ELSE.
*      MOVE sy-datum TO chdat.
      MOVE '20010101' TO chdat.
      houtput = '99999999' - chdat.
      j_1btxic3v_extract-validfrom  = houtput.
      MOVE '99991231' TO chdat.
      houtput = '99999999' - chdat.
      j_1btxic3v_extract-validto    = houtput.
      j_1btxic3v_extract-action     = 'N'.
    ENDIF.
    APPEND j_1btxic3v_extract.
    MOVE-CORRESPONDING j_1btxic3v_extract TO j_1btxic3v_total.
    APPEND j_1btxic3v_total.
  ENDLOOP.

  IF j_1btxic3v_extract[] IS NOT INITIAL.
    CALL FUNCTION 'VIEWPROC_J_1BTXIC3V'
      EXPORTING
        fcode                     = save
        view_action               = 'U' "maint_mode
        view_name                 = 'J_1BTXIC3V' "view_name
        corr_number               = ' ' "corr_number
      IMPORTING
        update_required           = status_j_1btxic3v-upd_flag
      TABLES
        excl_cua_funct            = excl_cua_funct
        extract                   = j_1btxic3v_extract
        total                     = j_1btxic3v_total
        x_header                  = header
        x_namtab                  = namtab
        dba_sellist               = rangetab "dba_sellist
        dpl_sellist               = dpl_sellist
        corr_keytab               = e071k_tab
      EXCEPTIONS
        missing_corr_number       = 1
        no_value_for_subset_ident = 2
        saving_correction_failed  = 3.
    CASE sy-subrc.
      WHEN 1.
        RAISE missing_corr_number.
      WHEN 2.
        RAISE no_value_for_subset_ident.
      WHEN 3.
    ENDCASE.
    "
    CLEAR  status_j_1btxic3v-upd_flag.
    function = save.
    excl_cua_funct-function = 'ANZG'.
    APPEND excl_cua_funct.
    excl_cua_funct-function = 'NEWL'.
    APPEND excl_cua_funct.
    excl_cua_funct-function = 'KOPE'.
    APPEND excl_cua_funct.
    excl_cua_funct-function = 'DELE'.
    APPEND excl_cua_funct.
    excl_cua_funct-function = 'ORGI'.
    APPEND excl_cua_funct.
    excl_cua_funct-function = 'MKAL'.
    APPEND excl_cua_funct.
    excl_cua_funct-function = 'MKBL'.
    APPEND excl_cua_funct.
    excl_cua_funct-function = 'MKLO'.
    APPEND excl_cua_funct.
    excl_cua_funct-function = 'HELP'.
    APPEND excl_cua_funct.

    CALL FUNCTION 'VIEWPROC_J_1BTXIC3V'
      EXPORTING
*       fcode                     = edit                                                         "*-CS2025000249-02.05.2025-#174115-JT
        fcode                     = COND #( WHEN lc_inb_aceite = abap_true THEN save ELSE edit ) "*-CS2025000249-02.05.2025-#174115-JT
        view_action               = 'U' "maint_mode
        view_name                 = 'J_1BTXIC3V' "view_name
        corr_number               = '' "corr_number
      IMPORTING
        ucomm                     = function
        update_required           = status_j_1btxic3v-upd_flag
      TABLES
        excl_cua_funct            = excl_cua_funct
        extract                   = j_1btxic3v_extract
        total                     = j_1btxic3v_total
        x_header                  = header
        x_namtab                  = namtab
        dba_sellist               = rangetab "dba_sellist
        dpl_sellist               = dpl_sellist
        corr_keytab               = e071k_tab
      EXCEPTIONS
        missing_corr_number       = 1
        no_value_for_subset_ident = 2.
    CASE sy-subrc.
      WHEN 1.

      WHEN 2.

      WHEN OTHERS.
        EXIT.
    ENDCASE.
  ENDIF.

  IF gs_log[]  IS NOT INITIAL.
    MESSAGE s024(sd) WITH 'NCM atualizado com sucesso'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_alv .
  IF p_chave IS INITIAL.
    it_fieldcat =  VALUE lvc_t_fcat(
      ( ref_table = ' '         ref_field = ' '        fieldname = 'ICON'             coltext =   'Status'           outputlen = '08'  checkbox = ''   edit = ''  icon = 'X' )
      ( ref_table = 'ZMMT0168'  ref_field = 'SHIPFROM' fieldname = 'SHIPFROM'         coltext =   'Emissor'          outputlen = '08'  checkbox = ''   edit = ''   )
      ( ref_table = 'ZMMT0168'  ref_field = 'SHIPTO'   fieldname = 'SHIPTO'           coltext =   'Receptor'         outputlen = '08'  checkbox = ''   edit = ''   )
      ( ref_table = 'T001W'     ref_field = 'WERKS'    fieldname = 'WERKS'            coltext =   'Centro'           outputlen = '06'  checkbox = ''   edit = ''   )
      ( ref_table = ' '         ref_field = ' '        fieldname = 'MATNR'            coltext =   'Material'         outputlen = '18'  checkbox = ''   edit = ''   f4availabl = 'X' )

      ( ref_table = 'MARC'      ref_field = 'STEUC'    fieldname = 'NCM_XML'          coltext =   'NCM XML'         outputlen = '12'  checkbox = ''   edit = ''   )
      ( ref_table = 'MARC'      ref_field = 'STEUC'    fieldname = 'NCM_MAT_SAP'      coltext =   'NCM SAP'         outputlen = '12'  checkbox = ''   edit = ''   )

      ( ref_table = ' '         ref_field = ' '        fieldname = 'DESC_MAT'         coltext =   'Desc.Material'    outputlen = '40'  checkbox = ''   edit = ''   )
      ( ref_table = 'LFA1'      ref_field = 'LIFNR'    fieldname = 'LIFNR'            coltext =   'Fornecedor'       outputlen = '12'  checkbox = ''   edit = ''   )
      ( ref_table = 'ZMMT0168'  ref_field = 'RATE'     fieldname = 'RATE'             coltext =   'Aliquota NFE'     outputlen = '12'  checkbox = ''   edit = ''   )
      ( ref_table = 'ZMMT0168'  ref_field = 'BASE'     fieldname = 'BASE'             coltext =   'Base NFE'         outputlen = '12'  checkbox = ''   edit = ''   )
      ( ref_table = 'ZMMT0168'  ref_field = 'RATE'     fieldname = 'RATEJ'            coltext =   'Aliquota SAP'     outputlen = '12'  checkbox = ''   edit = ''   )
      ( ref_table = 'ZMMT0168'  ref_field = 'BASE'     fieldname = 'BASEJ'            coltext =   'Base SAP'         outputlen = '12'  checkbox = ''   edit = ''   ) ).
  ELSE.
    it_fieldcat =  VALUE lvc_t_fcat(
       ( ref_table = ' '         ref_field = ' '        fieldname = 'ICON'             coltext =   'Status'           outputlen = '08'  checkbox = ''   edit = ''  icon = 'X' )
       ( ref_table = 'ZMMT0168'  ref_field = 'SHIPFROM' fieldname = 'SHIPFROM'         coltext =   'Emissor'          outputlen = '08'  checkbox = ''   edit = ''   )
       ( ref_table = 'ZMMT0168'  ref_field = 'SHIPTO'   fieldname = 'SHIPTO'           coltext =   'Receptor'         outputlen = '08'  checkbox = ''   edit = ''   )
       ( ref_table = 'T001W'     ref_field = 'WERKS'    fieldname = 'WERKS'            coltext =   'Centro'           outputlen = '06'  checkbox = ''   edit = ''   )
       ( ref_table = ' '         ref_field = ' '        fieldname = 'MATNR'            coltext =   'Material'         outputlen = '18'  checkbox = ''   edit = ''   f4availabl = 'X' )

       ( ref_table = 'MARC'      ref_field = 'STEUC'    fieldname = 'NCM_XML'          coltext =   'NCM XML'          outputlen = '12'  checkbox = ''   edit = ''   )
       ( ref_table = 'MARC'      ref_field = 'STEUC'    fieldname = 'NCM_MAT_SAP'      coltext =   'NCM SAP'          outputlen = '12'  checkbox = ''   edit = ''   )

       ( ref_table = ' '         ref_field = ' '        fieldname = 'EBELN'            coltext =   'Pedido'           outputlen = '12'  checkbox = ''   edit = ''   )
       ( ref_table = ' '         ref_field = ' '        fieldname = 'EBELP'            coltext =   'Item'             outputlen = '08'  checkbox = ''   edit = ''   )
       ( ref_table = ' '         ref_field = ' '        fieldname = 'MWSKZ_P'          coltext =   'IVA Pedido'       outputlen = '08'  checkbox = ''   edit = ''   )
       ( ref_table = ' '         ref_field = ' '        fieldname = 'MWSKZ_N'          coltext =   'IVA NFE'          outputlen = '08'  checkbox = ''   edit = ''   )
       ( ref_table = ' '         ref_field = ' '        fieldname = 'DESC_MAT'         coltext =   'Desc.Material'    outputlen = '40'  checkbox = ''   edit = ''   )
       ( ref_table = ' '         ref_field = ' '        fieldname = 'DESC_XML'         coltext =   'Desc. XML'        outputlen = '40'  checkbox = ''   edit = ''   )
       ( ref_table = ' '         ref_field = ' '        fieldname = 'ICMS_CST'         coltext =   'CST'              outputlen = '05'  checkbox = ''   edit = ''   )
       ( ref_table = 'LFA1'      ref_field = 'LIFNR'    fieldname = 'LIFNR'            coltext =   'Fornecedor'       outputlen = '12'  checkbox = ''   edit = ''   )
       ( ref_table = 'ZMMT0168'  ref_field = 'RATE'     fieldname = 'RATE'             coltext =   'Aliquota NFE'     outputlen = '12'  checkbox = ''   edit = ''   )
       ( ref_table = 'ZMMT0168'  ref_field = 'BASE'     fieldname = 'BASE'             coltext =   'Base NFE'         outputlen = '12'  checkbox = ''   edit = ''   )
       ( ref_table = 'ZMMT0168'  ref_field = 'RATE'     fieldname = 'RATEJ'            coltext =   'Aliquota SAP'     outputlen = '12'  checkbox = ''   edit = ''   )
       ( ref_table = 'ZMMT0168'  ref_field = 'BASE'     fieldname = 'BASEJ'            coltext =   'Base SAP'         outputlen = '12'  checkbox = ''   edit = ''   ) ).
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_save.

  DATA: it_save TYPE TABLE OF zmmt0168,
        wa_save TYPE zmmt0168.

  LOOP AT it_saida INTO wa_saida.
    IF wa_saida-matnr IS INITIAL.
      CONTINUE.
    ENDIF.
    IF wa_saida-shipfrom   IS INITIAL.
*-CS2025000249-02.05.2025-#174115-JT-inicio
      CASE lc_inb_aceite.
        WHEN abap_off.
          MESSAGE |Campo Emissor obrigatório! linha { sy-tabix } | TYPE 'E'.
          REFRESH it_save.
          EXIT.
        WHEN abap_true.
          lc_seqlog = lc_seqlog + 1.
          MESSAGE e024(sd) WITH |Campo Emissor obrigatório! linha { sy-tabix } | INTO lv_message.
          lc_zcl_nfe->set_add_log_nfe( EXPORTING i_transacao = 'ZFIS55' i_type = 'E' i_id  = 'Z01' i_num = 0 i_message_v1 = CONV #( lv_message ) i_message_v2 = '' i_message_v3 = CONV #( abap_off ) i_message_v4   = CONV #( abap_off )
                                        CHANGING p_lc_sequencia = lc_seqlog ).
          REFRESH it_save.
          EXIT.
      ENDCASE.
*-CS2025000249-02.05.2025-#174115-JT-fim
    ENDIF.

    IF wa_saida-shipto  IS INITIAL.
*-CS2025000249-02.05.2025-#174115-JT-inicio
      CASE lc_inb_aceite.
        WHEN abap_off.
          MESSAGE |Campo Receptor obrigatório! linha { sy-tabix } | TYPE 'E'.
          REFRESH it_save.
          EXIT.
        WHEN abap_true.
          lc_seqlog = lc_seqlog + 1.
          MESSAGE e024(sd) WITH |Campo Receptor obrigatório! linha { sy-tabix } | INTO lv_message.
          lc_zcl_nfe->set_add_log_nfe( EXPORTING i_transacao = 'ZFIS55' i_type = 'E' i_id  = 'Z01' i_num = 0 i_message_v1 = CONV #( lv_message ) i_message_v2 = '' i_message_v3 = CONV #( abap_off ) i_message_v4   = CONV #( abap_off )
                                        CHANGING p_lc_sequencia = lc_seqlog ).
          REFRESH it_save.
          EXIT.
      ENDCASE.
*-CS2025000249-02.05.2025-#174115-JT-fim
    ENDIF.

    IF wa_saida-werks   IS INITIAL.
*-CS2025000249-02.05.2025-#174115-JT-inicio
      CASE lc_inb_aceite.
        WHEN abap_off.
          MESSAGE |Campo Centro obrigatório! linha { sy-tabix } | TYPE 'E'.
          REFRESH it_save.
          EXIT.
        WHEN abap_true.
          lc_seqlog = lc_seqlog + 1.
          MESSAGE e024(sd) WITH |Campo Centro obrigatório! linha { sy-tabix } | INTO lv_message.
          lc_zcl_nfe->set_add_log_nfe( EXPORTING i_transacao = 'ZFIS55' i_type = 'E' i_id  = 'Z01' i_num = 0 i_message_v1 = CONV #( lv_message ) i_message_v2 = '' i_message_v3 = CONV #( abap_off ) i_message_v4   = CONV #( abap_off )
                                        CHANGING p_lc_sequencia = lc_seqlog ).
          REFRESH it_save.
          EXIT.
      ENDCASE.
*-CS2025000249-02.05.2025-#174115-JT-fim
    ENDIF.

    IF wa_saida-matnr  IS INITIAL.
*-CS2025000249-02.05.2025-#174115-JT-inicio
      CASE lc_inb_aceite.
        WHEN abap_off.
          MESSAGE |Campo Material obrigatório! linha { sy-tabix } | TYPE 'E'.
          REFRESH it_save.
          EXIT.
        WHEN abap_true.
          lc_seqlog = lc_seqlog + 1.
          MESSAGE e024(sd) WITH |Campo Material obrigatório! linha { sy-tabix } | INTO lv_message.
          lc_zcl_nfe->set_add_log_nfe( EXPORTING i_transacao = 'ZFIS55' i_type = 'E' i_id  = 'Z01' i_num = 0 i_message_v1 = CONV #( lv_message ) i_message_v2 = '' i_message_v3 = CONV #( abap_off ) i_message_v4   = CONV #( abap_off )
                                        CHANGING p_lc_sequencia = lc_seqlog ).
          REFRESH it_save.
          EXIT.
      ENDCASE.
*-CS2025000249-02.05.2025-#174115-JT-fim
    ELSE.
      SELECT COUNT(*)
         FROM marc
         WHERE matnr = wa_saida-matnr
         AND   werks = wa_saida-werks.
      IF sy-subrc NE 0.
*-CS2025000249-02.05.2025-#174115-JT-inicio
        CASE lc_inb_aceite.
          WHEN abap_off.
            MESSAGE |Material não expandido para o centro linha { sy-tabix } | TYPE 'E'.
            REFRESH it_save.
            EXIT.
          WHEN abap_true.
            lc_seqlog = lc_seqlog + 1.
            MESSAGE e024(sd) WITH |Material não expandido para o centro linha { sy-tabix } | INTO lv_message.
            lc_zcl_nfe->set_add_log_nfe( EXPORTING i_transacao = 'ZFIS55' i_type = 'E' i_id  = 'Z01' i_num = 0 i_message_v1 = CONV #( lv_message ) i_message_v2 = '' i_message_v3 = CONV #( abap_off ) i_message_v4   = CONV #( abap_off )
                                          CHANGING p_lc_sequencia = lc_seqlog ).
            REFRESH it_save.
            EXIT.
        ENDCASE.
*-CS2025000249-02.05.2025-#174115-JT-fim
      ELSEIF p_chave IS NOT INITIAL.
        SELECT  SINGLE *
         INTO wa_ekpo
         FROM ekpo
         WHERE ebeln IN p_ebeln
         AND   matnr EQ wa_saida-matnr.
        IF sy-subrc NE 0.
*-CS2025000249-02.05.2025-#174115-JT-inicio
          CASE lc_inb_aceite.
            WHEN abap_off.
              MESSAGE |Material não é do pedido linha { sy-tabix } | TYPE 'E'.
              REFRESH it_save.
              EXIT.
            WHEN abap_true.
              lc_seqlog = lc_seqlog + 1.
              MESSAGE e024(sd) WITH |Material não é do pedido linha { sy-tabix } | INTO lv_message.
              lc_zcl_nfe->set_add_log_nfe( EXPORTING i_transacao = 'ZFIS55' i_type = 'E' i_id  = 'Z01' i_num = 0 i_message_v1 = CONV #( lv_message ) i_message_v2 = '' i_message_v3 = CONV #( abap_off ) i_message_v4   = CONV #( abap_off )
                                            CHANGING p_lc_sequencia = lc_seqlog ).
              REFRESH it_save.
              EXIT.
          ENDCASE.
*-CS2025000249-02.05.2025-#174115-JT-fim
        ENDIF.
      ENDIF.
    ENDIF.

    IF wa_saida-lifnr  IS INITIAL.
*-CS2025000249-02.05.2025-#174115-JT-inicio
      CASE lc_inb_aceite.
        WHEN abap_off.
          MESSAGE |Campo fornecedor obrigatório! linha { sy-tabix } | TYPE 'E'.
          REFRESH it_save.
          EXIT.
        WHEN abap_true.
          lc_seqlog = lc_seqlog + 1.
          MESSAGE s024(sd) WITH |Campo fornecedor obrigatório! linha { sy-tabix } | INTO lv_message.
          lc_zcl_nfe->set_add_log_nfe( EXPORTING i_transacao = 'ZFIS55' i_type = 'E' i_id  = 'Z01' i_num = 0 i_message_v1 = CONV #( lv_message ) i_message_v2 = '' i_message_v3 = CONV #( abap_off ) i_message_v4   = CONV #( abap_off )
                                        CHANGING p_lc_sequencia = lc_seqlog ).
          REFRESH it_save.
          EXIT.
      ENDCASE.
*-CS2025000249-02.05.2025-#174115-JT-fim
    ELSE.
      SELECT COUNT(*)
         FROM lfa1
         WHERE lifnr = wa_saida-lifnr.
      IF sy-subrc NE 0.
*-CS2025000249-02.05.2025-#174115-JT-inicio
        CASE lc_inb_aceite.
          WHEN abap_off.
            MESSAGE |Fornecedor não existe linha  { sy-tabix } | TYPE 'E'.
            REFRESH it_save.
            EXIT.
          WHEN abap_true.
            lc_seqlog = lc_seqlog + 1.
            MESSAGE e024(sd) WITH |Fornecedor não existe linha  { sy-tabix } | INTO lv_message.
            lc_zcl_nfe->set_add_log_nfe( EXPORTING i_transacao = 'ZFIS55' i_type = 'E' i_id  = 'Z01' i_num = 0 i_message_v1 = CONV #( lv_message ) i_message_v2 = '' i_message_v3 = CONV #( abap_off ) i_message_v4   = CONV #( abap_off )
                                          CHANGING p_lc_sequencia = lc_seqlog ).
            REFRESH it_save.
            EXIT.
        ENDCASE.
*-CS2025000249-02.05.2025-#174115-JT-fim
      ENDIF.
    ENDIF.
*    IF wa_saida-rate  IS INITIAL.
*      MESSAGE |Campo Taxa obrigatório! linha { sy-tabix } | TYPE 'E'.
*      REFRESH it_save.
*      EXIT.
*    ELSEIF  wa_saida-rate > 100.
*      MESSAGE |Taxa Inválida! ! linha { sy-tabix } | TYPE 'E'.
*      REFRESH it_save.
*      EXIT.
*    ENDIF.
*    IF wa_saida-base  IS INITIAL.
*      MESSAGE |Campo Base obrigatório! linha { sy-tabix } | TYPE 'E'.
*      REFRESH it_save.
*      EXIT.
*    ELSEIF  wa_saida-base GT 100 OR wa_saida-base LE 0.
**      MESSAGE |Base Inválida! ! linha { sy-tabix } | TYPE 'E'.
**      REFRESH it_save.
**      EXIT.
*    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_saida-ebeln
      IMPORTING
        output = wa_saida-ebeln.

    MOVE-CORRESPONDING wa_saida TO wa_save.
    wa_save-chave_nfe   = v_chave.
    wa_save-usnam       = sy-uname.
    wa_save-zdt_atual   = sy-datum.
    wa_save-zhr_atual   = sy-uzeit.
    wa_save-mwskz       = wa_saida-mwskz_n.
    wa_save-indcoper    = wa_saida-indcoper.

    READ TABLE t_branch ASSIGNING FIELD-SYMBOL(<fs_branch>)
    WITH KEY branch = wa_saida-werks
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
*-------------Inicio CS2023000919  / USER STORY 128409 / AOENNING.
      SELECT SINGLE * FROM zmmt0183 INTO @DATA(wa_zmmt0183)
      WHERE bukrs EQ  @<fs_branch>-bukrs.
      IF sy-subrc EQ 0.
        IF wa_saida-ebeln IS NOT INITIAL AND wa_saida-mwskz_n IS NOT INITIAL.

          FREE: it_poitem_ped, it_poitemx_ped.
          it_poitem_ped = VALUE #( ( po_item  = wa_saida-ebelp
                                     tax_code = wa_saida-mwskz_n ) ).

          it_poitemx_ped = VALUE #( ( po_item = wa_saida-ebelp
                                     tax_code = abap_true ) ).

          FREE: it_return_ped.
          CALL FUNCTION 'BAPI_PO_CHANGE'
            EXPORTING
              purchaseorder = wa_zmmt0168-ebeln
            TABLES
              return        = it_return_ped
              poitem        = it_poitem_ped
              poitemx       = it_poitemx_ped
              popartner     = it_popartner.


          READ TABLE it_return_ped TRANSPORTING NO FIELDS WITH KEY type = 'E'.
          IF sy-subrc IS INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = abap_true.
          ENDIF.

*          if wa_saida-mwskz_n <> wa_saida-mwskz_p.
*            wa_saida-mwskz_p = wa_saida-mwskz_n.
*            update ekpo set mwskz = wa_saida-mwskz_n where ebeln = wa_saida-ebeln and
*                                                           ebelp = wa_saida-ebelp.
*            if sy-subrc is initial.
*              commit work.
*            endif.
*          endif.
        ENDIF.
      ENDIF.
    ENDIF.
*-------------Fim CS2023000919  / USER STORY 128409 / AOENNING.



    IF wa_saida-ncm_xml IS NOT INITIAL.
      wa_save-steuc       = wa_saida-ncm_xml.
    ELSE.
      IF wa_saida-ncm_mat_sap IS NOT INITIAL.
        wa_save-steuc     = wa_saida-ncm_mat_sap.
      ENDIF.
    ENDIF.

    IF wa_save-steuc IS INITIAL.
      MESSAGE e024(sd) WITH 'Informar NCM para o material:' wa_saida-matnr.
      EXIT.
    ENDIF.


    APPEND wa_save TO it_save.
    CLEAR: wa_save, wa_saida.

  ENDLOOP.

  IF it_save IS NOT INITIAL.
    DELETE FROM zmmt0168 WHERE chave_nfe = v_chave.
    MODIFY zmmt0168 FROM TABLE it_save.
    COMMIT WORK.
    MESSAGE 'Dados gravado com sucesso!' TYPE 'S'.
  ENDIF.

  REFRESH it_save.
ENDFORM.

*-CS2025000249-02.05.2025-#174115-JT-inicio
**********************************************************************
* preparacao selecao dados ZMM0110 - INBOUND - ACEITE
**********************************************************************
FORM f_executar_zfis55 USING p_chave_nfe
                             p_ebeln.

  DATA: rr_ebeln TYPE RANGE OF ebeln.

  FREE: it_zmmt0168.

  SELECT SINGLE chave_nfe, f_tomadora
    INTO @DATA(_zib_nfe_dist_ter)
    FROM zib_nfe_dist_ter
   WHERE chave_nfe = @p_chave_nfe.

  CHECK sy-subrc = 0.

  lc_inb_aceite = abap_true.

  p_usnam       = sy-uname.
  p_werks       = _zib_nfe_dist_ter-f_tomadora.
  p_chave       = _zib_nfe_dist_ter-chave_nfe.
  rr_ebeln      = VALUE #( ( sign = 'I' option = 'EQ'  low = p_ebeln ) ).
  s_ebeln[]     = rr_ebeln[].

  TRY.
      CREATE OBJECT lc_zcl_nfe
        EXPORTING
          i_chave_nfe    = p_chave_nfe
          i_sem_bloqueio = abap_true.
    CATCH zcx_nfe_inbound_exception.
    CATCH zcx_cadastro.
  ENDTRY.

  lc_seqlog = lc_zcl_nfe->get_sequencia_log( ).

  PERFORM zf_seleciona_dados.

  IF it_saida[] IS INITIAL.
    lc_zcl_nfe->nfe_inbound_gravar_log( ).
    RETURN.
  ENDIF.

  PERFORM zf_montar_dados_material.
  PERFORM zf_save.

  SELECT *
    FROM zmmt0168
    INTO TABLE it_zmmt0168
   WHERE chave_nfe EQ v_chave
     AND steuc     NE space.

  IF it_zmmt0168[] IS INITIAL.
    lc_zcl_nfe->nfe_inbound_gravar_log( ).
    RETURN.
  ENDIF.

  PERFORM zj1btax.

  lc_zcl_nfe->nfe_inbound_gravar_log( ).

ENDFORM.

**********************************************************************
* montar itens - material
**********************************************************************
FORM zf_montar_dados_material.

  DATA: lv_tabix TYPE sy-tabix.

  SELECT *
    INTO TABLE @DATA(t_zib_nfe_dist_itm)
    FROM zib_nfe_dist_itm
   WHERE chave_nfe = @p_chave.

  LOOP AT it_saida INTO wa_saida.
    lv_tabix = sy-tabix.

    READ TABLE t_zib_nfe_dist_itm INTO DATA(w_zib_nfe_dist_itm) WITH KEY chave_nfe  = wa_saida-chave_nfe
                                                                         prod_item  = wa_saida-prod_item.
    CHECK sy-subrc = 0.

    PERFORM zf_atualizar_info_material USING w_zib_nfe_dist_itm.

    MODIFY it_saida FROM wa_saida INDEX lv_tabix.
  ENDLOOP.

ENDFORM.

**********************************************************************
* montar itens - material
**********************************************************************
FORM zf_atualizar_info_material USING p_zib_nfe_dist_itm  TYPE zib_nfe_dist_itm.

  DATA: lv_value   TYPE lvc_value,
        w_makt     TYPE makt,
        w_steuc    TYPE steuc,
        w_ekpo     TYPE ekpo,
        w_zmmt0169 TYPE zmmt0169,
        v_bukrs    TYPE j_1bbranch-bukrs,
        v_comple   TYPE c,
        v_branch   TYPE zmmt0169-branch,
        v_regio    TYPE t001w-regio,
        v_indcoper TYPE zmmt0169-indcoper,
        v_regiof   TYPE lfa1-regio.

  lv_value          =  |{ p_zib_nfe_dist_itm-prod_ncm+0(4) }.{ p_zib_nfe_dist_itm-prod_ncm+4(2) }.{ p_zib_nfe_dist_itm-prod_ncm+6(2) }|.

  wa_saida-matnr    = p_zib_nfe_dist_itm-matnr.
  wa_saida-icms_cst = p_zib_nfe_dist_itm-icms_cst.
  wa_saida-ncm_xml  = lv_value.

  SELECT SINGLE *
    FROM makt
    INTO w_makt
   WHERE spras = sy-langu
     AND matnr = p_zib_nfe_dist_itm-matnr.

  wa_saida-desc_mat = w_makt-maktx.

  SELECT SINGLE steuc
    FROM marc
    INTO w_steuc
   WHERE werks = wa_saida-werks
     AND matnr = p_zib_nfe_dist_itm-matnr.

  wa_saida-ncm_mat_sap = w_steuc.

  IF p_ebeln[] IS NOT INITIAL.
    SELECT SINGLE *
      INTO w_ekpo
      FROM ekpo
     WHERE ebeln IN p_ebeln
       AND matnr  = p_zib_nfe_dist_itm-matnr.

    wa_saida-ebeln   = w_ekpo-ebeln.
    wa_saida-ebelp   = w_ekpo-ebelp.
    wa_saida-mwskz_p = w_ekpo-mwskz.
  ENDIF.

  SELECT SINGLE bukrs
    INTO v_bukrs
    FROM j_1bbranch
   WHERE branch = wa_saida-werks.

  SELECT SINGLE regio
    INTO v_regio
    FROM t001w
   WHERE werks = wa_saida-werks.

  SELECT SINGLE regio
    INTO v_regiof
    FROM lfa1
   WHERE lifnr = wa_saida-lifnr.

  IF v_regio NE v_regiof.
    v_indcoper  = 'F'.
  ELSE.
    v_indcoper  = 'D'.
  ENDIF.

  SELECT SINGLE *
    INTO w_zmmt0169
    FROM zmmt0169
   WHERE bukrs    = v_bukrs                          "OBRIGATORIO
     AND branch   = wa_saida-werks                   "OPCIONAL
     AND regio    = v_regio                          "OPCIONAL
     AND icms_cst = p_zib_nfe_dist_itm-icms_cst     "OBRIGATORIO
     AND indcoper = v_indcoper                       "OBRIGATORIO
     AND mwskz_1  = wa_saida-mwskz_p+0(1).

  IF sy-subrc NE 0.
    SELECT SINGLE *
      INTO w_zmmt0169
      FROM zmmt0169
     WHERE bukrs    = v_bukrs                          "OBRIGATORIO
       AND branch   = wa_saida-werks                   "OPCIONAL
       AND regio    = ' '                              "OPCIONAL
       AND icms_cst = p_zib_nfe_dist_itm-icms_cst     "OBRIGATORIO
       AND indcoper = v_indcoper                       "OBRIGATORIO
       AND mwskz_1  = wa_saida-mwskz_p+0(1).

    IF sy-subrc NE 0.
      SELECT SINGLE *
        INTO w_zmmt0169
        FROM zmmt0169
       WHERE bukrs    = v_bukrs                          "OBRIGATORIO
         AND branch   = v_branch                         "OPCIONAL
         AND regio    = v_regio                          "OPCIONAL
         AND icms_cst = p_zib_nfe_dist_itm-icms_cst     "OBRIGATORIO
         AND indcoper = v_indcoper                       "OBRIGATORIO
         AND mwskz_1  = wa_saida-mwskz_p+0(1).

      IF sy-subrc NE 0.
        SELECT SINGLE *
          INTO w_zmmt0169
          FROM zmmt0169
         WHERE bukrs    = v_bukrs                          "OBRIGATORIO
           AND branch   = v_branch                         "OPCIONAL
           AND regio    = ' '                              "OPCIONAL
           AND icms_cst = p_zib_nfe_dist_itm-icms_cst     "OBRIGATORIO
           AND indcoper = v_indcoper                       "OBRIGATORIO
           AND mwskz_1  = wa_saida-mwskz_p+0(1).
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR: wa_saida-indcoper, wa_saida-mwskz_n.

  IF sy-subrc = 0.
    IF w_zmmt0169-mwskz+0(1) = wa_saida-mwskz_p+0(1).
      wa_saida-indcoper = w_zmmt0169-indcoper.
      wa_saida-mwskz_n  = w_zmmt0169-mwskz.
    ENDIF.
  ENDIF.

  IF wa_saida-shipfrom   IS NOT INITIAL AND
     wa_saida-shipto     IS NOT INITIAL AND
     wa_saida-werks      IS NOT INITIAL AND
     wa_saida-matnr      IS NOT INITIAL AND
     wa_saida-lifnr      IS NOT INITIAL.
    SELECT SINGLE *
      INTO wa_j_1btxic3
      FROM j_1btxic3
     WHERE land1     = 'BR'
       AND shipfrom  = wa_saida-shipfrom
       AND shipto    = wa_saida-shipto
       AND gruop     = '31'
       AND value     = wa_saida-werks
       AND value2    = wa_saida-matnr
       AND value3    = wa_saida-lifnr.

    IF sy-subrc = 0.
      v_comple = abap_true.
      wa_saida-ratej = wa_j_1btxic3-rate.
      wa_saida-basej = wa_j_1btxic3-base.
      wa_saida-icon  = icon_complete.
    ELSE.
      v_comple = abap_false.
      CLEAR: wa_saida-ratej, wa_saida-basej.
    ENDIF.
  ENDIF.

  IF v_comple <> abap_true.
    IF wa_saida-shipfrom   IS NOT INITIAL AND
       wa_saida-shipto     IS NOT INITIAL AND
       wa_saida-werks      IS NOT INITIAL AND
       wa_saida-matnr      IS NOT INITIAL AND
       wa_saida-lifnr      IS NOT INITIAL AND
       wa_saida-rate       IS NOT INITIAL AND
       wa_saida-base       IS NOT INITIAL.
      wa_saida-icon  = icon_checked.
    ELSE.
      wa_saida-icon  = icon_warning.
    ENDIF.
  ENDIF.

ENDFORM.
*-CS2025000249-02.05.2025-#174115-JT-fim

**********************************************************************
**********************************************************************

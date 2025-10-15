*&---------------------------------------------------------------------*
*& Report  ZMASTER_DATA_FIX_XK_XD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmaster_data_fix_xk_xd.

TABLES: lfa1, kna1.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_nriv_custom,
         ktokk TYPE t077k-ktokk,
         ktokd TYPE t077d-ktokd,
         numkr TYPE t077k-numkr,
         fausa TYPE t077k-fausa,
         fausf TYPE t077k-fausf,
         fausm TYPE t077k-fausm,
         faus1 TYPE t077k-faus1,
         fausg TYPE t077k-fausg,
         fausn TYPE t077k-fausn,
         fausx TYPE t077k-fausx,
         fausu TYPE t077k-fausu,
         faus2 TYPE t077k-faus2,
         faus3 TYPE t077k-faus3.
         INCLUDE STRUCTURE nriv.
       TYPES: END OF ty_nriv_custom.

TYPES: BEGIN OF ty_lfa1_custom,
         lifnr_nriv TYPE nriv-fromnumber.
         INCLUDE STRUCTURE lfa1.
       TYPES: END OF ty_lfa1_custom.

TYPES: BEGIN OF ty_kna1_custom,
         kunnr_nriv TYPE nriv-fromnumber.
         INCLUDE STRUCTURE kna1.
       TYPES: END OF ty_kna1_custom.

TYPES: BEGIN OF ty_commom_xk_xd,
         alc        TYPE lfa1-alc,
         "anred      TYPE lfa1-anred,
         bahns      TYPE lfa1-bahns,
         bbbnr      TYPE lfa1-bbbnr,
         bbsnr      TYPE lfa1-bbsnr,
         begru      TYPE lfa1-begru,
         brsch      TYPE lfa1-brsch,
         bubkz      TYPE lfa1-bubkz,
         cnae       TYPE lfa1-cnae,
         comsize    TYPE lfa1-comsize,
         confs      TYPE lfa1-confs,
         crtn       TYPE lfa1-crtn,
         cvp_xblck  TYPE lfa1-cvp_xblck,
         datlt      TYPE lfa1-datlt,
         decregpc   TYPE lfa1-decregpc,
         dtams      TYPE lfa1-dtams,
         dtaws      TYPE lfa1-dtaws,
         duefl      TYPE lfa1-duefl,
         exp        TYPE lfa1-exp,
         fiskn      TYPE lfa1-fiskn,
         fityp      TYPE lfa1-fityp,
         icmstaxpay TYPE lfa1-icmstaxpay,
         indtyp     TYPE lfa1-indtyp,
         j_1kfrepre TYPE lfa1-j_1kfrepre,
         j_1kftbus  TYPE lfa1-j_1kftbus,
         j_1kftind  TYPE lfa1-j_1kftind,
         konzs      TYPE lfa1-konzs,
         land1      TYPE lfa1-land1,
         legalnat   TYPE lfa1-legalnat,
         lzone      TYPE lfa1-lzone,
         "mcod1      TYPE lfa1-mcod1,
         "mcod2      TYPE lfa1-mcod2,
         "mcod3      TYPE lfa1-mcod3,
         name1      TYPE lfa1-name1,
         name2      TYPE lfa1-name2,
         name3      TYPE lfa1-name3,
         name4      TYPE lfa1-name4,
         ort01      TYPE lfa1-ort01,
         ort02      TYPE lfa1-ort02,
         "pfach      TYPE lfa1-pfach,
         pfort      TYPE lfa1-pfort,
         pmt_office TYPE lfa1-pmt_office,
         psofg      TYPE lfa1-psofg,
         psohs      TYPE lfa1-psohs,
         psois      TYPE lfa1-psois,
         pson1      TYPE lfa1-pson1,
         pson2      TYPE lfa1-pson2,
         pson3      TYPE lfa1-pson3,
         psost      TYPE lfa1-psost,
         psotl      TYPE lfa1-psotl,
         psovn      TYPE lfa1-psovn,
         pstl2      TYPE lfa1-pstl2,
         pstlz      TYPE lfa1-pstlz,
         regio      TYPE lfa1-regio,
         rg         TYPE lfa1-rg,
         rgdate     TYPE lfa1-rgdate,
         ric        TYPE lfa1-ric,
         rne        TYPE lfa1-rne,
         rnedate    TYPE lfa1-rnedate,
         sortl      TYPE lfa1-sortl,
         stcd1      TYPE lfa1-stcd1,
         stcd2      TYPE lfa1-stcd2,
         stcd3      TYPE lfa1-stcd3,
         stcd4      TYPE lfa1-stcd4,
         stcd5      TYPE lfa1-stcd5,
         stcdt      TYPE lfa1-stcdt,
         stceg      TYPE lfa1-stceg,
         stkza      TYPE lfa1-stkza,
         stkzn      TYPE lfa1-stkzn,
         stkzu      TYPE lfa1-stkzu,
         "stras      TYPE lfa1-stras, "Comparar street da adrc
         tdt        TYPE lfa1-tdt,
         telbx      TYPE lfa1-telbx,
         telf1      TYPE lfa1-telf1,
         telf2      TYPE lfa1-telf2,
         telfx      TYPE lfa1-telfx,
         teltx      TYPE lfa1-teltx,
         "telx1      TYPE lfa1-telx1,
         txjcd      TYPE lfa1-txjcd,
         uf         TYPE lfa1-uf,
         vbund      TYPE lfa1-vbund,
         werks      TYPE lfa1-werks,
         xcpdk      TYPE lfa1-xcpdk,
         xzemp      TYPE lfa1-xzemp,
       END OF ty_commom_xk_xd.


TYPES: BEGIN OF ty_commom_xk_xd_adrc,
         city1            TYPE adrc-city1,
         city2            TYPE adrc-city2,
         cityh_code       TYPE adrc-cityh_code,
         cityp_code       TYPE adrc-cityp_code,
         city_code        TYPE adrc-city_code,
         city_code2       TYPE adrc-city_code2,
         country          TYPE adrc-country,
         county           TYPE adrc-county,
         county_code      TYPE adrc-county_code,
         deflt_comm       TYPE adrc-deflt_comm,
         deli_serv_number TYPE adrc-deli_serv_number,
         deli_serv_type   TYPE adrc-deli_serv_type,
         dont_use_p       TYPE adrc-dont_use_p,
         dont_use_s       TYPE adrc-dont_use_s,
         extension1       TYPE adrc-extension1,
         extension2       TYPE adrc-extension2,
         fax_extens       TYPE adrc-fax_extens,
         fax_number       TYPE adrc-fax_number,
         floor            TYPE adrc-floor,
         home_city        TYPE adrc-home_city,
         house_num1       TYPE adrc-house_num1,
         house_num2       TYPE adrc-house_num2,
         house_num3       TYPE adrc-house_num3,
         id_category      TYPE adrc-id_category,
         location         TYPE adrc-location,
         mc_city1         TYPE adrc-mc_city1,
         mc_county        TYPE adrc-mc_county,
         "mc_name1         TYPE adrc-mc_name1,
         mc_street        TYPE adrc-mc_street,
         mc_township      TYPE adrc-mc_township,
         "name1            TYPE adrc-name1,
         "name2            TYPE adrc-name2,
         "name3            TYPE adrc-name3,
         "name4            TYPE adrc-name4,
         name_co          TYPE adrc-name_co,
         nation           TYPE adrc-nation,
         pcode1_ext       TYPE adrc-pcode1_ext,
         pcode2_ext       TYPE adrc-pcode2_ext,
         pcode3_ext       TYPE adrc-pcode3_ext,
         pers_addr        TYPE adrc-pers_addr,
         postalarea       TYPE adrc-postalarea,
         post_code1       TYPE adrc-post_code1,
         post_code2       TYPE adrc-post_code2,
         post_code3       TYPE adrc-post_code3,
         "po_box           TYPE adrc-po_box,
         po_box_cty       TYPE adrc-po_box_cty,
         po_box_lobby     TYPE adrc-po_box_lobby,
         po_box_loc       TYPE adrc-po_box_loc,
         po_box_num       TYPE adrc-po_box_num,
         po_box_reg       TYPE adrc-po_box_reg,
         regiogroup       TYPE adrc-regiogroup,
         region           TYPE adrc-region,
         roomnumber       TYPE adrc-roomnumber,
         "sort1            TYPE adrc-sort1,
         "sort2            TYPE adrc-sort2,
         sort_phn         TYPE adrc-sort_phn,
         street           TYPE adrc-street,
         streetabbr       TYPE adrc-streetabbr,
         streetcode       TYPE adrc-streetcode,
         str_suppl1       TYPE adrc-str_suppl1,
         str_suppl2       TYPE adrc-str_suppl2,
         str_suppl3       TYPE adrc-str_suppl3,
         taxjurcode       TYPE adrc-taxjurcode,
         tel_extens       TYPE adrc-tel_extens,
         tel_number       TYPE adrc-tel_number,
         time_zone        TYPE adrc-time_zone,
         title            TYPE adrc-title,
         township         TYPE adrc-township,
         township_code    TYPE adrc-township_code,
         transpzone       TYPE adrc-transpzone,
         uuid_belated     TYPE adrc-uuid_belated,
         xpcpt            TYPE adrc-xpcpt,
       END OF ty_commom_xk_xd_adrc.

TYPES: BEGIN OF ty_saida_0100_01,
         lifnr            TYPE lfa1-lifnr,
         kunnr            TYPE lfa1-kunnr,

         loevm_xk         TYPE lfa1-loevm,
         loevm_xd         TYPE lfa1-loevm,
         loevm_dif        TYPE c,

         name1_xk         TYPE lfa1-name1,
         name1_xd         TYPE lfa1-name1,
         name1_dif        TYPE c,

         stcd1_xk         TYPE lfa1-stcd1,
         stcd1_xd         TYPE lfa1-stcd1,
         stcd1_dif        TYPE c,

         stcd2_xk         TYPE lfa1-stcd2,
         stcd2_xd         TYPE lfa1-stcd2,
         stcd2_dif        TYPE c,

         stcd3_xk         TYPE lfa1-stcd3,
         stcd3_xd         TYPE lfa1-stcd3,
         stcd3_dif        TYPE c,

         stcd4_xk         TYPE lfa1-stcd4,
         stcd4_xd         TYPE lfa1-stcd4,
         stcd4_dif        TYPE c,

         regio_xk         TYPE lfa1-regio,
         regio_xd         TYPE lfa1-regio,
         regio_dif        TYPE c,

         land1_xk         TYPE lfa1-land1,
         land1_xd         TYPE lfa1-land1,
         land1_dif        TYPE c,

         ort01_xk         TYPE lfa1-ort01,
         ort01_xd         TYPE lfa1-ort01,
         ort01_dif        TYPE c,

         stras_xk         TYPE lfa1-stras,
         stras_xd         TYPE lfa1-stras,
         stras_dif        TYPE c,

         ktokk_xk         TYPE lfa1-ktokk,
         ktokd_xd         TYPE kna1-ktokd,

         others_dif       TYPE char200,

         contas_banco_dif TYPE c.

TYPES: END OF ty_saida_0100_01.


TYPES: BEGIN OF ty_saida_0100_02,
         lifnr        TYPE lfa1-lifnr,
         kunnr        TYPE lfa1-kunnr,

         name1_xk     TYPE lfa1-name1,
         name1_xd     TYPE lfa1-name1,

         stcd1_xk     TYPE lfa1-stcd1,
         stcd1_xd     TYPE lfa1-stcd1,

         stcd2_xk     TYPE lfa1-stcd2,
         stcd2_xd     TYPE lfa1-stcd2,

         stcd3_xk     TYPE lfa1-stcd3,
         stcd3_xd     TYPE lfa1-stcd3,

         stcd4_xk     TYPE lfa1-stcd4,
         stcd4_xd     TYPE lfa1-stcd4,

         regio_xk     TYPE lfa1-regio,
         regio_xd     TYPE lfa1-regio,

         land1_xk     TYPE lfa1-land1,
         land1_xd     TYPE lfa1-land1,

         ort01_xk     TYPE lfa1-ort01,
         ort01_xd     TYPE lfa1-ort01,

         stras_xk     TYPE lfa1-stras,
         stras_xd     TYPE lfa1-stras,

         ktokk_xk     TYPE lfa1-ktokk,
         ktokd_xd     TYPE kna1-ktokd,

         qtde_xd_vinc TYPE i.

TYPES: END OF ty_saida_0100_02.


TYPES: BEGIN OF ty_saida_0100_03,
         lifnr      TYPE lfa1-lifnr,
         kunnr      TYPE kna1-kunnr,
         name1      TYPE lfa1-name1,
         ktokk      TYPE lfa1-ktokk,
         ktokd      TYPE kna1-ktokd,
         fromnumber TYPE nriv-fromnumber,
         tonumber   TYPE nriv-tonumber,
         ktokk_dest TYPE lfa1-ktokk,
         ktokd_dest TYPE kna1-ktokd,
         bkp_done   TYPE c,
         restore    TYPE c,
         layout_dif TYPE c.
TYPES: END OF ty_saida_0100_03.

CONSTANTS: c_new     TYPE char50 VALUE 'NEW',
           c_current TYPE char50 VALUE 'CURRENT'.

* ALV field catalogs
DATA: it_fcat TYPE lvc_t_fcat,
      wa_fcat TYPE lvc_s_fcat.

* ALV excluded functions
DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

* Alv Styles
DATA: ls_edit TYPE lvc_s_styl,
      lt_edit TYPE lvc_t_styl.

* ALV layout variant
DATA: gs_variant       TYPE disvariant.

* ALV layout
DATA: gs_layout        TYPE lvc_s_layo.

* ALV Stable
DATA: wa_stable        TYPE lvc_s_stbl.

DATA: it_selectedcell TYPE lvc_t_cell,
      wa_selectedcell TYPE lvc_s_cell.

DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA: gt_estilo TYPE lvc_t_styl WITH HEADER LINE,
      wl_estilo TYPE lvc_s_styl.

DATA: gt_f4  TYPE lvc_t_f4 WITH HEADER LINE.

* Objetos
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

DATA: wa_estrutura TYPE ty_estrutura,
      estrutura    TYPE TABLE OF ty_estrutura.

* Gerenciador Arquivos
DATA: manager TYPE REF TO cl_gos_manager,
      obj     TYPE borident,
      ip_mode TYPE sgs_rwmod,
      objtype TYPE borident-objtype VALUE 'ZGL059'.


CLASS lcl_alv_toolbar_0100 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.


DATA: obj_alv_0100       TYPE REF TO cl_gui_alv_grid,
      obj_container_0100 TYPE REF TO cl_gui_custom_container,
      obj_toolbar_0100   TYPE REF TO lcl_alv_toolbar_0100.

DATA: var_answer.

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: it_saida_0100_01 TYPE TABLE OF ty_saida_0100_01,
      wa_saida_0100_01 TYPE ty_saida_0100_01,

      it_saida_0100_02 TYPE TABLE OF ty_saida_0100_02,
      wa_saida_0100_02 TYPE ty_saida_0100_02,

      it_saida_0100_03 TYPE TABLE OF ty_saida_0100_03,
      wa_saida_0100_03 TYPE ty_saida_0100_03.

DATA: git_lfa1 TYPE TABLE OF lfa1,
      git_kna1 TYPE TABLE OF kna1,
      git_lfbk TYPE TABLE OF lfbk,
      git_knbk TYPE TABLE OF knbk,
      git_adrc TYPE TABLE OF adrc,
      git_adr6 TYPE TABLE OF adr6.


SELECTION-SCREEN: BEGIN OF BLOCK b0 WITH FRAME TITLE text-001.
PARAMETERS    : p_xk   RADIOBUTTON GROUP g1 USER-COMMAND usr1 DEFAULT 'X',
                p_xd   RADIOBUTTON GROUP g1,
                p_xkxd RADIOBUTTON GROUP g1.
SELECTION-SCREEN: END   OF BLOCK b0.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS    : p_fdomi  RADIOBUTTON  GROUP g2 USER-COMMAND usr2 DEFAULT 'X',
                p_fftra  RADIOBUTTON  GROUP g2,
                p_eqxdxk RADIOBUTTON  GROUP g2,
                p_vcxdxk RADIOBUTTON  GROUP g2,
                p_movgc  RADIOBUTTON  GROUP g2,
                p_eqid4  RADIOBUTTON  GROUP g2,
                p_filind RADIOBUTTON  GROUP g2,
                p_flzone RADIOBUTTON  GROUP g2.

PARAMETERS  p_restgc AS CHECKBOX.
SELECTION-SCREEN: END   OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
SELECT-OPTIONS: p_lifnr  FOR lfa1-lifnr  MODIF ID t2 NO INTERVALS, " OBLIGATORY,
                p_kunnr  FOR kna1-kunnr  MODIF ID t2 NO INTERVALS. " OBLIGATORY,

PARAMETERS : p_delxk AS CHECKBOX.
PARAMETERS : p_delxd AS CHECKBOX.

SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE text-004.
SELECT-OPTIONS : p_txjcd FOR lfa1-txjcd NO INTERVALS NO-EXTENSION.
SELECT-OPTIONS : p_pstlz FOR lfa1-pstlz NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN: END   OF BLOCK b4.

SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE text-005.
PARAMETERS : p_maxreg TYPE i DEFAULT 1000.
SELECTION-SCREEN: END   OF BLOCK b5.



START-OF-SELECTION.

  PERFORM f_processar.


FORM f_xk_process .

  IF p_movgc IS INITIAL.
    IF ( p_lifnr[] IS INITIAL ).
      MESSAGE 'Nenhum fornecedor informado!' TYPE 'I'.
      EXIT.
    ENDIF.

    SELECT *
      FROM lfa1 INTO TABLE git_lfa1
     WHERE lifnr IN p_lifnr.

    IF ( git_lfa1[] IS INITIAL ).
      MESSAGE 'Nenhum fornecedor encontrado!' TYPE 'I'.
      EXIT.
    ENDIF.

    SELECT *
      FROM adrc INTO TABLE git_adrc
      FOR ALL ENTRIES IN git_lfa1
    WHERE addrnumber EQ git_lfa1-adrnr.

    IF ( git_adrc[] IS INITIAL ).
      MESSAGE 'Nenhum fornecedor encontrado!' TYPE 'I'.
      EXIT.
    ENDIF.
  ENDIF.

  CASE abap_true.
    WHEN p_fdomi.
      MESSAGE 'Opção não permitida!' TYPE 'I'.
      EXIT.

      PERFORM f_xk_fix_domicilio.
    WHEN p_movgc.
      PERFORM f_xk_bkp_ktokk.
    WHEN OTHERS.
      EXIT.
  ENDCASE.


ENDFORM.

FORM f_xd_process .

  IF p_movgc IS INITIAL.
    IF  ( p_kunnr[] IS NOT INITIAL ).
      MESSAGE 'Nenhum cliente informado!' TYPE 'I'.
      EXIT.
    ENDIF.
  ENDIF.

  CASE abap_true.
    WHEN p_fdomi.
    WHEN p_movgc.
      PERFORM f_xd_bkp_ktokd.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.

FORM f_xk_fix_domicilio.

  DELETE git_lfa1 WHERE loevm IS INITIAL. "Só corrigir os fornecedores marcados para eliminação.

  CHECK git_lfa1[] IS NOT INITIAL.

  LOOP AT git_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1_change>).

    <fs_lfa1_change>-pstlz = p_pstlz-low.
    <fs_lfa1_change>-txjcd = p_txjcd-low.
    <fs_lfa1_change>-regio = p_txjcd-low(2).

    MODIFY lfa1 FROM <fs_lfa1_change>.

    LOOP AT git_adrc ASSIGNING FIELD-SYMBOL(<fs_adrc_change>) WHERE addrnumber = <fs_lfa1_change>-adrnr.
      <fs_adrc_change>-post_code1 = p_pstlz-low.
      <fs_adrc_change>-region     = p_txjcd-low(2).
      <fs_adrc_change>-taxjurcode = p_txjcd-low.
      MODIFY adrc FROM <fs_adrc_change>.
    ENDLOOP.

  ENDLOOP.

ENDFORM.


MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  IF obj_container_0100 IS INITIAL.
    SET TITLEBAR 'T0100'.
  ENDIF.
ENDMODULE.

MODULE criar_objetos_0100 OUTPUT.

  IF obj_container_0100 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0100'.

    CREATE OBJECT obj_container_0100
      EXPORTING
        container_name = 'CC_ALV_0100'.

    CREATE OBJECT obj_alv_0100
      EXPORTING
        i_parent = obj_container_0100.

    CREATE OBJECT obj_toolbar_0100
      EXPORTING
        io_alv_grid = obj_alv_0100.

    gs_layout-sel_mode   = 'A'.
    gs_variant-report  = sy-repid.
    wa_stable-row         = 'X'.
    wa_stable-col         = 'X'.

    SET HANDLER: obj_toolbar_0100->on_toolbar          FOR obj_alv_0100,
                 obj_toolbar_0100->handle_user_command FOR obj_alv_0100.

    PERFORM f_exclude_fcode USING '0100'.

    CASE abap_true.
      WHEN p_eqxdxk.
        CALL METHOD obj_alv_0100->set_table_for_first_display
          EXPORTING
            is_layout            = gs_layout
            i_save               = 'A'
            it_toolbar_excluding = it_exclude_fcode
            is_variant           = gs_variant
          CHANGING
            it_fieldcatalog      = it_fcat
            it_outtab            = it_saida_0100_01.
      WHEN p_vcxdxk.
        CALL METHOD obj_alv_0100->set_table_for_first_display
          EXPORTING
            is_layout            = gs_layout
            i_save               = 'A'
            it_toolbar_excluding = it_exclude_fcode
            is_variant           = gs_variant
          CHANGING
            it_fieldcatalog      = it_fcat
            it_outtab            = it_saida_0100_02.
      WHEN p_movgc.
        CALL METHOD obj_alv_0100->set_table_for_first_display
          EXPORTING
            is_layout            = gs_layout
            i_save               = 'A'
            it_toolbar_excluding = it_exclude_fcode
            is_variant           = gs_variant
          CHANGING
            it_fieldcatalog      = it_fcat
            it_outtab            = it_saida_0100_03.
    ENDCASE.



    CALL METHOD obj_alv_0100->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_alv_0100->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD obj_alv_0100->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.



ENDMODULE.



CLASS lcl_alv_toolbar_0100 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.





    CASE abap_true.
      WHEN p_xk.
        CASE abap_true.
          WHEN p_movgc.

            IF p_restgc IS NOT INITIAL.

              CLEAR ty_toolbar.
              ty_toolbar-icon      = icon_unpack.
              ty_toolbar-function  = 'RESTORE_GRUPO_CONTA_XK'.
              ty_toolbar-text      = 'Restore Grupo Conta'.
              ty_toolbar-butn_type = 0.
              APPEND ty_toolbar TO e_object->mt_toolbar.

            ELSE.

              CLEAR ty_toolbar.
              ty_toolbar-icon      = icon_packing.
              ty_toolbar-function  = 'BKP_GRUPO_CONTA_XK'.
              ty_toolbar-text      = 'Backup Grupo Conta'.
              ty_toolbar-butn_type = 0.
              APPEND ty_toolbar TO e_object->mt_toolbar.

              CLEAR ty_toolbar.
              ty_toolbar-icon      = icon_generate.
              ty_toolbar-function  = 'TRANSF_GRUPO_CONTA_XK'.
              ty_toolbar-text      = 'Transferir Grupo Conta'.
              ty_toolbar-butn_type = 0.
              APPEND ty_toolbar TO e_object->mt_toolbar.

            ENDIF.


          WHEN OTHERS.
        ENDCASE.
      WHEN p_xd.
        CASE abap_true.
          WHEN p_movgc.

            IF p_restgc IS NOT INITIAL.

              CLEAR ty_toolbar.
              ty_toolbar-icon      = icon_unpack.
              ty_toolbar-function  = 'RESTORE_GRUPO_CONTA_XD'.
              ty_toolbar-text      = 'Restore Grupo Conta'.
              ty_toolbar-butn_type = 0.
              APPEND ty_toolbar TO e_object->mt_toolbar.

            ELSE.

              CLEAR ty_toolbar.
              ty_toolbar-icon      = icon_packing.
              ty_toolbar-function  = 'BKP_GRUPO_CONTA_XD'.
              ty_toolbar-text      = 'Backup Grupo Conta'.
              ty_toolbar-butn_type = 0.
              APPEND ty_toolbar TO e_object->mt_toolbar.

              CLEAR ty_toolbar.
              ty_toolbar-icon      = icon_generate.
              ty_toolbar-function  = 'TRANSF_GRUPO_CONTA_XD'.
              ty_toolbar-text      = 'Transferir Grupo Conta'.
              ty_toolbar-butn_type = 0.
              APPEND ty_toolbar TO e_object->mt_toolbar.

            ENDIF.

          WHEN OTHERS.
        ENDCASE.

      WHEN p_xkxd.
        CASE abap_true.
          WHEN p_eqxdxk.

            CLEAR ty_toolbar.
            ty_toolbar-icon      = icon_generate.
            ty_toolbar-function  = 'EQUALIZE_XK_XD'.
            ty_toolbar-text      = 'Equalizar Cadastros(Base XK)'.
            ty_toolbar-butn_type = 0.
            APPEND ty_toolbar TO e_object->mt_toolbar.
          WHEN p_vcxdxk.

            CLEAR ty_toolbar.
            ty_toolbar-icon      = icon_relation.
            ty_toolbar-function  = 'LINK_XK_XD'.
            ty_toolbar-text      = 'Vincular Cadastros'.
            ty_toolbar-butn_type = 0.
            APPEND ty_toolbar TO e_object->mt_toolbar.

          WHEN OTHERS.

        ENDCASE.
    ENDCASE.


    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_refresh.
    ty_toolbar-function  = 'REFRESH'.
    ty_toolbar-text      = 'Atualizar'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.


    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'EQUALIZE_XK_XD'.
        PERFORM f_exec_equalize_xk_xd.
      WHEN 'LINK_XK_XD'.
        PERFORM f_exec_vinc_xk_xd.

      WHEN 'BKP_GRUPO_CONTA_XK'.
        PERFORM f_exec_bkp_ktokk_xk.
      WHEN 'TRANSF_GRUPO_CONTA_XK'.
        PERFORM f_exec_transf_ktokk_xk.
      WHEN 'RESTORE_GRUPO_CONTA_XK'.
        PERFORM f_exec_restore_ktokk_xk.

      WHEN 'BKP_GRUPO_CONTA_XD'.
        PERFORM f_exec_bkp_ktokd_xd.
      WHEN 'TRANSF_GRUPO_CONTA_XD'.
        PERFORM f_exec_transf_ktokd_xd.
      WHEN 'RESTORE_GRUPO_CONTA_XD'.
        PERFORM f_exec_restore_ktokd_xd.


      WHEN 'REFRESH'.
        PERFORM f_renew_consulta USING '0100'.
    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

MODULE selected_rows_0100 INPUT.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

ENDMODULE.

MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'SEA'.
      "PERFORM F_LANCA_FISCAL_DT_ATUAL.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.


FORM f_refresh_objetos .

  CLEAR: gs_layout,
         gs_variant.

  REFRESH: it_exclude_fcode.

ENDFORM.

FORM f_criar_catalog USING p_screen.

  FREE: wa_fcat, it_fcat.

  CASE p_screen.
    WHEN '0100'.


      CASE abap_true. "Tipo Processamento
        WHEN p_xkxd.  "Fornecedor e Cliente

          CASE abap_true.
            WHEN p_eqxdxk.

              PERFORM f_estrutura_alv USING:

                 01  ''  ''             'IT_SAIDA_0100_01' 'LIFNR'               'Fornecedor'            '10'    ' '    '' ' ' ' ' ' ' ' ' ' ',
                 02  ''  ''             'IT_SAIDA_0100_01' 'KUNNR'               'Cliente'               '10'    ' '    '' ' ' ' ' ' ' ' ' ' ',
                 02  ''  ''             'IT_SAIDA_0100_01' 'LOEVM_XK'            'Eliminado XK'          '13'    ' '    '' ' ' ' ' ' ' ' ' ' ',
                 02  ''  ''             'IT_SAIDA_0100_01' 'LOEVM_XD'            'Eliminado XD'          '13'    ' '    '' ' ' ' ' ' ' ' ' ' ',
                 02  ''  ''             'IT_SAIDA_0100_01' 'LOEVM_DIF'           'Dif.Eliminado'         '13'    ' '    '' ' ' ' ' ' ' ' ' 'X',
                 03  ''  ''             'IT_SAIDA_0100_01' 'NAME1_XK'            'Nome XK'               '25'    ' '    '' ' ' ' ' ' ' ' ' ' ',
                 04  ''  ''             'IT_SAIDA_0100_01' 'NAME1_XD'            'Nome XD'               '25'      ' '    '' ' ' ' ' ' ' ' ' ' ',
                 05  ''  ''             'IT_SAIDA_0100_01' 'NAME1_DIF'           'Nome Dif.'             '09'    ' '    '' ' ' ' ' ' ' ' ' 'X',
                 06  ''  ''             'IT_SAIDA_0100_01' 'STCD1_XK'            'Id.Fiscal 1 XK'        '17'      ' '    '' ' ' ' ' ' ' ' ' ' ',
                 07  ''  ''             'IT_SAIDA_0100_01' 'STCD1_XD'            'Id.Fiscal 1 XD'        '17'      ' '    '' ' ' ' ' ' ' ' ' ' ',
                 08  ''  ''             'IT_SAIDA_0100_01' 'STCD1_DIF'           'Id.Fiscal 1 Dif'       '17'    ' '    '' ' ' ' ' ' ' ' ' 'X',
                 09  ''  ''             'IT_SAIDA_0100_01' 'STCD2_XK'            'Id.Fiscal 2 XK'        '17'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 10  ''  ''             'IT_SAIDA_0100_01' 'STCD2_XD'            'Id.Fiscal 2 XD'        '17'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 11  ''  ''             'IT_SAIDA_0100_01' 'STCD2_DIF'           'Id.Fiscal 2 Dif'       '17'     ' '    '' ' ' ' ' ' ' ' ' 'X',
                 12  ''  ''             'IT_SAIDA_0100_01' 'STCD3_XK'            'Id.Fiscal 3 XK'        '17'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 13  ''  ''             'IT_SAIDA_0100_01' 'STCD3_XD'            'Id.Fiscal 3 XD'        '17'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 14  ''  ''             'IT_SAIDA_0100_01' 'STCD3_DIF'           'Id.Fiscal 3 Dif'       '17'     ' '    '' ' ' ' ' ' ' ' ' 'X',
                 15  ''  ''             'IT_SAIDA_0100_01' 'STCD4_XK'            'Id.Fiscal 4 XK'        '17'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 16  ''  ''             'IT_SAIDA_0100_01' 'STCD4_XD'            'Id.Fiscal 4 XD'        '17'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 17  ''  ''             'IT_SAIDA_0100_01' 'STCD4_DIF'           'Id.Fiscal 4 Dif'       '17'     ' '    '' ' ' ' ' ' ' ' ' 'X',
                 18  ''  ''             'IT_SAIDA_0100_01' 'REGIO_XK'            'UF XK'                 '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 19  ''  ''             'IT_SAIDA_0100_01' 'REGIO_XD'            'UF XD'                 '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 20  ''  ''             'IT_SAIDA_0100_01' 'REGIO_DIF'           'UF Dif'                '15'     ' '    '' ' ' ' ' ' ' ' ' 'X',
                 21  ''  ''             'IT_SAIDA_0100_01' 'LAND1_XK'            'Pais XK'               '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 22  ''  ''             'IT_SAIDA_0100_01' 'LAND1_XD'            'Pais XD'               '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 23  ''  ''             'IT_SAIDA_0100_01' 'LAND1_DIF'           'Pais Dif'              '15'     ' '    '' ' ' ' ' ' ' ' ' 'X',
                 24  ''  ''             'IT_SAIDA_0100_01' 'ORT01_XK'            'Cidade XK'             '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 25  ''  ''             'IT_SAIDA_0100_01' 'ORT01_XD'            'Cidade XD'             '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 26  ''  ''             'IT_SAIDA_0100_01' 'ORT01_DIF'           'Cidade Dif'            '15'     ' '    '' ' ' ' ' ' ' ' ' 'X',
                 27  ''  ''             'IT_SAIDA_0100_01' 'STRAS_XK'            'Endereco XK'           '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 28  ''  ''             'IT_SAIDA_0100_01' 'STRAS_XD'            'Endereco XD'           '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 29  ''  ''             'IT_SAIDA_0100_01' 'STRAS_DIF'           'Endereco Dif'          '15'     ' '    '' ' ' ' ' ' ' ' ' 'X',
                 30  ''  ''             'IT_SAIDA_0100_01' 'KTOKK_XK'            'Grp.C.Fornecedor'      '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 31  ''  ''             'IT_SAIDA_0100_01' 'KTOKD_XD'            'Grp.C.Cliente'         '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 32  ''  ''             'IT_SAIDA_0100_01' 'CONTAS_BANCO_DIF'    'Contas Banc. Dif.'     '15'     ' '    '' ' ' ' ' ' ' ' ' 'X',
                 33  ''  ''             'IT_SAIDA_0100_01' 'OTHERS_DIF'          'Outras Divergencias'   '99'     ' '    '' ' ' ' ' ' ' ' ' ' '.

            WHEN p_vcxdxk.

              PERFORM f_estrutura_alv USING:

                 01  ''  ''             'IT_SAIDA_0100_02' 'LIFNR'               'Fornecedor'            '10'    ' '    '' ' ' ' ' ' ' ' ' ' ',
                 02  ''  ''             'IT_SAIDA_0100_02' 'KUNNR'               'Cliente'               '10'    ' '    '' ' ' ' ' ' ' ' ' ' ',
                 03  ''  ''             'IT_SAIDA_0100_02' 'NAME1_XK'            'Nome XK'               '25'    ' '    '' ' ' ' ' ' ' ' ' ' ',
                 04  ''  ''             'IT_SAIDA_0100_02' 'NAME1_XD'            'Nome XD'               '25'      ' '    '' ' ' ' ' ' ' ' ' ' ',
                 06  ''  ''             'IT_SAIDA_0100_02' 'STCD1_XK'            'Id.Fiscal 1 XK'        '17'      ' '    '' ' ' ' ' ' ' ' ' ' ',
                 07  ''  ''             'IT_SAIDA_0100_02' 'STCD1_XD'            'Id.Fiscal 1 XD'        '17'      ' '    '' ' ' ' ' ' ' ' ' ' ',
                 09  ''  ''             'IT_SAIDA_0100_02' 'STCD2_XK'            'Id.Fiscal 2 XK'        '17'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 10  ''  ''             'IT_SAIDA_0100_02' 'STCD2_XD'            'Id.Fiscal 2 XD'        '17'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 12  ''  ''             'IT_SAIDA_0100_02' 'STCD3_XK'            'Id.Fiscal 3 XK'        '17'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 13  ''  ''             'IT_SAIDA_0100_02' 'STCD3_XD'            'Id.Fiscal 3 XD'        '17'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 15  ''  ''             'IT_SAIDA_0100_02' 'STCD4_XK'            'Id.Fiscal 4 XK'        '17'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 16  ''  ''             'IT_SAIDA_0100_02' 'STCD4_XD'            'Id.Fiscal 4 XD'        '17'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 18  ''  ''             'IT_SAIDA_0100_02' 'REGIO_XK'            'UF XK'                 '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 19  ''  ''             'IT_SAIDA_0100_02' 'REGIO_XD'            'UF XD'                 '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 21  ''  ''             'IT_SAIDA_0100_02' 'LAND1_XK'            'Pais XK'               '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 22  ''  ''             'IT_SAIDA_0100_02' 'LAND1_XD'            'Pais XD'               '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 24  ''  ''             'IT_SAIDA_0100_02' 'ORT01_XK'            'Cidade XK'             '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 25  ''  ''             'IT_SAIDA_0100_02' 'ORT01_XD'            'Cidade XD'             '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 27  ''  ''             'IT_SAIDA_0100_02' 'STRAS_XK'            'Endereco XK'           '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 28  ''  ''             'IT_SAIDA_0100_02' 'STRAS_XD'            'Endereco XD'           '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 30  ''  ''             'IT_SAIDA_0100_02' 'KTOKK_XK'            'Grp.C.Fornecedor'      '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 31  ''  ''             'IT_SAIDA_0100_02' 'KTOKD_XD'            'Grp.C.Cliente'         '15'     ' '    '' ' ' ' ' ' ' ' ' ' ',
                 32  ''  ''             'IT_SAIDA_0100_02' 'QTDE_XD_VINC'        'Qtde.Cliente Vinc.'    '18'     ' '    '' ' ' ' ' ' ' ' ' ' '.

          ENDCASE.

        WHEN p_xk.    "Fornecedor

          CASE abap_true.
            WHEN p_movgc.


              PERFORM f_estrutura_alv USING:

                01  ''  ''             'IT_SAIDA_0100_03' 'LIFNR'               'Codigo Fornecedor'     '17'      ' '    '' ' ' ' ' ' ' ' ' ' ',
                02  ''  ''             'IT_SAIDA_0100_03' 'NAME1'               'Nome Fornecedor'       '17'      ' '    '' ' ' ' ' ' ' ' ' ' ',
                03  ''  ''             'IT_SAIDA_0100_03' 'KTOKK'               'Grupo Contas'          '17'      ' '    '' ' ' ' ' ' ' ' ' ' ',
                04  ''  ''             'IT_SAIDA_0100_03' 'KTOKK_DEST'          'Novo Grupo'            '17'      ' '    '' ' ' ' ' ' ' ' ' ' '.

              IF p_restgc IS INITIAL. "Restore Grupo Conta

                PERFORM f_estrutura_alv USING:

                05  ''  ''             'IT_SAIDA_0100_03' 'FROMNUMBER'          'Inicio Intervalo'      '17'      ' '    '' ' ' ' ' ' ' ' ' ' ',
                06  ''  ''             'IT_SAIDA_0100_03' 'TONUMBER'            'Fim Intervalo'         '17'      ' '    '' ' ' ' ' ' ' ' ' ' ',
                07  ''  ''             'IT_SAIDA_0100_03' 'BKP_DONE'            'Backup Concluido'      '17'      ' '    '' ' ' 'C' ' ' ' ' 'X',
                08  ''  ''             'IT_SAIDA_0100_03' 'LAYOUT_DIF'          'Layout Dif.'           '17'      ' '    '' ' ' 'C' ' ' ' ' 'X'.

              ELSE.

                PERFORM f_estrutura_alv USING:

                09  ''  ''             'IT_SAIDA_0100_03' 'RESTORE'             'Restore'               '17'      ' '    '' ' ' 'C' ' ' ' ' 'X'.

              ENDIF.

          ENDCASE.

        WHEN p_xd.    "Cliente

          CASE abap_true.
            WHEN p_movgc.

              PERFORM f_estrutura_alv USING:

                01  ''  ''             'IT_SAIDA_0100_03' 'KUNNR'               'Codigo Cliente'        '17'      ' '    '' ' ' ' ' ' ' ' ' ' ',
                02  ''  ''             'IT_SAIDA_0100_03' 'NAME1'               'Nome Cliente'          '17'      ' '    '' ' ' ' ' ' ' ' ' ' ',
                03  ''  ''             'IT_SAIDA_0100_03' 'KTOKD'               'Grupo Contas'          '17'      ' '    '' ' ' ' ' ' ' ' ' ' ',
                04  ''  ''             'IT_SAIDA_0100_03' 'KTOKD_DEST'          'Novo Grupo'            '17'      ' '    '' ' ' ' ' ' ' ' ' ' '.

              IF p_restgc IS INITIAL. "Restore Grupo Conta
                PERFORM f_estrutura_alv USING:

                  05  ''  ''             'IT_SAIDA_0100_03' 'FROMNUMBER'          'Inicio Intervalo'      '17'      ' '    '' ' ' ' ' ' ' ' ' ' ',
                  06  ''  ''             'IT_SAIDA_0100_03' 'TONUMBER'            'Fim Intervalo'         '17'      ' '    '' ' ' ' ' ' ' ' ' ' ',
                  07  ''  ''             'IT_SAIDA_0100_03' 'BKP_DONE'            'Backup Concluido'      '17'      ' '    '' ' ' 'C' ' ' ' ' 'X',
                  08  ''  ''             'IT_SAIDA_0100_03' 'LAYOUT_DIF'          'Layout Dif.'           '17'      ' '    '' ' ' 'C' ' ' ' ' 'X'.
              ELSE.
                PERFORM f_estrutura_alv USING:
                  09  ''  ''             'IT_SAIDA_0100_03' 'RESTORE'             'Restore'               '17'      ' '    '' ' ' 'C' ' ' ' ' 'X'.
              ENDIF.

            WHEN OTHERS.
          ENDCASE.

      ENDCASE.







  ENDCASE.

ENDFORM.

FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                           VALUE(p_tabname)       LIKE dd02d-tabname
                           VALUE(p_field)         LIKE dd03d-fieldname
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                           VALUE(p_outputlen)
                           VALUE(p_edit)
                           VALUE(p_sum)
                           VALUE(p_emphasize)
                           VALUE(p_just)
                           VALUE(p_hotspot)
                           VALUE(p_f4)
                           VALUE(p_checkbox).

  CLEAR wa_fcat.

  wa_fcat-fieldname   = p_field.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-ref_table   = p_ref_tabname.
  wa_fcat-ref_field   = p_ref_fieldname.
  wa_fcat-key         = ' '.
  wa_fcat-edit        = p_edit.
  wa_fcat-col_pos     = p_col_pos.
  wa_fcat-outputlen   = p_outputlen.
  wa_fcat-no_out      = ' '.
  wa_fcat-reptext     = p_scrtext_l.
  wa_fcat-scrtext_s   = p_scrtext_l.
  wa_fcat-scrtext_m   = p_scrtext_l.
  wa_fcat-scrtext_l   = p_scrtext_l.
  wa_fcat-emphasize   = p_emphasize.
  wa_fcat-style       =
  wa_fcat-just        = p_just.
  wa_fcat-hotspot     = p_hotspot.
  wa_fcat-f4availabl  = p_f4.
  wa_fcat-checkbox    = p_checkbox.

  APPEND wa_fcat TO it_fcat.

ENDFORM.                    " ESTRUTURA_ALV

FORM f_exclude_fcode USING p_screen.

  APPEND cl_gui_alv_grid=>mc_fc_refresh           TO it_exclude_fcode.

  CASE p_screen.
    WHEN '0112' OR '0113'.
      APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO it_exclude_fcode.
  ENDCASE.

  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_check             TO it_exclude_fcode.

ENDFORM.

FORM f_xk_xd_process.

  CASE abap_true.
    WHEN p_eqid4.
      PERFORM f_xk_xd_equalize_stcd4.
    WHEN p_eqxdxk.
      PERFORM f_xk_xd_equalize.
    WHEN p_vcxdxk.
      PERFORM f_xk_xd_vincular.
    WHEN p_fftra.
      PERFORM f_fix_xk_xd_forma_tratamento.
    WHEN p_filind.
      PERFORM f_fill_xk_xd_setor_ind.
    WHEN p_flzone.
      PERFORM f_fix_xk_xd_lzone.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.

FORM f_xk_xd_equalize.

  PERFORM f_xk_xd_equalize_saida.

  CALL SCREEN 0100.

ENDFORM.


FORM f_xk_xd_equalize_saida.

  DATA: lwa_lfa1_commom      TYPE ty_commom_xk_xd,
        lwa_kna1_commom      TYPE ty_commom_xk_xd,

        lwa_lfa1_adrc_commom TYPE ty_commom_xk_xd_adrc,
        lwa_kna1_adrc_commom TYPE ty_commom_xk_xd_adrc,

        lwa_adr6_xk          TYPE adr6,
        lwa_adr6_xd          TYPE adr6.

  CLEAR: it_saida_0100_01[],
         git_lfa1[],
         git_kna1[],

         git_adrc[],
         git_adr6[],

         git_lfbk[],
         git_knbk[].

  SELECT f~* INTO CORRESPONDING FIELDS OF TABLE @git_lfa1
    FROM lfa1 AS f INNER JOIN kna1 AS c ON f~kunnr = c~kunnr
    "UP TO @p_maxreg ROWS
   WHERE f~lifnr IN @p_lifnr
     AND f~kunnr IN @p_kunnr.

  CASE p_delxk.
    WHEN abap_true.
      DELETE git_lfa1 WHERE loevm IS INITIAL.
    WHEN abap_false.
      DELETE git_lfa1 WHERE loevm IS NOT INITIAL.
  ENDCASE.

  DELETE git_lfa1 WHERE kunnr IS INITIAL.

  IF git_lfa1[] IS NOT INITIAL.

    SELECT *
       FROM lfbk APPENDING TABLE git_lfbk
       FOR ALL ENTRIES IN git_lfa1
      WHERE lifnr = git_lfa1-lifnr.

    SELECT *
         FROM adrc APPENDING TABLE git_adrc
         FOR ALL ENTRIES IN git_lfa1
        WHERE addrnumber = git_lfa1-adrnr.

    SELECT *
        FROM adr6 APPENDING TABLE git_adr6
        FOR ALL ENTRIES IN git_lfa1
       WHERE addrnumber = git_lfa1-adrnr.

    SELECT *
       FROM kna1 APPENDING TABLE git_kna1
       FOR ALL ENTRIES IN  git_lfa1
      WHERE kunnr = git_lfa1-kunnr.

    IF git_kna1[] IS NOT INITIAL.
      SELECT *
         FROM knbk APPENDING TABLE git_knbk
         FOR ALL ENTRIES IN git_kna1
        WHERE kunnr = git_kna1-kunnr.

      SELECT *
         FROM adrc APPENDING TABLE git_adrc
         FOR ALL ENTRIES IN git_kna1
        WHERE addrnumber = git_kna1-adrnr.

      SELECT *
        FROM adr6 APPENDING TABLE git_adr6
        FOR ALL ENTRIES IN git_kna1
       WHERE addrnumber = git_kna1-adrnr.
    ENDIF.

  ENDIF.

  SORT git_kna1 BY kunnr.

  LOOP AT git_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>).

    CLEAR: wa_saida_0100_01,
           lwa_lfa1_commom,
           lwa_kna1_commom,

           lwa_lfa1_adrc_commom,
           lwa_kna1_adrc_commom,

           lwa_adr6_xk,
           lwa_adr6_xd.

    READ TABLE git_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>) WITH KEY kunnr = <fs_lfa1>-kunnr BINARY SEARCH.

    CHECK sy-subrc EQ 0.

    CASE p_delxd.
      WHEN abap_true.
        CHECK <fs_kna1>-loevm IS NOT INITIAL.
      WHEN abap_false.
        CHECK <fs_kna1>-loevm IS INITIAL.
    ENDCASE.

    MOVE-CORRESPONDING <fs_lfa1> TO lwa_lfa1_commom.
    MOVE-CORRESPONDING <fs_kna1> TO lwa_kna1_commom.


    READ TABLE git_adrc INTO DATA(lwa_adrc) WITH KEY addrnumber = <fs_lfa1>-adrnr.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING lwa_adrc TO lwa_lfa1_adrc_commom.
    ENDIF.

    READ TABLE git_adrc INTO lwa_adrc WITH KEY addrnumber = <fs_kna1>-adrnr.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING lwa_adrc TO lwa_kna1_adrc_commom.
    ENDIF.

    READ TABLE git_adr6 INTO lwa_adr6_xd WITH KEY addrnumber = <fs_kna1>-adrnr
                                                  flgdefault = abap_true.


    wa_saida_0100_01-lifnr               = <fs_lfa1>-lifnr.
    wa_saida_0100_01-kunnr               = <fs_lfa1>-kunnr.

    wa_saida_0100_01-loevm_xk            = <fs_lfa1>-loevm.
    wa_saida_0100_01-loevm_xd            = <fs_kna1>-loevm.

    IF wa_saida_0100_01-loevm_xk <> wa_saida_0100_01-loevm_xd.
      wa_saida_0100_01-loevm_dif = abap_true.
    ENDIF.

    wa_saida_0100_01-name1_xk            = <fs_lfa1>-name1.
    wa_saida_0100_01-name1_xd            = <fs_kna1>-name1.

    IF wa_saida_0100_01-name1_xk <> wa_saida_0100_01-name1_xd.
      wa_saida_0100_01-name1_dif = abap_true.
    ENDIF.

    wa_saida_0100_01-stcd1_xk            = <fs_lfa1>-stcd1.
    wa_saida_0100_01-stcd1_xd            = <fs_kna1>-stcd1.

    IF wa_saida_0100_01-stcd1_xk <> wa_saida_0100_01-stcd1_xd.
      wa_saida_0100_01-stcd1_dif = abap_true.
    ENDIF.

    wa_saida_0100_01-stcd2_xk            = <fs_lfa1>-stcd2.
    wa_saida_0100_01-stcd2_xd            = <fs_kna1>-stcd2.

    IF wa_saida_0100_01-stcd2_xk <> wa_saida_0100_01-stcd2_xd.
      wa_saida_0100_01-stcd2_dif =  abap_true.
    ENDIF.

    wa_saida_0100_01-stcd3_xk            = <fs_lfa1>-stcd3.
    wa_saida_0100_01-stcd3_xd            = <fs_kna1>-stcd3.

    IF wa_saida_0100_01-stcd3_xk <> wa_saida_0100_01-stcd3_xd.
      wa_saida_0100_01-stcd3_dif = abap_true.
    ENDIF.

    wa_saida_0100_01-stcd4_xk            = <fs_lfa1>-stcd4.
    wa_saida_0100_01-stcd4_xd            = <fs_kna1>-stcd4.

    IF wa_saida_0100_01-stcd4_xk <> wa_saida_0100_01-stcd4_xd.
      wa_saida_0100_01-stcd4_dif = abap_true.
    ENDIF.

    wa_saida_0100_01-regio_xk            = <fs_lfa1>-regio.
    wa_saida_0100_01-regio_xd            = <fs_kna1>-regio.

    IF wa_saida_0100_01-regio_xk <> wa_saida_0100_01-regio_xd.
      wa_saida_0100_01-regio_dif = abap_true.
    ENDIF.

    wa_saida_0100_01-land1_xk            = <fs_lfa1>-land1.
    wa_saida_0100_01-land1_xd            = <fs_kna1>-land1.

    IF wa_saida_0100_01-land1_xk <> wa_saida_0100_01-land1_xd.
      wa_saida_0100_01-land1_dif = abap_true.
    ENDIF.

    wa_saida_0100_01-ort01_xk            = <fs_lfa1>-ort01.
    wa_saida_0100_01-ort01_xd            = <fs_kna1>-ort01.

    IF wa_saida_0100_01-ort01_xk <> wa_saida_0100_01-ort01_xd.
      wa_saida_0100_01-ort01_dif = abap_true.
    ENDIF.

    "Será comparado na o campo street e house_no ADRC
    wa_saida_0100_01-stras_xk            = lwa_lfa1_adrc_commom-street.
    wa_saida_0100_01-stras_xd            = lwa_kna1_adrc_commom-street.

    IF wa_saida_0100_01-stras_xk <> wa_saida_0100_01-stras_xd.
      wa_saida_0100_01-stras_dif = abap_true.
    ENDIF.

    IF <fs_lfa1>-stkzn <> <fs_kna1>-stkzn.
      wa_saida_0100_01-others_dif = 'Definição Pessoa Fisica(STKZN)'.
    ENDIF.

*    IF <fs_lfa1>-anred <> <fs_kna1>-anred.
*      wa_saida_0100_01-others_dif = 'Forma Tratamento(ANRED)'.
*    ENDIF.

    IF <fs_lfa1>-pstlz <> <fs_kna1>-pstlz.
      wa_saida_0100_01-others_dif = 'Código postal da cx.postal(PSTLZ)'.
    ENDIF.

    IF <fs_lfa1>-ort02 <> <fs_kna1>-ort02.
      wa_saida_0100_01-others_dif = 'Bairro(ORT02)'.
    ENDIF.

    IF <fs_lfa1>-telf1 <> <fs_kna1>-telf1.
      wa_saida_0100_01-others_dif = '1º Nº telefone(TELF1)'.
    ENDIF.

    IF <fs_lfa1>-telf2 <> <fs_kna1>-telf2.
      wa_saida_0100_01-others_dif = '2º Nº telefone(TELF2)'.
    ENDIF.

    IF <fs_lfa1>-telfx <> <fs_kna1>-telfx.
      wa_saida_0100_01-others_dif = 'Nº telefax(TELFX)'.
    ENDIF.

*    IF <fs_lfa1>-mcod1 <> <fs_kna1>-mcod1.
*      wa_saida_0100_01-others_dif = 'Termo de pesquisa para utilização matchcode(MCOD1)'.
*    ENDIF.
*
*    IF <fs_lfa1>-mcod2 <> <fs_kna1>-mcod2.
*      wa_saida_0100_01-others_dif = 'Termo de pesquisa para utilização matchcode(MCOD2)'.
*    ENDIF.
*
*    IF <fs_lfa1>-mcod3 <> <fs_kna1>-mcod3.
*      wa_saida_0100_01-others_dif = 'Termo de pesquisa para utilização matchcode(MCOD3)'.
*    ENDIF.

    IF <fs_lfa1>-bahns <> <fs_kna1>-bahns.
      wa_saida_0100_01-others_dif = 'Registro Nacional dos Transp.de Cargas(BAHNS)'.
    ENDIF.

    IF <fs_lfa1>-txjcd <> <fs_kna1>-txjcd.
      wa_saida_0100_01-others_dif = 'Domicílio fiscal(TXJCD)'.
    ENDIF.

    IF <fs_lfa1>-stcd5 <> <fs_kna1>-stcd5.
      wa_saida_0100_01-others_dif = 'Domicílio fiscal(TXJCD)'.
    ENDIF.

    IF <fs_lfa1>-name2 <> <fs_kna1>-name2.
      wa_saida_0100_01-others_dif = 'Nome 2(NAME2)'.
    ENDIF.

    IF <fs_lfa1>-name3 <> <fs_kna1>-name3.
      wa_saida_0100_01-others_dif = 'Nome 3(NAME3)'.
    ENDIF.

    IF <fs_lfa1>-name4 <> <fs_kna1>-name4.
      wa_saida_0100_01-others_dif = 'Nome 4(NAME4)'.
    ENDIF.

    IF <fs_lfa1>-sortl <> <fs_kna1>-sortl.
      wa_saida_0100_01-others_dif = 'Campo de seleção(SORTL)'.
    ENDIF.

    IF <fs_lfa1>-lzone <> <fs_kna1>-lzone.
      wa_saida_0100_01-others_dif = 'Standard Carrier Access Code(LZONE)'.
    ENDIF.

    "Comparação Email
    READ TABLE git_adr6 INTO lwa_adr6_xk WITH KEY addrnumber = <fs_lfa1>-adrnr
                                                  flgdefault = abap_true.

    READ TABLE git_adr6 INTO lwa_adr6_xd WITH KEY addrnumber = <fs_kna1>-adrnr
                                                  flgdefault = abap_true.

    IF lwa_adr6_xk-smtp_addr <> lwa_adr6_xd-smtp_addr.
      wa_saida_0100_01-others_dif = 'Email Padrão(ADR6)'.
    ENDIF.

    IF wa_saida_0100_01-others_dif  IS INITIAL.
      IF lwa_lfa1_commom <> lwa_kna1_commom.
        wa_saida_0100_01-others_dif = 'Existem outras diferenças no cadastro(KNA1 - LFA1)'.
      ELSEIF lwa_lfa1_adrc_commom <> lwa_kna1_adrc_commom.
        wa_saida_0100_01-others_dif = 'Existem outras diferenças no endereço(ADRC)'.
      ENDIF.
    ENDIF.

    wa_saida_0100_01-ktokk_xk         = <fs_lfa1>-ktokk.
    wa_saida_0100_01-ktokd_xd         = <fs_kna1>-ktokd.

    PERFORM f_compare_bank_xk_xd CHANGING wa_saida_0100_01.


    IF wa_saida_0100_01-loevm_dif = abap_true OR
      wa_saida_0100_01-name1_dif = abap_true OR
      wa_saida_0100_01-stcd1_dif = abap_true OR
      wa_saida_0100_01-stcd2_dif = abap_true OR
      wa_saida_0100_01-stcd3_dif = abap_true OR
      wa_saida_0100_01-stcd4_dif = abap_true OR
      wa_saida_0100_01-regio_dif = abap_true OR
      wa_saida_0100_01-land1_dif = abap_true OR
      wa_saida_0100_01-ort01_dif = abap_true OR
      wa_saida_0100_01-stras_dif = abap_true OR
      wa_saida_0100_01-others_dif IS NOT INITIAL OR
      wa_saida_0100_01-contas_banco_dif  EQ abap_true.

      APPEND wa_saida_0100_01 TO it_saida_0100_01.


      IF lines( it_saida_0100_01 ) GE p_maxreg.
        EXIT.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.

FORM f_xk_xd_vincular.

  PERFORM f_xk_xd_vincular_saida.

  CASE sy-batch.
    WHEN abap_true.

      DELETE it_saida_0100_02 WHERE qtde_xd_vinc NE 1.

      LOOP AT it_saida_0100_02 INTO wa_saida_0100_02.
        PERFORM f_exec_vinc_xk_xd_reg USING wa_saida_0100_02.
      ENDLOOP.

    WHEN abap_false.
      CALL SCREEN 0100.
  ENDCASE.

ENDFORM.



FORM f_xk_xd_vincular_saida.

  CLEAR: it_saida_0100_02[],
         git_lfa1[],
         git_kna1[],

         git_adrc[],
         git_adr6[],

         git_lfbk[],
         git_knbk[].

  SELECT f~* INTO CORRESPONDING FIELDS OF TABLE @git_lfa1
    FROM lfa1 AS f INNER JOIN kna1 AS c ON f~stcd1 = c~stcd1
                                       AND f~stcd2 = c~stcd2
                                       AND f~stcd3 = c~stcd3
    "UP TO @p_maxreg ROWS
   WHERE ( f~stcd1 NE @space OR
           f~stcd2 NE @space )
     AND f~lifnr IN @p_lifnr
     AND f~kunnr IN @p_kunnr.
  "f~kunnr EQ @space
  "AND f~loevm EQ @space
  "AND c~loevm EQ @space
  "AND f~sperr EQ @space
  "AND c~sperr EQ @space
  DELETE git_lfa1 WHERE kunnr IS NOT INITIAL OR
                        loevm IS NOT INITIAL OR
                        sperr IS NOT INITIAL.

  DELETE git_lfa1 WHERE stcd1 EQ space AND
                        stcd2 EQ space.

  LOOP AT git_lfa1 INTO DATA(lwa_lfa1_aux) FROM p_maxreg + 1.
    DELETE git_lfa1 INDEX sy-tabix.
  ENDLOOP.

  IF git_lfa1[] IS NOT INITIAL.
    SELECT *
       FROM kna1 INTO TABLE git_kna1
       FOR ALL ENTRIES IN  git_lfa1
      WHERE stcd1 = git_lfa1-stcd1
        AND stcd2 = git_lfa1-stcd2
        AND stcd3 = git_lfa1-stcd3.

    DELETE git_kna1 WHERE lifnr IS NOT INITIAL OR
                          loevm IS NOT INITIAL OR
                          sperr IS NOT INITIAL.
  ENDIF.

  SORT git_kna1 BY stcd1 stcd2 stcd3.

  LOOP AT git_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>).
    CLEAR: wa_saida_0100_02.

    READ TABLE git_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>) WITH KEY stcd1 = <fs_lfa1>-stcd1
                                                                   stcd2 = <fs_lfa1>-stcd2
                                                                   stcd3 = <fs_lfa1>-stcd3
                                                                   BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    wa_saida_0100_02-lifnr       = <fs_lfa1>-lifnr.
    wa_saida_0100_02-kunnr       = <fs_kna1>-kunnr.

    wa_saida_0100_02-name1_xk    = <fs_lfa1>-name1.
    wa_saida_0100_02-name1_xd    = <fs_kna1>-name1.

    wa_saida_0100_02-stcd1_xk    = <fs_lfa1>-stcd1.
    wa_saida_0100_02-stcd1_xd    = <fs_kna1>-stcd1.

    wa_saida_0100_02-stcd2_xk    = <fs_lfa1>-stcd2.
    wa_saida_0100_02-stcd2_xd    = <fs_kna1>-stcd2.


    wa_saida_0100_02-stcd3_xk    = <fs_lfa1>-stcd3.
    wa_saida_0100_02-stcd3_xd    = <fs_kna1>-stcd3.

    wa_saida_0100_02-stcd4_xk    = <fs_lfa1>-stcd4.
    wa_saida_0100_02-stcd4_xd    = <fs_kna1>-stcd4.

    wa_saida_0100_02-regio_xk    = <fs_lfa1>-regio.
    wa_saida_0100_02-regio_xd    = <fs_kna1>-regio.

    wa_saida_0100_02-land1_xk    = <fs_lfa1>-land1.
    wa_saida_0100_02-land1_xd    = <fs_kna1>-land1.

    wa_saida_0100_02-ort01_xk    = <fs_lfa1>-ort01.
    wa_saida_0100_02-ort01_xd    = <fs_kna1>-ort01.

    wa_saida_0100_02-stras_xk    = <fs_lfa1>-stras.
    wa_saida_0100_02-stras_xd    = <fs_kna1>-stras.

    wa_saida_0100_02-ktokk_xk    = <fs_lfa1>-ktokk.
    wa_saida_0100_02-ktokd_xd    = <fs_kna1>-ktokd.

    LOOP AT git_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1_cnt>) WHERE stcd1 = <fs_lfa1>-stcd1
                                                             AND stcd2 = <fs_lfa1>-stcd2
                                                             AND stcd3 = <fs_lfa1>-stcd3.
      ADD 1 TO wa_saida_0100_02-qtde_xd_vinc.
    ENDLOOP.

    APPEND wa_saida_0100_02 TO it_saida_0100_02.

    IF lines( it_saida_0100_02 ) GE p_maxreg.
      EXIT.
    ENDIF.

  ENDLOOP.

ENDFORM.



FORM f_compare_bank_xk_xd  CHANGING p_saida_0100_01 TYPE ty_saida_0100_01.

  TYPES: BEGIN OF ty_bank,
           banks         TYPE lfbk-banks,
           bankl         TYPE lfbk-bankl,
           bankn         TYPE lfbk-bankn,
           bkont         TYPE lfbk-bkont,
           bvtyp         TYPE lfbk-bvtyp,
           xezer         TYPE lfbk-xezer,
           bkref         TYPE lfbk-bkref,
           koinh         TYPE lfbk-koinh,
           ebpp_accname  TYPE lfbk-ebpp_accname,
           ebpp_bvstatus TYPE lfbk-ebpp_bvstatus,
           kovon         TYPE lfbk-kovon,
           kobis         TYPE lfbk-kobis,
         END OF ty_bank.

  DATA: lit_bank_xk TYPE TABLE OF ty_bank,
        lit_bank_xd TYPE TABLE OF ty_bank.

  CLEAR: p_saida_0100_01-contas_banco_dif.

  LOOP AT git_knbk ASSIGNING FIELD-SYMBOL(<fs_knbk>) WHERE kunnr EQ p_saida_0100_01-kunnr.

    APPEND INITIAL LINE TO lit_bank_xd  ASSIGNING FIELD-SYMBOL(<fs_bank_xd>).

    <fs_bank_xd>-banks             = <fs_knbk>-banks.
    <fs_bank_xd>-bankl             = <fs_knbk>-bankl.
    <fs_bank_xd>-bankn             = <fs_knbk>-bankn.
    <fs_bank_xd>-bkont             = <fs_knbk>-bkont.
    <fs_bank_xd>-bvtyp             = <fs_knbk>-bvtyp.
    <fs_bank_xd>-xezer             = <fs_knbk>-xezer.
    <fs_bank_xd>-bkref             = <fs_knbk>-bkref.
    <fs_bank_xd>-koinh             = <fs_knbk>-koinh.
    <fs_bank_xd>-ebpp_accname      = <fs_knbk>-ebpp_accname.
    <fs_bank_xd>-ebpp_bvstatus     = <fs_knbk>-ebpp_bvstatus.
    <fs_bank_xd>-kovon             = <fs_knbk>-kovon.
    <fs_bank_xd>-kobis             = <fs_knbk>-kobis.
  ENDLOOP.

  LOOP AT git_lfbk ASSIGNING FIELD-SYMBOL(<fs_lfbk>) WHERE lifnr EQ p_saida_0100_01-lifnr.

    APPEND INITIAL LINE TO lit_bank_xk  ASSIGNING FIELD-SYMBOL(<fs_bank_xk>).

    <fs_bank_xk>-banks             = <fs_lfbk>-banks.
    <fs_bank_xk>-bankl             = <fs_lfbk>-bankl.
    <fs_bank_xk>-bankn             = <fs_lfbk>-bankn.
    <fs_bank_xk>-bkont             = <fs_lfbk>-bkont.
    <fs_bank_xk>-bvtyp             = <fs_lfbk>-bvtyp.
    <fs_bank_xk>-xezer             = <fs_lfbk>-xezer.
    <fs_bank_xk>-bkref             = <fs_lfbk>-bkref.
    <fs_bank_xk>-koinh             = <fs_lfbk>-koinh.
    <fs_bank_xk>-ebpp_accname      = <fs_lfbk>-ebpp_accname.
    <fs_bank_xk>-ebpp_bvstatus     = <fs_lfbk>-ebpp_bvstatus.
    <fs_bank_xk>-kovon             = <fs_lfbk>-kovon.
    <fs_bank_xk>-kobis             = <fs_lfbk>-kobis.
  ENDLOOP.

  IF lit_bank_xk[] NE lit_bank_xd[].
    p_saida_0100_01-contas_banco_dif = abap_true.
  ENDIF.


ENDFORM.

FORM f_processar .

  CASE abap_true.
    WHEN p_xk.
      PERFORM f_xk_process.
    WHEN p_xd.
      PERFORM f_xd_process.
    WHEN p_xkxd.
      PERFORM f_xk_xd_process.
    WHEN OTHERS.
      RETURN.
  ENDCASE.

ENDFORM.

FORM f_exec_equalize_xk_xd .

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente equalizar o cadastro dos registros selecionados?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  DATA(lva_total) = lines( it_sel_rows ).

  LOOP AT it_sel_rows INTO wa_sel_rows.

    DATA(_perc) = ( sy-tabix /  lva_total ) * 100.

    DATA(msg_progress) =  | Processando { sy-tabix } de { lva_total } registros...|.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = _perc
        text       = msg_progress.

    READ TABLE it_saida_0100_01 INTO wa_saida_0100_01 INDEX wa_sel_rows-index.
    CHECK sy-subrc = 0.

    PERFORM f_exec_equalize_xk_xd_reg USING wa_saida_0100_01.

  ENDLOOP.

  PERFORM f_renew_consulta USING '0100'.

ENDFORM.

FORM f_renew_consulta  USING  p_alv.

  CASE p_alv.
    WHEN '0100'.

      CASE abap_true.
        WHEN p_xk.
          CASE abap_true.
            WHEN p_movgc.
              PERFORM f_xk_bkp_ktokk_saida.
            WHEN OTHERS.
          ENDCASE.
        WHEN p_xd.
          CASE abap_true.
            WHEN p_movgc.
              PERFORM f_xd_bkp_ktokd_saida.
            WHEN OTHERS.
          ENDCASE.
        WHEN p_xkxd.
          CASE abap_true.
            WHEN p_eqxdxk.
              PERFORM f_xk_xd_equalize_saida.
            WHEN p_vcxdxk.
              PERFORM f_xk_xd_vincular_saida.
            WHEN OTHERS.
              EXIT.
          ENDCASE.
      ENDCASE.


      PERFORM: f_refresh_alv USING '0100'.

  ENDCASE.


ENDFORM.

FORM f_refresh_alv USING p_alv.
  CASE p_alv.
    WHEN '0100'.
      CALL METHOD obj_alv_0100->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
  ENDCASE.
ENDFORM.


FORM f_get_build_knbk  TABLES t_lit_knbk_bapi STRUCTURE fknbk
                        USING p_saida_0100_01 TYPE ty_saida_0100_01
                              p_operacao      TYPE char50.

  CLEAR: t_lit_knbk_bapi[].

  CASE p_operacao.
    WHEN c_current.
      LOOP AT git_knbk INTO DATA(lwa_knbk) WHERE kunnr = p_saida_0100_01-kunnr.
        APPEND INITIAL LINE TO t_lit_knbk_bapi ASSIGNING FIELD-SYMBOL(<fs_knbk>).

        <fs_knbk>-kunnr           = lwa_knbk-kunnr.
        <fs_knbk>-banks           = lwa_knbk-banks.
        <fs_knbk>-bankl           = lwa_knbk-bankl.
        <fs_knbk>-bankn           = lwa_knbk-bankn.
        <fs_knbk>-bkont           = lwa_knbk-bkont.
        <fs_knbk>-bvtyp           = lwa_knbk-bvtyp.
        <fs_knbk>-xezer           = lwa_knbk-xezer.
        <fs_knbk>-bkref           = lwa_knbk-bkref.
        <fs_knbk>-koinh           = lwa_knbk-koinh.
        <fs_knbk>-ebpp_accname    = lwa_knbk-ebpp_accname.
        <fs_knbk>-ebpp_bvstatus   = lwa_knbk-ebpp_bvstatus.
        <fs_knbk>-kovon           = lwa_knbk-kovon.
        <fs_knbk>-kobis           = lwa_knbk-kobis.
      ENDLOOP.

    WHEN c_new.
      LOOP AT git_lfbk INTO DATA(lwa_lfbk) WHERE lifnr = p_saida_0100_01-lifnr.
        APPEND INITIAL LINE TO t_lit_knbk_bapi ASSIGNING <fs_knbk>.

        <fs_knbk>-kunnr           = p_saida_0100_01-kunnr.
        <fs_knbk>-banks           = lwa_lfbk-banks.
        <fs_knbk>-bankl           = lwa_lfbk-bankl.
        <fs_knbk>-bankn           = lwa_lfbk-bankn.
        <fs_knbk>-bkont           = lwa_lfbk-bkont.
        <fs_knbk>-bvtyp           = lwa_lfbk-bvtyp.
        <fs_knbk>-xezer           = lwa_lfbk-xezer.
        <fs_knbk>-bkref           = lwa_lfbk-bkref.
        <fs_knbk>-koinh           = lwa_lfbk-koinh.
        <fs_knbk>-ebpp_accname    = lwa_lfbk-ebpp_accname.
        <fs_knbk>-ebpp_bvstatus   = lwa_lfbk-ebpp_bvstatus.
        <fs_knbk>-kovon           = lwa_lfbk-kovon.
        <fs_knbk>-kobis           = lwa_lfbk-kobis.
      ENDLOOP.


  ENDCASE.



ENDFORM.

FORM f_exec_equalize_xk_xd_reg  USING  p_saida_0100_01 TYPE ty_saida_0100_01.

  DATA: lwa_adr6_xk TYPE adr6.

*----------------------------------------------------------------------------*
* Estruturas BAPI - Ini
*----------------------------------------------------------------------------*
  DATA: lwa_bapiaddr1 LIKE  bapiaddr1, "PJ
        lwa_bapiaddr2 LIKE  bapiaddr2. "PF

  DATA: lit_xknbk_bapi TYPE TABLE OF fknbk, "Dados Novos
        lit_yknbk_bapi TYPE TABLE OF fknbk. "Dados Antigos/Atuais

  DATA: lwa_kna1_bapi TYPE kna1.
*----------------------------------------------------------------------------*
* Estruturas BAPI - Fim
*----------------------------------------------------------------------------*

  CLEAR: lwa_adr6_xk.

  READ TABLE git_lfa1 INTO DATA(lwa_lfa1) WITH KEY lifnr = p_saida_0100_01-lifnr.
  CHECK sy-subrc EQ 0.

  READ TABLE git_adrc INTO DATA(lwa_adrc_xk) WITH KEY addrnumber = lwa_lfa1-adrnr.
  CHECK sy-subrc EQ 0.

  READ TABLE git_adr6 INTO lwa_adr6_xk WITH KEY addrnumber = lwa_lfa1-adrnr
                                                flgdefault = abap_true.

  READ TABLE git_kna1 INTO DATA(lwa_kna1) WITH KEY kunnr = p_saida_0100_01-kunnr.
  CHECK sy-subrc EQ 0.

  MOVE-CORRESPONDING lwa_kna1 TO lwa_kna1_bapi.

  IF ( lwa_lfa1-loevm EQ abap_true ) AND ( lwa_kna1-loevm EQ abap_false ) .
    MESSAGE 'Fornecedor marcado para eliminação! Operação não permitida!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF NOT ( lwa_kna1-stcd1 EQ lwa_lfa1-stcd1 AND
           lwa_kna1-stcd2 EQ lwa_lfa1-stcd2 AND
           lwa_kna1-stcd3 EQ lwa_lfa1-stcd3 ).
    MESSAGE 'Identificadores fiscais 1,2 e 3 devem ser iguais para equalizar cadastros!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF ( lwa_kna1-lzone NE lwa_lfa1-lzone ).
    MESSAGE 'Cadastros com Zona de transporte diferentes, não podem ser equalizados!' TYPE 'S'.
    EXIT.
  ENDIF.

  "Master Data
  lwa_kna1_bapi-alc           = lwa_lfa1-alc.
  lwa_kna1_bapi-anred         = lwa_lfa1-anred.
  lwa_kna1_bapi-bahns         = lwa_lfa1-bahns.
  lwa_kna1_bapi-bbbnr         = lwa_lfa1-bbbnr.
  lwa_kna1_bapi-bbsnr         = lwa_lfa1-bbsnr.
  lwa_kna1_bapi-begru         = lwa_lfa1-begru.
  lwa_kna1_bapi-brsch         = lwa_lfa1-brsch.
  lwa_kna1_bapi-bubkz         = lwa_lfa1-bubkz.
  lwa_kna1_bapi-cnae          = lwa_lfa1-cnae.
  lwa_kna1_bapi-comsize       = lwa_lfa1-comsize.
  lwa_kna1_bapi-confs         = lwa_lfa1-confs.
  lwa_kna1_bapi-crtn          = lwa_lfa1-crtn.
  lwa_kna1_bapi-cvp_xblck     = lwa_lfa1-cvp_xblck.
  lwa_kna1_bapi-datlt         = lwa_lfa1-datlt.
  lwa_kna1_bapi-decregpc      = lwa_lfa1-decregpc.
  lwa_kna1_bapi-dtams         = lwa_lfa1-dtams.
  lwa_kna1_bapi-dtaws         = lwa_lfa1-dtaws.
  lwa_kna1_bapi-duefl         = lwa_lfa1-duefl.
  lwa_kna1_bapi-exp           = lwa_lfa1-exp.
  lwa_kna1_bapi-fiskn         = lwa_lfa1-fiskn.
  lwa_kna1_bapi-fityp         = lwa_lfa1-fityp.
  lwa_kna1_bapi-icmstaxpay    = lwa_lfa1-icmstaxpay.
  lwa_kna1_bapi-indtyp        = lwa_lfa1-indtyp.
  lwa_kna1_bapi-j_1kfrepre    = lwa_lfa1-j_1kfrepre.
  lwa_kna1_bapi-j_1kftbus     = lwa_lfa1-j_1kftbus.
  lwa_kna1_bapi-j_1kftind     = lwa_lfa1-j_1kftind.
  lwa_kna1_bapi-konzs         = lwa_lfa1-konzs.
  lwa_kna1_bapi-land1         = lwa_lfa1-land1.
  lwa_kna1_bapi-legalnat      = lwa_lfa1-legalnat.
  lwa_kna1_bapi-lzone         = lwa_lfa1-lzone.
  lwa_kna1_bapi-mcod1         = lwa_lfa1-mcod1.
  lwa_kna1_bapi-mcod2         = lwa_lfa1-mcod2.
  lwa_kna1_bapi-mcod3         = lwa_lfa1-mcod3.
  lwa_kna1_bapi-name1         = lwa_lfa1-name1.
  lwa_kna1_bapi-name2         = lwa_lfa1-name2.
  lwa_kna1_bapi-name3         = lwa_lfa1-name3.
  lwa_kna1_bapi-name4         = lwa_lfa1-name4.
  lwa_kna1_bapi-ort01         = lwa_lfa1-ort01.
  lwa_kna1_bapi-ort02         = lwa_lfa1-ort02.
  lwa_kna1_bapi-pfach         = lwa_lfa1-pfach.
  lwa_kna1_bapi-pfort         = lwa_lfa1-pfort.
  lwa_kna1_bapi-pmt_office    = lwa_lfa1-pmt_office.
  lwa_kna1_bapi-psofg         = lwa_lfa1-psofg.
  lwa_kna1_bapi-psohs         = lwa_lfa1-psohs.
  lwa_kna1_bapi-psois         = lwa_lfa1-psois.
  lwa_kna1_bapi-pson1         = lwa_lfa1-pson1.
  lwa_kna1_bapi-pson2         = lwa_lfa1-pson2.
  lwa_kna1_bapi-pson3         = lwa_lfa1-pson3.
  lwa_kna1_bapi-psost         = lwa_lfa1-psost.
  lwa_kna1_bapi-psotl         = lwa_lfa1-psotl.
  lwa_kna1_bapi-psovn         = lwa_lfa1-psovn.
  lwa_kna1_bapi-pstl2         = lwa_lfa1-pstl2.
  lwa_kna1_bapi-pstlz         = lwa_lfa1-pstlz.
  lwa_kna1_bapi-regio         = lwa_lfa1-regio.
  lwa_kna1_bapi-rg            = lwa_lfa1-rg.
  lwa_kna1_bapi-rgdate        = lwa_lfa1-rgdate.
  lwa_kna1_bapi-ric           = lwa_lfa1-ric.
  lwa_kna1_bapi-rne           = lwa_lfa1-rne.
  lwa_kna1_bapi-rnedate       = lwa_lfa1-rnedate.
  lwa_kna1_bapi-sortl         = lwa_lfa1-sortl.
  lwa_kna1_bapi-stcd1         = lwa_lfa1-stcd1.
  lwa_kna1_bapi-stcd2         = lwa_lfa1-stcd2.
  lwa_kna1_bapi-stcd3         = lwa_lfa1-stcd3.
  lwa_kna1_bapi-stcd4         = lwa_lfa1-stcd4.
  lwa_kna1_bapi-stcd5         = lwa_lfa1-stcd5.
  lwa_kna1_bapi-stcdt         = lwa_lfa1-stcdt.
  lwa_kna1_bapi-stceg         = lwa_lfa1-stceg.
  lwa_kna1_bapi-stkza         = lwa_lfa1-stkza.
  lwa_kna1_bapi-stkzn         = lwa_lfa1-stkzn.
  lwa_kna1_bapi-stkzu         = lwa_lfa1-stkzu.
  lwa_kna1_bapi-stras         = lwa_lfa1-stras.
  lwa_kna1_bapi-tdt           = lwa_lfa1-tdt.
  lwa_kna1_bapi-telbx         = lwa_lfa1-telbx.
  lwa_kna1_bapi-telf1         = lwa_lfa1-telf1.
  lwa_kna1_bapi-telf2         = lwa_lfa1-telf2.
  lwa_kna1_bapi-telfx         = lwa_lfa1-telfx.
  lwa_kna1_bapi-teltx         = lwa_lfa1-teltx.
  lwa_kna1_bapi-telx1         = lwa_lfa1-telx1.
  lwa_kna1_bapi-txjcd         = lwa_lfa1-txjcd.
  lwa_kna1_bapi-uf            = lwa_lfa1-uf.
  lwa_kna1_bapi-vbund         = lwa_lfa1-vbund.
  lwa_kna1_bapi-werks         = lwa_lfa1-werks.
  lwa_kna1_bapi-xcpdk         = lwa_lfa1-xcpdk.
  lwa_kna1_bapi-xzemp         = lwa_lfa1-xzemp.

*------------------------------------------------------------------------------*
* Dados Endereço
*------------------------------------------------------------------------------*

  "Estrutura Dados PJ
  lwa_bapiaddr1-title       = lwa_kna1_bapi-anred.
  lwa_bapiaddr1-name        = lwa_kna1_bapi-name1.
  lwa_bapiaddr1-name_2      = lwa_kna1_bapi-name2.
  lwa_bapiaddr1-name_3      = lwa_kna1_bapi-name3.
  lwa_bapiaddr1-name_4      = lwa_kna1_bapi-name4.
  lwa_bapiaddr1-city        = lwa_adrc_xk-city1. "Cidade
  lwa_bapiaddr1-district    = lwa_adrc_xk-city2. "Bairro
  lwa_bapiaddr1-postl_cod1  = lwa_adrc_xk-post_code1. "Código postal da localidade
  lwa_bapiaddr1-postl_cod2  = lwa_adrc_xk-post_code2. "Código postal da cx.postal
  lwa_bapiaddr1-postl_cod3  = lwa_adrc_xk-post_code3. "Código postal da empresa (para clientes importantes)
  lwa_bapiaddr1-street_lng  = lwa_adrc_xk-street.
  lwa_bapiaddr1-country     = lwa_adrc_xk-country.
  lwa_bapiaddr1-region      = lwa_adrc_xk-region.
  lwa_bapiaddr1-tel1_numbr  = lwa_adrc_xk-tel_number.
  lwa_bapiaddr1-tel1_ext    = lwa_adrc_xk-tel_extens.
  lwa_bapiaddr1-fax_number  = lwa_adrc_xk-fax_number.
  lwa_bapiaddr1-fax_extens  = lwa_adrc_xk-fax_extens.
  lwa_bapiaddr1-taxjurcode  = lwa_adrc_xk-taxjurcode.
  lwa_bapiaddr1-time_zone   = lwa_adrc_xk-time_zone.
  lwa_bapiaddr1-transpzone  = lwa_adrc_xk-transpzone.
  lwa_bapiaddr1-e_mail      = lwa_adr6_xk-smtp_addr.
  lwa_bapiaddr1-langu       = lwa_adrc_xk-langu.
  lwa_bapiaddr1-house_no    = lwa_adrc_xk-house_num1.
  lwa_bapiaddr1-house_no2   = lwa_adrc_xk-house_num2.
  lwa_bapiaddr1-house_no3   = lwa_adrc_xk-house_num3.
  lwa_bapiaddr1-sort1       = lwa_adrc_xk-sort1.
  lwa_bapiaddr1-sort2       = lwa_adrc_xk-sort2.
  lwa_bapiaddr1-location    = lwa_adrc_xk-location.
  lwa_bapiaddr1-str_suppl1  = lwa_adrc_xk-str_suppl1.
  lwa_bapiaddr1-str_suppl2  = lwa_adrc_xk-str_suppl2.
  lwa_bapiaddr1-str_suppl3  = lwa_adrc_xk-str_suppl3.
  lwa_bapiaddr1-c_o_name    = lwa_adrc_xk-name_co.
  lwa_bapiaddr1-home_city   = lwa_adrc_xk-home_city.
  lwa_bapiaddr1-room_no     = lwa_adrc_xk-roomnumber.
  lwa_bapiaddr1-floor       = lwa_adrc_xk-floor.

*  IF lwa_kna1_bapi-stkzn EQ abap_true. "Pessoa Fisica
*    lwa_bapiaddr2-title_p     = lwa_kna1_bapi-anred.
*    lwa_bapiaddr2-fullname    = lwa_adrc_xk-name1.
*    lwa_bapiaddr2-lastname    = lwa_adrc_xk-name1.
*    lwa_bapiaddr2-firstname   = lwa_adrc_xk-name2.
*    lwa_bapiaddr2-middlename  = lwa_adrc_xk-name3.
*    lwa_bapiaddr2-secondname  = lwa_adrc_xk-name4.
*    lwa_bapiaddr2-city        = lwa_adrc_xk-city1. "Cidade
*    lwa_bapiaddr2-district    = lwa_adrc_xk-city2. "Bairro
*    lwa_bapiaddr2-postl_cod1  = lwa_adrc_xk-post_code1. "Código postal da localidade
*    lwa_bapiaddr2-postl_cod2  = lwa_adrc_xk-post_code2. "Código postal da cx.postal
*    lwa_bapiaddr2-street      = lwa_adrc_xk-street.
*    lwa_bapiaddr2-country     = lwa_adrc_xk-country.
*    lwa_bapiaddr2-region      = lwa_adrc_xk-region.
*    lwa_bapiaddr2-tel1_numbr  = lwa_adrc_xk-tel_number.
*    lwa_bapiaddr2-tel1_ext    = lwa_adrc_xk-tel_extens.
*    lwa_bapiaddr2-fax_number  = lwa_adrc_xk-fax_number.
*    lwa_bapiaddr2-fax_extens  = lwa_adrc_xk-fax_extens.
*    lwa_bapiaddr2-taxjurcode  = lwa_adrc_xk-taxjurcode.
*    lwa_bapiaddr2-time_zone   = lwa_adrc_xk-time_zone.
*    lwa_bapiaddr2-transpzone  = lwa_adrc_xk-transpzone.
*    lwa_bapiaddr2-e_mail      = lwa_adr6_xk-smtp_addr.
*    lwa_bapiaddr2-langu_p     = lwa_adrc_xk-langu.
*    lwa_bapiaddr2-house_no    = lwa_adrc_xk-house_num1.
*    lwa_bapiaddr2-house_no2   = lwa_adrc_xk-house_num2.
*    lwa_bapiaddr2-house_no3   = lwa_adrc_xk-house_num3.
*    lwa_bapiaddr2-sort1_p     = lwa_adrc_xk-sort1.
*    lwa_bapiaddr2-sort2_p     = lwa_adrc_xk-sort2.
*    lwa_bapiaddr2-location    = lwa_adrc_xk-location.
*    lwa_bapiaddr2-str_suppl1  = lwa_adrc_xk-str_suppl1.
*    lwa_bapiaddr2-str_suppl2  = lwa_adrc_xk-str_suppl2.
*    lwa_bapiaddr2-str_suppl3  = lwa_adrc_xk-str_suppl3.
*    lwa_bapiaddr2-c_o_name    = lwa_adrc_xk-name_co.
*    lwa_bapiaddr2-home_city   = lwa_adrc_xk-home_city.
*  ENDIF.

  PERFORM f_get_build_knbk  TABLES lit_yknbk_bapi
                             USING p_saida_0100_01
                                   c_current.

  PERFORM f_get_build_knbk  TABLES lit_xknbk_bapi
                             USING p_saida_0100_01
                                   c_new.

  CALL FUNCTION 'SD_CUSTOMER_MAINTAIN_ALL'
    EXPORTING
      i_kna1                  = lwa_kna1_bapi
      i_bapiaddr1             = lwa_bapiaddr1
      i_bapiaddr2             = lwa_bapiaddr2
      "i_maintain_address_by_kna1 = 'X'
      pi_postflag             = 'X'
    TABLES
      t_xknbk                 = lit_xknbk_bapi
      t_yknbk                 = lit_yknbk_bapi
    EXCEPTIONS
      client_error            = 1
      kna1_incomplete         = 2
      knb1_incomplete         = 3
      knb5_incomplete         = 4
      knvv_incomplete         = 5
      kunnr_not_unique        = 6
      sales_area_not_unique   = 7
      sales_area_not_valid    = 8
      insert_update_conflict  = 9
      number_assignment_error = 10
      number_not_in_range     = 11
      number_range_not_extern = 12
      number_range_not_intern = 13
      account_group_not_valid = 14
      parnr_invalid           = 15
      bank_address_invalid    = 16
      tax_data_not_valid      = 17
      no_authority            = 18
      company_code_not_unique = 19
      dunning_data_not_valid  = 20
      knb1_reference_invalid  = 21
      cam_error               = 22
      OTHERS                  = 23.
  IF sy-subrc <> 0.
    MESSAGE |Erro ao equalizar cadastro cliente: { p_saida_0100_01-kunnr } | TYPE 'I'.
  ENDIF.

  COMMIT WORK.

ENDFORM.


FORM f_exec_vinc_xk_xd .

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente vincular o cadastro dos registros selecionados?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0100_02 INTO wa_saida_0100_02 INDEX wa_sel_rows-index.
    CHECK sy-subrc = 0.

    PERFORM f_exec_vinc_xk_xd_reg USING wa_saida_0100_02.

  ENDLOOP.

  PERFORM f_renew_consulta USING '0100'.

ENDFORM.

FORM f_exec_vinc_xk_xd_reg  USING  p_saida_0100_02 TYPE ty_saida_0100_02.

*----------------------------------------------------------------------------*
* Estruturas BAPI - Ini
*----------------------------------------------------------------------------*

  DATA: lwa_kna1_bapi TYPE kna1.
*----------------------------------------------------------------------------*
* Estruturas BAPI - Fim
*----------------------------------------------------------------------------*

  IF p_saida_0100_02-qtde_xd_vinc > 1.
    MESSAGE 'Existe mais de um cliente vinculado com mesmo Id.Fiscal 1,2,3...' TYPE 'S'.
    EXIT.
  ENDIF.

  CHECK p_saida_0100_02-kunnr IS NOT INITIAL AND p_saida_0100_02-lifnr IS NOT INITIAL.

  READ TABLE git_lfa1 INTO DATA(lwa_lfa1) WITH KEY lifnr = p_saida_0100_02-lifnr.
  CHECK sy-subrc EQ 0.

  READ TABLE git_kna1 INTO DATA(lwa_kna1) WITH KEY kunnr = p_saida_0100_02-kunnr.
  CHECK sy-subrc EQ 0.

  MOVE-CORRESPONDING lwa_kna1 TO lwa_kna1_bapi.

  CHECK lwa_lfa1-kunnr IS INITIAL OR lwa_kna1-lifnr IS INITIAL.

  UPDATE lfa1 SET kunnr = p_saida_0100_02-kunnr
   WHERE lifnr EQ p_saida_0100_02-lifnr.

  UPDATE kna1 SET lifnr = p_saida_0100_02-lifnr
   WHERE kunnr EQ p_saida_0100_02-kunnr.

  COMMIT WORK.

  MESSAGE |Fornecedor { p_saida_0100_02-lifnr } vinculado com Cliente { p_saida_0100_02-kunnr } com sucesso!| TYPE 'S'.

  EXIT.


  "CHECK lwa_kna1_bapi-lifnr IS INITIAL.
  DATA(i_mode) = zif_shdb=>st_tipo_mode_sem_tela_sem_debu.
  TRY .


      zcl_shdb=>zif_shdb~get_instance(
                    )->set_add_bdcdata( i_bdcdata = VALUE #( program = 'SAPMF02K'  dynpro = '0101' dynbegin = 'X' )
                    )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'BDC_CURSOR'   fval = 'RF02K-D0120'          )
                    )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'BDC_OKCODE'   fval = '/00'     )

                    )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'RF02K-LIFNR'  fval = p_saida_0100_02-lifnr )
                    )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'RF02K-D0120'  fval = 'X' )

                    )->set_add_bdcdata( i_bdcdata = VALUE #( program = 'SAPMF02K'    dynpro = '0120' dynbegin = 'X' )
                    )->set_add_bdcdata( i_bdcdata = VALUE #( fnam    = 'BDC_CURSOR'  fval = 'LFA1-KUNNR' )
                    )->set_add_bdcdata( i_bdcdata = VALUE #( fnam    = 'LFA1-KUNNR'  fval =  p_saida_0100_02-kunnr     )
                    )->set_add_bdcdata( i_bdcdata = VALUE #( fnam    = 'BDC_OKCODE'  fval = '=UPDA'     )
                    )->set_add_bdcdata( i_bdcdata = VALUE #( fnam    = 'BDC_OKCODE'   fval = '/00'     )

                    )->set_transaction( i_tcode   = CONV #( 'XK02' )
                    )->set_mode( i_mode = i_mode
                    )->set_executar(
                    )->get_ck_existe_msg_erro( IMPORTING e_msg = DATA(e_msg_erro) e_msg_tab = DATA(e_msg_tab)
                    ).

    CATCH zcx_shdb INTO DATA(ex_shdb).

      IF zcx_shdb=>zcx_sem_msg_erro-msgid NE ex_shdb->msgid OR
         zcx_shdb=>zcx_sem_msg_erro-msgno NE ex_shdb->msgno.

        RAISE EXCEPTION TYPE zcx_shdb
          EXPORTING
            textid = VALUE #( msgid = ex_shdb->msgid
                              msgno = ex_shdb->msgno
                              attr1 = CONV #( ex_shdb->msgv1 )
                              attr2 = CONV #( ex_shdb->msgv2 )
                              attr3 = CONV #( ex_shdb->msgv3 )
                              attr4 = CONV #( ex_shdb->msgv4 ) )
            msgid  = ex_shdb->msgid
            msgno  = ex_shdb->msgno
            msgty  = 'E'
            msgv1  = ex_shdb->msgv1
            msgv2  = ex_shdb->msgv2
            msgv3  = ex_shdb->msgv3
            msgv4  = ex_shdb->msgv4.

      ELSE.
        MESSAGE |Fornecedor { p_saida_0100_02-lifnr } vinculado com Cliente { p_saida_0100_02-kunnr } com sucesso!| TYPE 'S'.
      ENDIF.

  ENDTRY.

*  UPDATE lfa1 SET kunnr = p_saida_0100_02-kunnr
*   WHERE lifnr EQ p_saida_0100_02-lifnr
*     AND kunnr EQ space.
*
*  UPDATE kna1 SET lifnr = p_saida_0100_02-lifnr
*   WHERE kunnr EQ p_saida_0100_02-kunnr
*     AND lifnr EQ space.


*  lwa_kna1_bapi-lifnr      = lwa_lfa1-lifnr.

*  CALL FUNCTION 'SD_CUSTOMER_MAINTAIN_ALL'
*    EXPORTING
*      i_kna1                     = lwa_kna1_bapi
*      "i_maintain_address_by_kna1 = 'X'
*      pi_postflag                = 'X'
*    EXCEPTIONS
*      client_error               = 1
*      kna1_incomplete            = 2
*      knb1_incomplete            = 3
*      knb5_incomplete            = 4
*      knvv_incomplete            = 5
*      kunnr_not_unique           = 6
*      sales_area_not_unique      = 7
*      sales_area_not_valid       = 8
*      insert_update_conflict     = 9
*      number_assignment_error    = 10
*      number_not_in_range        = 11
*      number_range_not_extern    = 12
*      number_range_not_intern    = 13
*      account_group_not_valid    = 14
*      parnr_invalid              = 15
*      bank_address_invalid       = 16
*      tax_data_not_valid         = 17
*      no_authority               = 18
*      company_code_not_unique    = 19
*      dunning_data_not_valid     = 20
*      knb1_reference_invalid     = 21
*      cam_error                  = 22
*      OTHERS                     = 23.
*  IF sy-subrc <> 0.
*    MESSAGE |Erro ao equalizar cadastro cliente: { p_saida_0100_02-kunnr } | TYPE 'I'.
*  ENDIF.

  COMMIT WORK.



ENDFORM.

FORM f_xk_bkp_ktokk.

  PERFORM f_xk_bkp_ktokk_saida.

  CALL SCREEN 0100.

ENDFORM.


FORM f_xk_bkp_ktokk_saida.

  CONSTANTS: c_object TYPE nriv-object VALUE 'KREDITOR'.

  DATA: lit_lfa1_custom       TYPE TABLE OF ty_lfa1_custom.
  DATA: lit_nriv_custom       TYPE TABLE OF ty_nriv_custom.
  DATA: lit_nriv_custom_geral TYPE TABLE OF ty_nriv_custom.

  CLEAR: it_saida_0100_03[].

  CASE p_restgc.
    WHEN abap_false. "Backup e Trasnferencia Grupo Conta

      SELECT a~* INTO CORRESPONDING FIELDS OF TABLE @lit_lfa1_custom
        FROM lfa1 AS a INNER JOIN t077k AS g ON a~ktokk = g~ktokk
                       INNER JOIN nriv  AS c ON c~object = @c_object
                                            AND g~numkr  = c~nrrangenr
         WHERE a~lifnr LT c~fromnumber.

      SELECT a~* APPENDING CORRESPONDING FIELDS OF TABLE @lit_lfa1_custom
        FROM lfa1 AS a INNER JOIN t077k AS g ON a~ktokk  = g~ktokk
                        INNER JOIN nriv  AS c ON c~object = @c_object
                                             AND g~numkr  = c~nrrangenr
       WHERE a~lifnr GT c~tonumber.


      CHECK lit_lfa1_custom[] IS NOT INITIAL.

      DELETE lit_lfa1_custom WHERE lifnr NOT IN p_lifnr.

      SORT lit_lfa1_custom BY lifnr.
      DELETE ADJACENT DUPLICATES FROM lit_lfa1_custom COMPARING lifnr.

      SELECT *
         FROM t077k INTO TABLE @DATA(lit_t077k)
          FOR ALL ENTRIES IN @lit_lfa1_custom
        WHERE ktokk = @lit_lfa1_custom-ktokk.

      IF lit_t077k[] IS NOT INITIAL.
        SELECT *
         FROM nriv INTO CORRESPONDING FIELDS OF TABLE lit_nriv_custom
          FOR ALL ENTRIES IN lit_t077k
        WHERE object    = c_object
          AND nrrangenr = lit_t077k-numkr.
      ENDIF.

      SELECT *
         FROM zbkp_grp_conta INTO TABLE @DATA(lit_zbkp_grp_conta)
          FOR ALL ENTRIES IN @lit_lfa1_custom
        WHERE lifnr = @lit_lfa1_custom-lifnr.

      DELETE lit_zbkp_grp_conta WHERE ktokk IS INITIAL.

      "Busca Grupos Corretos
      SELECT c~object,
             c~nrrangenr,
             c~fromnumber,
             c~tonumber,
             g~ktokk,
             g~fausa,
             g~fausf,
             g~fausm,
             g~faus1,
             g~fausg,
             g~fausn,
             g~fausx,
             g~fausu,
             g~faus2,
             g~faus3
          APPENDING CORRESPONDING FIELDS OF TABLE @lit_nriv_custom_geral
          FROM nriv AS c INNER JOIN t077k AS g ON c~object = @c_object
                                              AND g~numkr  = c~nrrangenr.

      LOOP AT lit_lfa1_custom ASSIGNING FIELD-SYMBOL(<fs_lfa1>).
        CLEAR: wa_saida_0100_03.

        wa_saida_0100_03-lifnr       = <fs_lfa1>-lifnr.
        wa_saida_0100_03-name1       = <fs_lfa1>-name1.
        wa_saida_0100_03-ktokk       = <fs_lfa1>-ktokk.

        READ TABLE lit_t077k INTO DATA(lwa_t077k) WITH KEY ktokk = <fs_lfa1>-ktokk.
        IF sy-subrc EQ 0.

          READ TABLE lit_nriv_custom INTO DATA(lwa_nriv) WITH KEY nrrangenr = lwa_t077k-numkr.
          IF sy-subrc EQ 0.
            wa_saida_0100_03-fromnumber = lwa_nriv-fromnumber.
            wa_saida_0100_03-tonumber   = lwa_nriv-tonumber.

            LOOP AT lit_nriv_custom_geral INTO DATA(lwa_nriv_new) WHERE fromnumber   LE <fs_lfa1>-lifnr
                                                                    AND tonumber     GE <fs_lfa1>-lifnr
                                                                    AND fausa        EQ lwa_t077k-fausa
                                                                    AND fausf        EQ lwa_t077k-fausf
                                                                    AND fausm        EQ lwa_t077k-fausm
                                                                    AND faus1        EQ lwa_t077k-faus1
                                                                    AND fausg        EQ lwa_t077k-fausg
                                                                    AND fausn        EQ lwa_t077k-fausn
                                                                    AND fausx        EQ lwa_t077k-fausx
                                                                    AND fausu        EQ lwa_t077k-fausu
                                                                    AND faus2        EQ lwa_t077k-faus2
                                                                    AND faus3        EQ lwa_t077k-faus3.

              wa_saida_0100_03-ktokk_dest  = lwa_nriv_new-ktokk.
              EXIT.
            ENDLOOP.

            IF wa_saida_0100_03-ktokk_dest IS INITIAL. "Busca Grupo de Contas com Layout Diferente
              LOOP AT lit_nriv_custom_geral INTO lwa_nriv_new WHERE fromnumber   LE <fs_lfa1>-lifnr
                                                                AND tonumber     GE <fs_lfa1>-lifnr.
                wa_saida_0100_03-ktokk_dest  = lwa_nriv_new-ktokk.
                wa_saida_0100_03-layout_dif  = abap_true.
                EXIT.
              ENDLOOP.
            ENDIF.

          ENDIF.
        ENDIF.

        READ TABLE lit_zbkp_grp_conta INTO DATA(lwa_zbkp_grp_conta) WITH KEY lifnr = <fs_lfa1>-lifnr.
        IF sy-subrc EQ 0 AND <fs_lfa1>-lifnr IS NOT INITIAL AND lwa_zbkp_grp_conta-ktokk IS NOT INITIAL.
          wa_saida_0100_03-bkp_done   = abap_true.
          IF ( lwa_zbkp_grp_conta-ktokk_dest IS NOT INITIAL ) AND lwa_zbkp_grp_conta-layout_dif EQ abap_true.
            wa_saida_0100_03-layout_dif = lwa_zbkp_grp_conta-layout_dif.
          ENDIF.
        ENDIF.

        APPEND wa_saida_0100_03 TO it_saida_0100_03.

      ENDLOOP.


    WHEN abap_true. "Restore Grupo Conta

      SELECT a~* INTO CORRESPONDING FIELDS OF TABLE @lit_lfa1_custom
        FROM lfa1 AS a INNER JOIN zbkp_grp_conta AS g ON a~lifnr = g~lifnr.

      CHECK lit_lfa1_custom[] IS NOT INITIAL.

      SELECT *
        FROM zbkp_grp_conta INTO TABLE @lit_zbkp_grp_conta
         FOR ALL ENTRIES IN @lit_lfa1_custom
       WHERE lifnr = @lit_lfa1_custom-lifnr.

      LOOP AT lit_lfa1_custom ASSIGNING <fs_lfa1>.

        CHECK <fs_lfa1>-lifnr IS NOT INITIAL.

        CLEAR: wa_saida_0100_03.

        READ TABLE lit_zbkp_grp_conta INTO lwa_zbkp_grp_conta WITH KEY lifnr = <fs_lfa1>-lifnr.
        CHECK ( sy-subrc EQ 0 ) AND
              ( lwa_zbkp_grp_conta-ktokk      IS NOT INITIAL ) AND
              ( lwa_zbkp_grp_conta-ktokk_dest IS NOT INITIAL ).

        wa_saida_0100_03-lifnr       = <fs_lfa1>-lifnr.
        wa_saida_0100_03-name1       = <fs_lfa1>-name1.
        wa_saida_0100_03-ktokk       = <fs_lfa1>-ktokk.
        wa_saida_0100_03-ktokk_dest  = lwa_zbkp_grp_conta-ktokk.
        wa_saida_0100_03-restore     = lwa_zbkp_grp_conta-restore.

        APPEND wa_saida_0100_03 TO it_saida_0100_03.
      ENDLOOP.

  ENDCASE.


ENDFORM.

FORM f_xd_bkp_ktokd.
  PERFORM f_xd_bkp_ktokd_saida.
  CALL SCREEN 0100.
ENDFORM.

FORM f_xd_bkp_ktokd_saida.

  CONSTANTS: c_object TYPE nriv-object VALUE 'DEBITOR'.

  DATA: lit_kna1_custom       TYPE TABLE OF ty_kna1_custom.
  DATA: lit_nriv_custom       TYPE TABLE OF ty_nriv_custom.
  DATA: lit_nriv_custom_geral TYPE TABLE OF ty_nriv_custom.

  CLEAR: it_saida_0100_03[].

  CASE p_restgc.
    WHEN abap_false. "Backup e Trasnferencia Grupo Conta

      SELECT a~* INTO CORRESPONDING FIELDS OF TABLE @lit_kna1_custom
        FROM kna1 AS a INNER JOIN t077d AS g ON a~ktokd = g~ktokd
                       INNER JOIN nriv  AS c ON c~object = @c_object
                                            AND g~numkr  = c~nrrangenr
         WHERE a~kunnr LT c~fromnumber.

      SELECT a~* APPENDING CORRESPONDING FIELDS OF TABLE @lit_kna1_custom
        FROM kna1 AS a INNER JOIN t077d AS g ON a~ktokd  = g~ktokd
                        INNER JOIN nriv  AS c ON c~object = @c_object
                                             AND g~numkr  = c~nrrangenr
       WHERE a~kunnr GT c~tonumber.


      CHECK lit_kna1_custom[] IS NOT INITIAL.

      DELETE lit_kna1_custom WHERE kunnr NOT IN p_kunnr.

      SORT lit_kna1_custom BY kunnr.
      DELETE ADJACENT DUPLICATES FROM lit_kna1_custom COMPARING kunnr.

      SELECT *
         FROM t077d INTO TABLE @DATA(lit_t077d)
          FOR ALL ENTRIES IN @lit_kna1_custom
        WHERE ktokd = @lit_kna1_custom-ktokd.

      IF lit_t077d[] IS NOT INITIAL.
        SELECT *
         FROM nriv INTO CORRESPONDING FIELDS OF TABLE lit_nriv_custom
          FOR ALL ENTRIES IN lit_t077d
        WHERE object    = c_object
          AND nrrangenr = lit_t077d-numkr.
      ENDIF.

      SELECT *
         FROM zbkp_grp_conta INTO TABLE @DATA(lit_zbkp_grp_conta)
          FOR ALL ENTRIES IN @lit_kna1_custom
        WHERE kunnr = @lit_kna1_custom-kunnr.

      DELETE lit_zbkp_grp_conta WHERE ktokd IS INITIAL.

      "Busca Grupos Corretos
      SELECT c~object,
             c~nrrangenr,
             c~fromnumber,
             c~tonumber,
             g~ktokd,
             g~fausa,
             g~fausf,
             g~faus1,
             g~fausg,
             g~fausu,
             g~faus2,
             g~faus3
          APPENDING CORRESPONDING FIELDS OF TABLE @lit_nriv_custom_geral
          FROM nriv AS c INNER JOIN t077d AS g ON c~object = @c_object
                                              AND g~numkr  = c~nrrangenr.

      LOOP AT lit_kna1_custom ASSIGNING FIELD-SYMBOL(<fs_kna1>).
        CLEAR: wa_saida_0100_03.

        wa_saida_0100_03-kunnr       = <fs_kna1>-kunnr.
        wa_saida_0100_03-name1       = <fs_kna1>-name1.
        wa_saida_0100_03-ktokd       = <fs_kna1>-ktokd.

        READ TABLE lit_t077d INTO DATA(lwa_t077d) WITH KEY ktokd = <fs_kna1>-ktokd.
        IF sy-subrc EQ 0.

          READ TABLE lit_nriv_custom INTO DATA(lwa_nriv) WITH KEY nrrangenr = lwa_t077d-numkr.
          IF sy-subrc EQ 0.
            wa_saida_0100_03-fromnumber = lwa_nriv-fromnumber.
            wa_saida_0100_03-tonumber   = lwa_nriv-tonumber.

            LOOP AT lit_nriv_custom_geral INTO DATA(lwa_nriv_new) WHERE fromnumber   LE <fs_kna1>-kunnr
                                                                    AND tonumber     GE <fs_kna1>-kunnr
                                                                    AND fausa        EQ lwa_t077d-fausa
                                                                    AND fausf        EQ lwa_t077d-fausf
                                                                    AND faus1        EQ lwa_t077d-faus1
                                                                    AND fausg        EQ lwa_t077d-fausg
                                                                    AND fausu        EQ lwa_t077d-fausu
                                                                    AND faus2        EQ lwa_t077d-faus2
                                                                    AND faus3        EQ lwa_t077d-faus3.

              wa_saida_0100_03-ktokd_dest  = lwa_nriv_new-ktokd.
              EXIT.
            ENDLOOP.

            IF wa_saida_0100_03-ktokd_dest IS INITIAL. "Busca Grupo de Contas com Layout Diferente
              LOOP AT lit_nriv_custom_geral INTO lwa_nriv_new WHERE fromnumber   LE <fs_kna1>-kunnr
                                                                AND tonumber     GE <fs_kna1>-kunnr.
                wa_saida_0100_03-ktokd_dest  = lwa_nriv_new-ktokd.
                wa_saida_0100_03-layout_dif  = abap_true.
                EXIT.
              ENDLOOP.
            ENDIF.

          ENDIF.
        ENDIF.

        READ TABLE lit_zbkp_grp_conta INTO DATA(lwa_zbkp_grp_conta) WITH KEY kunnr = <fs_kna1>-kunnr.
        IF sy-subrc EQ 0 AND <fs_kna1>-kunnr IS NOT INITIAL AND lwa_zbkp_grp_conta-ktokd IS NOT INITIAL.
          wa_saida_0100_03-bkp_done   = abap_true.
          IF ( lwa_zbkp_grp_conta-ktokd_dest IS NOT INITIAL ) AND lwa_zbkp_grp_conta-layout_dif EQ abap_true.
            wa_saida_0100_03-layout_dif = lwa_zbkp_grp_conta-layout_dif.
          ENDIF.
        ENDIF.

        APPEND wa_saida_0100_03 TO it_saida_0100_03.

      ENDLOOP.


    WHEN abap_true. "Restore Grupo Conta

      SELECT a~* INTO CORRESPONDING FIELDS OF TABLE @lit_kna1_custom
        FROM kna1 AS a INNER JOIN zbkp_grp_conta AS g ON a~kunnr = g~kunnr.

      CHECK lit_kna1_custom[] IS NOT INITIAL.

      SELECT *
        FROM zbkp_grp_conta INTO TABLE @lit_zbkp_grp_conta
         FOR ALL ENTRIES IN @lit_kna1_custom
       WHERE kunnr = @lit_kna1_custom-kunnr.

      LOOP AT lit_kna1_custom ASSIGNING <fs_kna1>.

        CHECK <fs_kna1>-kunnr IS NOT INITIAL.

        CLEAR: wa_saida_0100_03.

        READ TABLE lit_zbkp_grp_conta INTO lwa_zbkp_grp_conta WITH KEY kunnr = <fs_kna1>-kunnr.
        CHECK ( sy-subrc EQ 0 ) AND
              ( lwa_zbkp_grp_conta-ktokd      IS NOT INITIAL ) AND
              ( lwa_zbkp_grp_conta-ktokd_dest IS NOT INITIAL ).

        wa_saida_0100_03-kunnr       = <fs_kna1>-kunnr.
        wa_saida_0100_03-name1       = <fs_kna1>-name1.
        wa_saida_0100_03-ktokd       = <fs_kna1>-ktokd.
        wa_saida_0100_03-ktokd_dest  = lwa_zbkp_grp_conta-ktokd.
        wa_saida_0100_03-restore     = lwa_zbkp_grp_conta-restore.

        APPEND wa_saida_0100_03 TO it_saida_0100_03.
      ENDLOOP.

  ENDCASE.

ENDFORM. "FORM f_xd_bkp_ktokd_saida.

FORM f_exec_bkp_ktokk_xk .

  DATA(_bkp_execute) = abap_false.
  LOOP AT it_saida_0100_03 INTO DATA(lwa_saida_0100_03).

    CHECK lwa_saida_0100_03-lifnr IS NOT INITIAL.

    SELECT SINGLE *
      FROM zbkp_grp_conta INTO @DATA(lwa_zbkp_grp_conta)
     WHERE lifnr EQ @lwa_saida_0100_03-lifnr.

    IF ( sy-subrc EQ 0 ) AND ( lwa_zbkp_grp_conta-ktokk IS NOT INITIAL ).
      CONTINUE. "Backup já feito
    ELSE.
      _bkp_execute = abap_true.
      lwa_zbkp_grp_conta-lifnr        = lwa_saida_0100_03-lifnr.
      lwa_zbkp_grp_conta-ktokk        = lwa_saida_0100_03-ktokk.
      lwa_zbkp_grp_conta-dt_registro  = sy-datum.
      lwa_zbkp_grp_conta-hr_registro  = sy-uzeit.
      lwa_zbkp_grp_conta-us_registro  = sy-uname.
      MODIFY zbkp_grp_conta FROM lwa_zbkp_grp_conta.
    ENDIF.
  ENDLOOP.

  CASE _bkp_execute.
    WHEN abap_true.
      MESSAGE 'Backups realizados!' TYPE 'S'.
    WHEN abap_false.
      MESSAGE 'Nenhum Backup realizado!' TYPE 'I'.
  ENDCASE.

  PERFORM f_renew_consulta USING '0100'.

ENDFORM.

FORM f_exec_bkp_ktokd_xd .

  DATA(_bkp_execute) = abap_false.
  LOOP AT it_saida_0100_03 INTO DATA(lwa_saida_0100_03).

    CHECK lwa_saida_0100_03-kunnr IS NOT INITIAL.

    SELECT SINGLE *
      FROM zbkp_grp_conta INTO @DATA(lwa_zbkp_grp_conta)
     WHERE kunnr EQ @lwa_saida_0100_03-kunnr.

    IF ( sy-subrc EQ 0 ) AND ( lwa_zbkp_grp_conta-ktokd IS NOT INITIAL ).
      CONTINUE. "Backup já feito
    ELSE.
      _bkp_execute = abap_true.
      lwa_zbkp_grp_conta-kunnr        = lwa_saida_0100_03-kunnr.
      lwa_zbkp_grp_conta-ktokd        = lwa_saida_0100_03-ktokd.
      lwa_zbkp_grp_conta-dt_registro  = sy-datum.
      lwa_zbkp_grp_conta-hr_registro  = sy-uzeit.
      lwa_zbkp_grp_conta-us_registro  = sy-uname.
      MODIFY zbkp_grp_conta FROM lwa_zbkp_grp_conta.
    ENDIF.
  ENDLOOP.

  CASE _bkp_execute.
    WHEN abap_true.
      MESSAGE 'Backups realizados!' TYPE 'S'.
    WHEN abap_false.
      MESSAGE 'Nenhum Backup realizado!' TYPE 'I'.
  ENDCASE.

  PERFORM f_renew_consulta USING '0100'.

ENDFORM.


FORM f_exec_transf_ktokk_xk .

  DATA(_transf_execute) = abap_false.
  DATA(_transf_error)  = abap_false.
  LOOP AT it_saida_0100_03 INTO DATA(lwa_saida_0100_03).

    CHECK lwa_saida_0100_03-lifnr IS NOT INITIAL.

    SELECT SINGLE *
      FROM zbkp_grp_conta INTO @DATA(lwa_zbkp_grp_conta)
     WHERE lifnr EQ @lwa_saida_0100_03-lifnr.

    IF NOT ( sy-subrc EQ 0 AND lwa_zbkp_grp_conta-ktokk IS NOT INITIAL ) .
      _transf_error = abap_true.
      MESSAGE 'Existem registros sem backup...' TYPE 'S'.
      CONTINUE.
    ENDIF.

    IF lwa_zbkp_grp_conta-ktokk_dest IS NOT INITIAL.
      CONTINUE. "Transferencia já feita...
    ENDIF.

    IF ( lwa_saida_0100_03-ktokk_dest IS INITIAL ).
      _transf_error = abap_true.
      MESSAGE 'Existem registros sem grupo de conta de destino...' TYPE 'S'.
      CONTINUE.
    ELSE.
      _transf_execute = abap_true.
      lwa_zbkp_grp_conta-ktokk_dest   = lwa_saida_0100_03-ktokk_dest.
      lwa_zbkp_grp_conta-layout_dif   = lwa_saida_0100_03-layout_dif.
      lwa_zbkp_grp_conta-dt_registro  = sy-datum.
      lwa_zbkp_grp_conta-hr_registro  = sy-uzeit.
      lwa_zbkp_grp_conta-us_registro  = sy-uname.
      MODIFY zbkp_grp_conta FROM lwa_zbkp_grp_conta.

      UPDATE lfa1 SET ktokk = lwa_zbkp_grp_conta-ktokk_dest
       WHERE lifnr = lwa_saida_0100_03-lifnr.

    ENDIF.
  ENDLOOP.

  IF _transf_error EQ abap_false.
    CASE _transf_execute.
      WHEN abap_true.
        MESSAGE 'Transferencias realizadas!' TYPE 'S'.
      WHEN abap_false.
        MESSAGE 'Nenhuma Transferencia realizada!' TYPE 'I'.
    ENDCASE.
  ENDIF.

  PERFORM f_renew_consulta USING '0100'.

ENDFORM.

FORM f_exec_transf_ktokd_xd .

  DATA(_transf_execute) = abap_false.
  DATA(_transf_error)  = abap_false.
  LOOP AT it_saida_0100_03 INTO DATA(lwa_saida_0100_03).

    CHECK lwa_saida_0100_03-kunnr IS NOT INITIAL.

    SELECT SINGLE *
      FROM zbkp_grp_conta INTO @DATA(lwa_zbkp_grp_conta)
     WHERE kunnr EQ @lwa_saida_0100_03-kunnr.

    IF NOT ( sy-subrc EQ 0 AND lwa_zbkp_grp_conta-ktokd IS NOT INITIAL ) .
      _transf_error = abap_true.
      MESSAGE 'Existem registros sem backup...' TYPE 'S'.
      CONTINUE.
    ENDIF.

    IF lwa_zbkp_grp_conta-ktokd_dest IS NOT INITIAL.
      CONTINUE. "Transferencia já feita...
    ENDIF.

    IF ( lwa_saida_0100_03-ktokd_dest IS INITIAL ).
      _transf_error = abap_true.
      MESSAGE 'Existem registros sem grupo de conta de destino...' TYPE 'S'.
      CONTINUE.
    ELSE.
      _transf_execute = abap_true.
      lwa_zbkp_grp_conta-ktokd_dest   = lwa_saida_0100_03-ktokd_dest.
      lwa_zbkp_grp_conta-layout_dif   = lwa_saida_0100_03-layout_dif.
      lwa_zbkp_grp_conta-dt_registro  = sy-datum.
      lwa_zbkp_grp_conta-hr_registro  = sy-uzeit.
      lwa_zbkp_grp_conta-us_registro  = sy-uname.
      MODIFY zbkp_grp_conta FROM lwa_zbkp_grp_conta.

      UPDATE kna1 SET ktokd = lwa_zbkp_grp_conta-ktokd_dest
       WHERE kunnr = lwa_saida_0100_03-kunnr.

    ENDIF.
  ENDLOOP.

  IF _transf_error EQ abap_false.
    CASE _transf_execute.
      WHEN abap_true.
        MESSAGE 'Transferencias realizadas!' TYPE 'S'.
      WHEN abap_false.
        MESSAGE 'Nenhuma Transferencia realizada!' TYPE 'I'.
    ENDCASE.
  ENDIF.

  PERFORM f_renew_consulta USING '0100'.

ENDFORM.

FORM f_exec_restore_ktokk_xk .

  DATA(_transf_execute) = abap_false.
  DATA(_transf_error)  = abap_false.
  LOOP AT it_saida_0100_03 INTO DATA(lwa_saida_0100_03).

    CHECK lwa_saida_0100_03-lifnr IS NOT INITIAL.

    SELECT SINGLE *
      FROM zbkp_grp_conta INTO @DATA(lwa_zbkp_grp_conta)
     WHERE lifnr EQ @lwa_saida_0100_03-lifnr.

    IF NOT ( sy-subrc EQ 0 AND lwa_zbkp_grp_conta-ktokk IS NOT INITIAL  AND lwa_zbkp_grp_conta-ktokk_dest IS NOT INITIAL ).
      _transf_error = abap_true.
      MESSAGE 'Existem registros sem transferencia realizada...' TYPE 'S'.
      CONTINUE.
    ENDIF.

    IF lwa_zbkp_grp_conta-restore IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    _transf_execute = abap_true.
    lwa_zbkp_grp_conta-restore      = abap_true.
    lwa_zbkp_grp_conta-dt_registro  = sy-datum.
    lwa_zbkp_grp_conta-hr_registro  = sy-uzeit.
    lwa_zbkp_grp_conta-us_registro  = sy-uname.
    MODIFY zbkp_grp_conta FROM lwa_zbkp_grp_conta.

    UPDATE lfa1 SET ktokk = lwa_zbkp_grp_conta-ktokk
     WHERE lifnr = lwa_saida_0100_03-lifnr.

  ENDLOOP.

  IF _transf_error EQ abap_false.
    CASE _transf_execute.
      WHEN abap_true.
        MESSAGE 'Restore realizado!' TYPE 'S'.
      WHEN abap_false.
        MESSAGE 'Nenhum Restore realizado!' TYPE 'I'.
    ENDCASE.
  ENDIF.

  PERFORM f_renew_consulta USING '0100'.

ENDFORM.

FORM f_exec_restore_ktokd_xd .

  DATA(_transf_execute) = abap_false.
  DATA(_transf_error)  = abap_false.
  LOOP AT it_saida_0100_03 INTO DATA(lwa_saida_0100_03).

    CHECK lwa_saida_0100_03-kunnr IS NOT INITIAL.

    SELECT SINGLE *
      FROM zbkp_grp_conta INTO @DATA(lwa_zbkp_grp_conta)
     WHERE kunnr EQ @lwa_saida_0100_03-kunnr.

    IF NOT ( sy-subrc EQ 0 AND lwa_zbkp_grp_conta-ktokd IS NOT INITIAL  AND lwa_zbkp_grp_conta-ktokd_dest IS NOT INITIAL ).
      _transf_error = abap_true.
      MESSAGE 'Existem registros sem transferencia realizada...' TYPE 'S'.
      CONTINUE.
    ENDIF.

    IF lwa_zbkp_grp_conta-restore IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    _transf_execute = abap_true.
    lwa_zbkp_grp_conta-restore      = abap_true.
    lwa_zbkp_grp_conta-dt_registro  = sy-datum.
    lwa_zbkp_grp_conta-hr_registro  = sy-uzeit.
    lwa_zbkp_grp_conta-us_registro  = sy-uname.
    MODIFY zbkp_grp_conta FROM lwa_zbkp_grp_conta.

    UPDATE kna1 SET ktokd = lwa_zbkp_grp_conta-ktokd
     WHERE kunnr = lwa_saida_0100_03-kunnr.

  ENDLOOP.

  IF _transf_error EQ abap_false.
    CASE _transf_execute.
      WHEN abap_true.
        MESSAGE 'Restore realizado!' TYPE 'S'.
      WHEN abap_false.
        MESSAGE 'Nenhum Restore realizado!' TYPE 'I'.
    ENDCASE.
  ENDIF.

  PERFORM f_renew_consulta USING '0100'.

ENDFORM.

FORM f_xk_xd_equalize_stcd4 .

  CHECK p_lifnr[] IS NOT INITIAL.


  SELECT *
    FROM lfa1 INTO TABLE @DATA(lit_lfa1)
   WHERE lifnr IN @p_lifnr.

  DELETE lit_lfa1 WHERE stcd4 IS NOT INITIAL.
  DELETE lit_lfa1 WHERE kunnr IS INITIAL.

  CHECK lit_lfa1[] IS NOT INITIAL.

  SELECT *
    FROM kna1 INTO TABLE @DATA(lit_kna1)
    FOR ALL ENTRIES IN @lit_lfa1
   WHERE kunnr = @lit_lfa1-kunnr.

  DELETE lit_kna1 WHERE stcd4 IS INITIAL.

  LOOP AT lit_lfa1 INTO DATA(lwa_lfa1).
    READ TABLE lit_kna1 INTO DATA(lwa_kna1) WITH KEY  kunnr = lwa_lfa1-kunnr.

    CHECK sy-subrc EQ 0
      AND lwa_lfa1-kunnr IS NOT INITIAL
      AND lwa_kna1-stcd4 IS NOT INITIAL
      AND lwa_lfa1-stcd4 IS INITIAL.

    lwa_lfa1-stcd4 = lwa_kna1-stcd4.

    MODIFY lfa1 FROM lwa_lfa1.

  ENDLOOP.

  MESSAGE 'Equalização ID Fiscal 4 Concluida!' TYPE 'S'.

ENDFORM.

FORM f_fix_xk_xd_forma_tratamento.

  PERFORM f_fix_xk_xd_forma_trat_01. "Caracteristica PF e PJ
  PERFORM f_fix_xk_xd_forma_trat_02. "Forma tratamento não previstas
  PERFORM f_fix_xk_xd_forma_trat_03. "Incompatibilidade sexo com forma de tratamento  - XK

ENDFORM.

FORM f_fix_xk_xd_forma_trat_01 .

  RANGES: lra_anred_empresa   FOR kna1-anred.

  APPEND VALUE #( sign = 'I' option = 'EQ'  low =  space )    TO lra_anred_empresa.
  APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'Empresa' ) TO lra_anred_empresa.
  APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'Company' ) TO lra_anred_empresa.

*------------------------------------------------------------------------------------*
*  Corrigir forma de tratamento Cliente
*------------------------------------------------------------------------------------*

  SELECT *
     FROM kna1 INTO TABLE @DATA(lit_kna1)
    WHERE
           "Caracteristica PJ
          ( stcd1 <> ' ' OR
            anred IN @lra_anred_empresa OR
            stkzn =  @abap_false )

          AND

         "Caracteristica PF
         ( stcd2 <> ' ' OR
            anred NOT IN @lra_anred_empresa OR
            stkzn = @abap_true ).

  LOOP AT lit_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>).

    IF <fs_kna1>-stkzn EQ abap_true AND
       <fs_kna1>-stcd2 NE space AND
       <fs_kna1>-stcd1 EQ space AND
       <fs_kna1>-anred IN lra_anred_empresa.

      <fs_kna1>-anred = 'Senhor'.

      MODIFY kna1 FROM <fs_kna1>.

    ELSEIF <fs_kna1>-stkzn EQ abap_false AND
           <fs_kna1>-stcd2 EQ space AND
           <fs_kna1>-stcd1 NE space AND
           <fs_kna1>-anred NOT IN lra_anred_empresa.

      <fs_kna1>-anred = 'Empresa'.

      MODIFY kna1 FROM <fs_kna1>.

    ELSEIF <fs_kna1>-stkzn EQ abap_false AND
           <fs_kna1>-stcd2 EQ space AND
           <fs_kna1>-stcd1 EQ space AND
           <fs_kna1>-anred NOT IN lra_anred_empresa.

      <fs_kna1>-anred = 'Empresa'.
      MODIFY kna1 FROM <fs_kna1>.

    ENDIF.

  ENDLOOP.

  "Fornecedor

  SELECT *
     FROM lfa1 INTO TABLE @DATA(lit_lfa1)
    WHERE
        "Caracteristica PJ
       ( stcd1 <> ' ' OR
         anred IN @lra_anred_empresa OR
         stkzn =  @abap_false )

       AND

      "Caracteristica PF
      ( stcd2 <> ' ' OR
         anred NOT IN @lra_anred_empresa OR
         stkzn = @abap_true ).

  LOOP AT lit_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>).

    IF <fs_lfa1>-stkzn EQ abap_true AND
       <fs_lfa1>-stcd2 NE space AND
       <fs_lfa1>-stcd1 EQ space AND
       <fs_lfa1>-anred IN lra_anred_empresa.

      CASE <fs_lfa1>-sexkz.
        WHEN '1'. "Masculino
          <fs_lfa1>-anred = 'Senhor'.
        WHEN '2'. "Feminino
          <fs_lfa1>-anred = 'Senhora'.
        WHEN space. "Nao informado
          <fs_lfa1>-sexkz = '1'.
          <fs_lfa1>-anred = 'Senhor'.
      ENDCASE.

      MODIFY lfa1 FROM <fs_lfa1>.

    ELSEIF <fs_lfa1>-stkzn EQ abap_false AND
           <fs_lfa1>-stcd2 EQ space AND
           <fs_lfa1>-stcd1 NE space AND
           <fs_lfa1>-anred NOT IN lra_anred_empresa.

      <fs_lfa1>-anred = 'Empresa'.

      MODIFY lfa1 FROM <fs_lfa1>.

    ELSEIF <fs_lfa1>-stkzn EQ abap_false AND
           <fs_lfa1>-stcd2 EQ space AND
           <fs_lfa1>-stcd1 EQ space AND
           <fs_lfa1>-anred NOT IN lra_anred_empresa.

      <fs_lfa1>-anred = 'Empresa'.

      MODIFY lfa1 FROM <fs_lfa1>.

    ENDIF.

  ENDLOOP.

ENDFORM.

FORM f_fix_xk_xd_forma_trat_02 .

  RANGES: lra_anred_previstos FOR kna1-anred.

  APPEND VALUE #( sign = 'I' option = 'EQ'  low = space )     TO lra_anred_previstos.
  APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'Empresa' ) TO lra_anred_previstos.
  APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'Company' ) TO lra_anred_previstos.

  APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'Senhora' ) TO lra_anred_previstos.
  APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'Seora' )   TO lra_anred_previstos.
  APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'Ms' )      TO lra_anred_previstos.

  APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'Senhor' )  TO lra_anred_previstos.
  APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'Seor' )    TO lra_anred_previstos.
  APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'Mr' )      TO lra_anred_previstos.


*------------------------------------------------------------------------------------*
*  Corrigir forma de tratamento Cliente
*------------------------------------------------------------------------------------*

  SELECT *
    FROM kna1 INTO TABLE @DATA(lit_kna1)
   WHERE anred NOT IN @lra_anred_previstos.

  LOOP AT lit_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>).

    CASE <fs_kna1>-stkzn.
        WHEN abap_true.
        <fs_kna1>-anred = 'Senhor'.
        WHEN abap_false.
        <fs_kna1>-anred = 'Empresa'.
    ENDCASE.

    MODIFY kna1 FROM <fs_kna1>.

  ENDLOOP.

  "Fornecedor
  SELECT *
    FROM lfa1 INTO TABLE @DATA(lit_lfa1)
   WHERE anred NOT IN @lra_anred_previstos.

  LOOP AT lit_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>).

    CASE <fs_lfa1>-stkzn.
        WHEN abap_true.
        CASE <fs_lfa1>-sexkz.
          WHEN '1'. "Masculino
            <fs_lfa1>-anred = 'Senhor'.
          WHEN '2'. "Feminino
            <fs_lfa1>-anred = 'Senhora'.
          WHEN space. "Nao informado
            <fs_lfa1>-sexkz = '1'.
            <fs_lfa1>-anred = 'Senhor'.
        ENDCASE.
        WHEN abap_false.
        <fs_lfa1>-anred = 'Empresa'.
    ENDCASE.

    MODIFY lfa1 FROM <fs_lfa1>.

  ENDLOOP.


ENDFORM.


FORM f_fix_xk_xd_forma_trat_03 .

  RANGES: lra_anred_masculino FOR kna1-anred.
  RANGES: lra_anred_feminino  FOR kna1-anred.


  APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'Senhora' ) TO lra_anred_feminino.
  APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'Seora' )   TO lra_anred_feminino.
  APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'Ms' )      TO lra_anred_feminino.

  APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'Senhor' )  TO lra_anred_masculino.
  APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'Seor' )    TO lra_anred_masculino.
  APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'Mr' )      TO lra_anred_masculino.


*------------------------------------------------------------------------------------*
*  Corrigir forma de tratamento Fornecedor
*------------------------------------------------------------------------------------*
  SELECT *
    FROM lfa1 INTO TABLE @DATA(lit_lfa1)
   WHERE sexkz = '1'  "Masculino
     AND anred NOT IN @lra_anred_masculino.

  LOOP AT lit_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>).
    CASE <fs_lfa1>-stkzn.
      WHEN abap_true.
        <fs_lfa1>-anred = 'Senhor'.
        MODIFY lfa1 FROM <fs_lfa1>.
      WHEN abap_false.
        <fs_lfa1>-sexkz = space.
        MODIFY lfa1 FROM <fs_lfa1>.
    ENDCASE.
  ENDLOOP.

  CLEAR: lit_lfa1[].
  SELECT *
    FROM lfa1 INTO TABLE lit_lfa1
   WHERE sexkz = '2'  "Feminino
     AND anred NOT IN lra_anred_feminino.

  LOOP AT lit_lfa1 ASSIGNING <fs_lfa1>.
    CASE <fs_lfa1>-stkzn.
      WHEN abap_true.
        <fs_lfa1>-anred = 'Senhora'.
        MODIFY lfa1 FROM <fs_lfa1>.
      WHEN abap_false.
        <fs_lfa1>-sexkz = space.
        MODIFY lfa1 FROM <fs_lfa1>.
    ENDCASE.
  ENDLOOP.

ENDFORM.

FORM f_fill_xk_xd_setor_ind.

  CONSTANTS: c_z999 TYPE c LENGTH 50 VALUE 'Z999'.

  "Replicar setor do cliente para o fornecedor
  SELECT *
    FROM kna1 INTO TABLE @DATA(lit_kna1).

  SELECT *
    FROM lfa1 INTO TABLE @DATA(lit_lfa1).

  SORT lit_lfa1 BY lifnr.
  SORT lit_kna1 BY kunnr.

  LOOP AT lit_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>).

    CHECK <fs_kna1>-brsch IS INITIAL.

    IF <fs_kna1>-lifnr IS INITIAL. "Sem vinculo com fornecedor.
      <fs_kna1>-brsch = c_z999.
      MODIFY kna1 FROM <fs_kna1>.
      COMMIT WORK AND WAIT.
      CONTINUE.
    ENDIF.

    READ TABLE lit_lfa1 INTO DATA(lwa_lfa1) WITH KEY lifnr = <fs_kna1>-lifnr BINARY SEARCH.
    IF sy-subrc EQ 0 AND lwa_lfa1-brsch IS NOT INITIAL . "Pegar Setor do Fornecedor vinculado
      <fs_kna1>-brsch = lwa_lfa1-brsch.
      MODIFY kna1 FROM <fs_kna1>.
      CONTINUE.
    ELSE.
      <fs_kna1>-brsch = c_z999.
      MODIFY kna1 FROM <fs_kna1>.
      COMMIT WORK AND WAIT.
      CONTINUE.
    ENDIF.

  ENDLOOP.

  LOOP AT lit_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>).

    CHECK <fs_lfa1>-brsch IS INITIAL.

    IF <fs_lfa1>-kunnr IS INITIAL. "Sem vinculo com cliente.
      <fs_lfa1>-brsch = c_z999.
      MODIFY lfa1 FROM <fs_lfa1>.
      COMMIT WORK AND WAIT.
      CONTINUE.
    ENDIF.

    READ TABLE lit_kna1 INTO DATA(lwa_kna1) WITH KEY kunnr = <fs_lfa1>-kunnr BINARY SEARCH.
    IF sy-subrc EQ 0 AND lwa_kna1-brsch IS NOT INITIAL . "Pegar Setor do Cliente vinculado
      <fs_lfa1>-brsch = lwa_kna1-brsch.
      MODIFY lfa1 FROM <fs_lfa1>.
      CONTINUE.
    ELSE.
      <fs_lfa1>-brsch = c_z999.
      MODIFY lfa1 FROM <fs_lfa1>.
      COMMIT WORK AND WAIT.
      CONTINUE.
    ENDIF.

  ENDLOOP.





ENDFORM.


FORM f_fix_xk_xd_lzone.

  CONSTANTS: c_z999 TYPE c LENGTH 50 VALUE 'Z999'.

  "Replicar setor do cliente para o fornecedor
  SELECT *
    FROM kna1 INTO TABLE @DATA(lit_kna1).

  SELECT *
    FROM lfa1 INTO TABLE @DATA(lit_lfa1).

  SORT lit_lfa1 BY lifnr.
  SORT lit_kna1 BY kunnr.

  LOOP AT lit_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>) WHERE lifnr IS NOT INITIAL.

    CHECK <fs_kna1>-lzone IS INITIAL.

    READ TABLE lit_lfa1 INTO DATA(lwa_lfa1) WITH KEY lifnr = <fs_kna1>-lifnr BINARY SEARCH.
    IF sy-subrc EQ 0 AND lwa_lfa1-lzone IS NOT INITIAL . "Pegar Zona do Fornecedor vinculado
      <fs_kna1>-lzone = lwa_lfa1-lzone.
      MODIFY kna1 FROM <fs_kna1>.
      COMMIT WORK AND WAIT.
      CONTINUE.
    ENDIF.

  ENDLOOP.

  LOOP AT lit_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>) WHERE kunnr IS NOT INITIAL.

    READ TABLE lit_kna1 INTO DATA(lwa_kna1) WITH KEY kunnr = <fs_lfa1>-kunnr BINARY SEARCH.
    CHECK sy-subrc EQ 0 AND lwa_kna1-lzone IS NOT INITIAL.

    IF <fs_lfa1>-lzone <> lwa_kna1-lzone. "Pegar Zona do Cliente vinculado
      <fs_lfa1>-lzone = lwa_kna1-lzone.
      MODIFY lfa1 FROM <fs_lfa1>.
      COMMIT WORK AND WAIT.
      CONTINUE.
    ENDIF.

  ENDLOOP.





ENDFORM.

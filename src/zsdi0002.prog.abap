*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDI0002                                                *
* Descrição  : Controle Memorando para Terceiros                       *
* Módulo     : SD                                Transação: ZSDT0002   *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 28/07/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT zsdi0002 NO STANDARD PAGE HEADING MESSAGE-ID sd.

TYPE-POOLS icon.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_nfs,
         nfenum  TYPE j_1bnfnum9,
         bukrs   TYPE bukrs,
         werks   TYPE werks_d,
         lgort   TYPE lgort_d,
         matnr   TYPE matnr,
         pstdat  TYPE j_1bpstdat,
         vendat  TYPE zvendat,
         vbeln   TYPE vbeln,
         kunnr   TYPE kunnr,
         menge   TYPE j_1bnetqty,
         fkart   TYPE fkart,
         cfop    TYPE j_1bcfop,
         nfnet   TYPE j_1bnfnet,
         docnum  TYPE j_1bdocnum,
         memo    TYPE zmemo,
         saldo   TYPE zsaldo,
         qtd     TYPE j_1bnetqty,
         saldo_a TYPE j_1bnetqty,
         marc    TYPE char1,
       END   OF type_nfs,

       BEGIN OF type_vbrk,
         vbeln  TYPE vbrk-vbeln      ,
         fkart  TYPE vbrk-fkart      ,
         kunrg  TYPE vbrk-kunrg      ,
         xblnr  TYPE vbrk-xblnr      ,
         refkey TYPE j_1bnflin-refkey,
       END   OF type_vbrk,

       BEGIN OF type_vbrp,
         vbeln TYPE vbrp-vbeln,
         posnr TYPE vbrp-posnr,
         fkimg TYPE vbrp-fkimg,
         matnr TYPE vbrp-matnr,
         werks TYPE vbrp-werks,
         lgort TYPE vbrp-lgort,
         netwr TYPE vbrp-netwr,
         aubel TYPE vbrp-aubel,
       END   OF type_vbrp,

       BEGIN OF type_lin,
         docnum TYPE j_1bnflin-docnum,
         itmnum TYPE j_1bnflin-itmnum,
         matnr  TYPE j_1bnflin-matnr,
         bwkey  TYPE j_1bnflin-bwkey,
         matkl  TYPE j_1bnflin-matkl,
         nbm    TYPE j_1bnflin-nbm,
         cfop   TYPE j_1bnflin-cfop,
         taxsit TYPE j_1bnflin-taxsit,
         taxsi2 TYPE j_1bnflin-taxsi2,
         matuse TYPE j_1bnflin-matuse,
         refkey TYPE j_1bnflin-refkey,
         menge  TYPE j_1bnflin-menge,
         meins  TYPE j_1bnflin-meins,
         netpr  TYPE j_1bnflin-netpr,
         netwr  TYPE j_1bnflin-netwr,
         netwrt TYPE j_1bnflin-netwrt,
         taxlw1 TYPE j_1bnflin-taxlw1,
         taxlw2 TYPE j_1bnflin-taxlw2,
         itmtyp TYPE j_1bnflin-itmtyp,
         werks  TYPE j_1bnflin-werks,
         taxlw3 TYPE j_1bnflin-taxlw3,
         taxlw4 TYPE j_1bnflin-taxlw4,
       END   OF type_lin,

       BEGIN OF type_doc,
         docnum TYPE j_1bnfdoc-docnum,
         credat TYPE j_1bnfdoc-credat,
         nfenum TYPE j_1bnfdoc-nfenum,
         bukrs  TYPE j_1bnfdoc-bukrs ,
         branch TYPE j_1bnfdoc-branch,
         kunnr  TYPE j_1bnfdoc-parid ,
       END   OF type_doc,

       BEGIN OF type_memo,
         num_memo  TYPE zmemor    ,
         data      TYPE j_1bcredat,
         pais_dest TYPE land1     ,
         desp_dde  TYPE zdesp_dde ,
         data_dde  TYPE zdata_dde ,
         emb_bl    TYPE zemb_bl   ,
         data_bl   TYPE zdata_bl  ,
         reg_re    TYPE zreg_re   ,
         data_re   TYPE zdata_re  ,
         nf_exp    TYPE znf_exp   ,
       END   OF type_memo,

       BEGIN OF type_text,
         line(255) TYPE c,
       END OF type_text,

       BEGIN OF type_vbak,
         vbeln TYPE vbak-vbeln,
         auart TYPE vbak-auart,
       END   OF type_vbak.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_nfs_m    TYPE TABLE OF type_nfs  ,
      t_fcat     TYPE TABLE OF lvc_s_fcat,
      t_vbrk     TYPE TABLE OF type_vbrk ,
      t_vbrp     TYPE TABLE OF type_vbrp ,
      t_lin      TYPE TABLE OF type_lin  ,
      t_doc      TYPE TABLE OF type_doc  ,
      t_nfs      TYPE TABLE OF type_nfs  ,
      t_tool     TYPE ui_functions       ,
      t_memo     TYPE TABLE OF type_nfs  ,
      t_text     TYPE TABLE OF type_text ,
      t_zsdt0004 TYPE TABLE OF zsdt0004  ,
      t_zsdt0005 TYPE TABLE OF zsdt0005  ,
      t_vbak     TYPE TABLE OF type_vbak .

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*
DATA: v_index                   TYPE syindex                       ,
      v_editor_itxt             TYPE REF TO cl_gui_textedit        ,
      v_textedit_container_itxt TYPE REF TO cl_gui_custom_container,
      v_ok                      TYPE char1                         .
CONTROLS tc_memo TYPE TABLEVIEW USING SCREEN '0200'.

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*
DATA: s_cont   TYPE REF TO cl_gui_custom_container,
      s_alv    TYPE REF TO cl_gui_alv_grid        ,
      s_layout TYPE lvc_s_layo                    ,
      s_nfs    TYPE type_nfs                      ,
      s_memo   TYPE type_memo                     .

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*
CONSTANTS: c_table TYPE char5 VALUE 'T_NFS' ,
           c_memo  TYPE char6 VALUE 'S_MEMO',
           c_x     TYPE char1 VALUE 'X'     .

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.
PARAMETERS:
  p_bukrs TYPE t001-bukrs  OBLIGATORY,
  p_fkart TYPE vbrk-fkart            ,
  p_werks TYPE t001w-werks           ,
  p_lgort TYPE t001l-lgort           ,
  p_matnr TYPE mara-matnr            ,
  p_kunnr TYPE kna1-kunnr            .
SELECTION-SCREEN END   OF BLOCK a2.
*SELECTION-SCREEN BEGIN OF BLOCK a3 WITH FRAME.
*PARAMETERS:
*  p_todos RADIOBUTTON GROUP g01 DEFAULT 'X',
*  p_venc  RADIOBUTTON GROUP g01            ,
*  p_avenc RADIOBUTTON GROUP g01            .
*SELECTION-SCREEN END   OF BLOCK a3.
SELECTION-SCREEN END   OF BLOCK a1.

*----------------------------------------------------------------------*
*                         AT SELECTION SCREEN                          *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON p_bukrs.
* Válida Empresa
  PERFORM z_valida_emp.

AT SELECTION-SCREEN ON p_werks.
* Válida Centro
  PERFORM z_valida_cen.

AT SELECTION-SCREEN ON p_lgort.
* Válida Depósito
  PERFORM z_valida_dep.

AT SELECTION-SCREEN ON p_kunnr.
* Válida Cliente
  PERFORM z_valida_cli.

AT SELECTION-SCREEN ON p_matnr.
* Válida Material
  PERFORM z_valida_mat.

*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Lógica Processamento
  PERFORM z_inicia_proc.

**----------------------------------------------------------------------*
**                               Classes                                *
**----------------------------------------------------------------------*
  CLASS lcl_event_receiver DEFINITION DEFERRED.
  DATA s_event TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION                            *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
       zm_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
         IMPORTING
           e_object e_interactive,

       zm_handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
         IMPORTING e_ucomm.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION                        *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD zm_handle_toolbar.
*   Incluindo Botão ALV
    PERFORM z_handle_toolbar USING e_object
                                   e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command.
*   User Command Botões Incluidos
    PERFORM z_handle_command USING e_ucomm.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  Z_VALIDA_EMP                                             *
*&---------------------------------------------------------------------*
*                             Válida Empresa                           *
*----------------------------------------------------------------------*
FORM z_valida_emp.

  CHECK NOT p_bukrs IS INITIAL.

  SELECT SINGLE bukrs
    FROM t001
    INTO p_bukrs
  WHERE  bukrs EQ p_bukrs.

  CHECK NOT sy-subrc IS INITIAL.
  MESSAGE e836 WITH text-002.

ENDFORM.                    " Z_VALIDA_EMP

*&---------------------------------------------------------------------*
*&      Form  Z_VALIDA_CEN                                             *
*&---------------------------------------------------------------------*
*                              Válida Centro                           *
*----------------------------------------------------------------------*
FORM z_valida_cen.

  CHECK NOT p_werks IS INITIAL.

  SELECT SINGLE werks
    FROM t001w
    INTO p_werks
  WHERE  werks EQ p_werks.

  CHECK NOT sy-subrc IS INITIAL.
  MESSAGE e836 WITH text-004.

ENDFORM.                    " Z_VALIDA_CEN

*&---------------------------------------------------------------------*
*&      Form  Z_VALIDA_DEP                                             *
*&---------------------------------------------------------------------*
*                           Válida Depósito                            *
*----------------------------------------------------------------------*
FORM z_valida_dep.

  CHECK NOT p_lgort IS INITIAL.

  SELECT SINGLE lgort
    FROM t001l
    INTO p_lgort
  WHERE  werks EQ p_werks
    AND  lgort EQ p_lgort.

  CHECK NOT sy-subrc IS INITIAL.
  MESSAGE e836 WITH text-005 p_werks.

ENDFORM.                    " Z_VALIDA_DEP

*&---------------------------------------------------------------------*
*&      Form  Z_VALIDA_CLI                                             *
*&---------------------------------------------------------------------*
*                             Válida Cliente                           *
*----------------------------------------------------------------------*
FORM z_valida_cli.

  CHECK NOT p_kunnr IS INITIAL.

  SELECT SINGLE kunnr
    FROM kna1
    INTO p_kunnr
  WHERE  kunnr EQ p_kunnr.

  CHECK NOT sy-subrc IS INITIAL.
  MESSAGE e836 WITH text-006.

ENDFORM.                    " Z_VALIDA_CLI

*&---------------------------------------------------------------------*
*&      Form  Z_VALIDA_MAT                                             *
*&---------------------------------------------------------------------*
*                           Válida Material                            *
*----------------------------------------------------------------------*
FORM z_valida_mat.

  CHECK NOT p_matnr IS INITIAL.

  SELECT SINGLE matnr
    FROM mara
    INTO p_matnr
  WHERE  matnr EQ p_matnr.

  CHECK NOT sy-subrc IS INITIAL.
  MESSAGE e836 WITH text-008.

ENDFORM.                    " Z_VALIDA_MAT

*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS  OUTPUT                                      *
*&---------------------------------------------------------------------*
*                                  Status                              *
*----------------------------------------------------------------------*
MODULE zm_status OUTPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      SET PF-STATUS 'PF0100'.
    WHEN '0200'.
      SET PF-STATUS 'PF0200'.
      DESCRIBE TABLE t_nfs_m LINES tc_memo-lines.
  ENDCASE.

ENDMODULE.                 " ZM_STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              User Command                            *
*----------------------------------------------------------------------*
MODULE zm_user_command INPUT.

  CASE sy-ucomm.
    WHEN 'BT_DEL'.
*     Deleta Linhas
      PERFORM z_del.
    WHEN 'BT_ALL'.
*     Marca Todas as Linhas
      PERFORM z_marc_all.
    WHEN 'BT_NONE'.
*     Desmarca as Linhas
      PERFORM z_marc_none.
    WHEN 'BT_SAVE'.
*     Salva Memorando
      PERFORM z_save_memo.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_exit_command INPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      CASE sy-ucomm.
        WHEN 'BACK' OR
             'CANC' OR
             'EXIT'.
          LEAVE TO SCREEN 0.
      ENDCASE.
    WHEN '0200'.
      CASE sy-ucomm.
        WHEN 'BACK' OR
             'CANC' OR
             'EXIT'.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_EXIT_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR                                         *
*&---------------------------------------------------------------------*
*                           Incluindo Botão ALV                        *
*----------------------------------------------------------------------*
FORM z_handle_toolbar USING p_object      TYPE REF TO cl_alv_event_toolbar_set
                            p_interactive TYPE char1.

* Constants for button type
  CONSTANTS:
        c_button_normal           TYPE i VALUE 0,
        c_menu_and_default_button TYPE i VALUE 1,
        c_menu                    TYPE i VALUE 2,
        c_separator               TYPE i VALUE 3,
        c_radio_button            TYPE i VALUE 4,
        c_checkbox                TYPE i VALUE 5,
        c_menu_entry              TYPE i VALUE 6.

  DATA sl_toolbar TYPE stb_button.

* Append Seperator
  MOVE c_separator  TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO p_object->mt_toolbar.

* Botão Vincular NF's
  CLEAR sl_toolbar.
  MOVE: 'VINCNF'           TO sl_toolbar-function ,
         icon_copy_object  TO sl_toolbar-icon     ,
         text-003          TO sl_toolbar-quickinfo,
         text-003          TO sl_toolbar-text     ,
         space             TO sl_toolbar-disabled .
  APPEND sl_toolbar TO p_object->mt_toolbar.

ENDFORM.                    " Z_HANDLE_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND                                         *
*&---------------------------------------------------------------------*
*                      User Command Botões Incluidos                   *
*----------------------------------------------------------------------*
FORM z_handle_command USING p_ucomm TYPE syucomm.

  CASE p_ucomm.
    WHEN 'VINCNF'.
*     Vincular NF's
      PERFORM z_vincular_nf.
  ENDCASE.

ENDFORM.                    " Z_HANDLE_COMMAND

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                           Seleção dos Dados                          *
*----------------------------------------------------------------------*
FORM z_seleciona_dados.

* Seleciona VBRK
  PERFORM: z_seleciona_vbrk,

* Seleciona VBRP
           z_seleciona_vbrp,

* Seleciona J_1BNFLIN
           z_seleciona_lin ,

* Seleciona J_1BNFDOC
           z_seleciona_doc ,

* Seleciona ZSDT0004
           z_seleciona_004 ,

* Seleciona ZSDT005
           z_seleciona_005 ,

* Seleciona VBAK
           z_seleciona_vbak.

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBRK                                         *
*&---------------------------------------------------------------------*
*                             Seleciona VBRK                           *
*----------------------------------------------------------------------*
FORM z_seleciona_vbrk.

  DATA sl_vbrk TYPE type_vbrk.

  REFRESH t_vbrk.

  SELECT vbeln fkart kunrg xblnr
    FROM vbrk
    INTO TABLE t_vbrk
  WHERE  bukrs EQ p_bukrs AND DRAFT = SPACE .

  SORT t_vbrk BY vbeln ASCENDING.

  IF NOT p_fkart IS INITIAL.
    DELETE t_vbrk WHERE fkart NE p_fkart.
  ENDIF.

  LOOP AT t_vbrk INTO sl_vbrk.
    v_index = sy-tabix.
    sl_vbrk-refkey = sl_vbrk-vbeln.
    MODIFY t_vbrk FROM sl_vbrk
      INDEX v_index
      TRANSPORTING refkey.
    CLEAR sl_vbrk.
  ENDLOOP.

  IF t_vbrk[] IS INITIAL.
    MESSAGE i836 WITH text-009.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_VBRK

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBRP                                         *
*&---------------------------------------------------------------------*
*                              Seleciona VBRP                          *
*----------------------------------------------------------------------*
FORM z_seleciona_vbrp.

  REFRESH t_vbrp.

  CHECK NOT t_vbrk[] IS INITIAL.

  SELECT vbeln posnr fkimg
         matnr werks lgort netwr
         aubel
    FROM vbrp
    INTO TABLE t_vbrp
    FOR ALL ENTRIES IN t_vbrk
  WHERE  vbeln EQ t_vbrk-vbeln.

  SORT t_vbrp BY vbeln ASCENDING
                 posnr ASCENDING.

  IF NOT p_werks IS INITIAL.
    DELETE t_vbrp WHERE werks NE p_werks.
  ENDIF.

  IF NOT p_matnr IS INITIAL.
    DELETE t_vbrp WHERE matnr NE p_matnr.
  ENDIF.

  IF NOT p_lgort IS INITIAL.
    DELETE t_vbrp WHERE lgort NE p_lgort.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_VBRP

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIN                                          *
*&---------------------------------------------------------------------*
*                        Seleciona J_1BNFLIN                           *
*----------------------------------------------------------------------*
FORM z_seleciona_lin.

  REFRESH t_lin.

  CHECK NOT t_vbrk[] IS INITIAL.

  SELECT docnum itmnum matnr
         bwkey  matkl  nbm cfop
         taxsit taxsi2 matuse
         refkey menge  meins
         netpr  netwr  netwrt
         taxlw1 taxlw2 itmtyp
         werks  taxlw3 taxlw4
    FROM j_1bnflin
    INTO TABLE t_lin
    FOR ALL ENTRIES IN t_vbrk
  WHERE  refkey EQ t_vbrk-refkey.

  SORT t_lin BY docnum ASCENDING
                itmnum ASCENDING.

  IF t_lin[] IS INITIAL.
    MESSAGE i836 WITH text-012.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_LIN

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DOC                                          *
*&---------------------------------------------------------------------*
*                           Seleciona J_1BNFDOC                        *
*----------------------------------------------------------------------*
FORM z_seleciona_doc.

  DATA tl_lin TYPE TABLE OF type_lin.

  REFRESH t_doc.

  tl_lin[] = t_lin[].
  SORT tl_lin BY docnum ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_lin COMPARING docnum.

  SELECT docnum credat nfenum bukrs
         branch parid
    FROM j_1bnfdoc
    INTO TABLE t_doc
    FOR ALL ENTRIES IN tl_lin
  WHERE  docnum EQ tl_lin-docnum.

  SORT t_doc BY credat ASCENDING
                docnum ASCENDING.

  IF NOT p_kunnr IS INITIAL.
    DELETE t_doc WHERE kunnr EQ p_kunnr.
  ENDIF.

  IF t_doc[] IS INITIAL.
    MESSAGE i836 WITH text-012.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_DOC

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_NFS                                              *
*&---------------------------------------------------------------------*
*                            Monta Dados NF's                          *
*----------------------------------------------------------------------*
FORM z_monta_nfs.

  DATA: sl_nfs   TYPE type_nfs ,
        sl_doc   TYPE type_doc ,
        sl_lin   TYPE type_lin ,
        sl_vbrk  TYPE type_vbrk,
        sl_vbrp  TYPE type_vbrp,
        sl_004   TYPE zsdt0004 ,
        sl_005   TYPE zsdt0005 ,
        sl_vbak  TYPE type_vbak,
        vl_quant TYPE menge_d  .
*---> 05/07/2023 - Migração S4 - DL
  SORT t_zsdt0005 BY cfop.
*<--- 05/07/2023 - Migração S4 - DL
  LOOP AT t_doc INTO sl_doc.

    LOOP AT t_lin INTO sl_lin
      WHERE docnum EQ sl_doc-docnum.

      CLEAR vl_quant.

      READ TABLE t_vbrk INTO sl_vbrk
        WITH KEY vbeln = sl_lin-refkey
        BINARY SEARCH.

      READ TABLE t_vbrp INTO sl_vbrp
        WITH KEY vbeln = sl_vbrk-vbeln
        BINARY SEARCH.

      IF NOT sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE t_vbak INTO sl_vbak
        WITH KEY vbeln = sl_vbrp-aubel
        BINARY SEARCH.

      READ TABLE t_zsdt0005 INTO sl_005
        WITH KEY cfop = sl_lin-cfop
        BINARY SEARCH.

      IF NOT sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      LOOP AT t_zsdt0004 INTO sl_004
        WHERE nfenum EQ sl_doc-nfenum.
        ADD sl_004-quanbai TO vl_quant.
        CLEAR sl_004.
      ENDLOOP.

      sl_nfs-lgort   = sl_vbrp-lgort.
      sl_nfs-vbeln   = sl_vbrp-aubel.
      sl_nfs-matnr   = sl_lin-matnr.
      sl_nfs-kunnr   = sl_doc-kunnr.
      sl_nfs-nfenum  = sl_doc-nfenum.
      sl_nfs-menge   = sl_lin-menge.
      sl_nfs-fkart   = sl_vbak-auart.
      sl_nfs-cfop    = sl_lin-cfop.
      sl_nfs-nfnet   = sl_lin-netwr.
      sl_nfs-docnum  = sl_doc-docnum.
      sl_nfs-pstdat  = sl_doc-credat.
      sl_nfs-bukrs   = p_bukrs.
      sl_nfs-werks   = sl_lin-werks.
      sl_nfs-saldo   = sl_lin-menge - vl_quant.
      sl_nfs-saldo_a = sl_nfs-saldo.
      IF sl_nfs-saldo LE 0.
        CONTINUE.
      ENDIF.

      APPEND sl_nfs TO t_nfs.

      CLEAR: sl_lin ,
             sl_nfs ,
             sl_vbrp,
             sl_vbrk,
             sl_vbak,
             sl_005 .

    ENDLOOP.

    CLEAR: sl_lin ,
           sl_doc ,
           sl_nfs ,
           sl_vbrp,
           sl_vbak,
           sl_vbrk.

  ENDLOOP.

ENDFORM.                    " Z_MONTA_NFS

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_FIELDCAT                                         *
*&---------------------------------------------------------------------*
*                           Monta FieldCat                             *
*----------------------------------------------------------------------*
FORM z_monta_fieldcat.

  REFRESH: t_fcat,
           t_tool.

  CHECK NOT t_nfs[] IS INITIAL.

* Preenche FieldCat
  PERFORM z_preenche_fieldcat USING:
    c_table 'LGORT'  text-015  8,
    c_table 'VBELN'  text-016 10,
    c_table 'MATNR'  text-017 18,
    c_table 'KUNNR'  text-018 10,
    c_table 'NFENUM' text-019  9,
    c_table 'PSTDAT' text-041 10,
    c_table 'MENGE'  text-020 18,
    c_table 'SALDO'  text-025 18,
    c_table 'FKART'  text-021 10,
    c_table 'CFOP'   text-022  8,
    c_table 'NFNET'  text-023 18.

* Monta Layout
  PERFORM z_layout.

* Deleta Botões
  PERFORM z_deleta_bot USING: '&LOCAL&APPEND'       ,
                              '&LOCAL&COPY'         ,
                              '&LOCAL&COPY_ROW'     ,
                              '&LOCAL&CUT'          ,
                              '&LOCAL&DELETE_ROW'   ,
                              '&LOCAL&INSERT_ROW'   ,
                              '&LOCAL&MOVE_ROW'     ,
                              '&LOCAL&PASTE'        ,
                              '&LOCAL&PASTE_NEW_ROW',
                              '&LOCAL&UNDO'         ,
                              '&CHECK'              .

ENDFORM.                    " Z_MONTA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_FIELDCAT                                      *
*&---------------------------------------------------------------------*
*                           Preenche FieldCat                          *
*----------------------------------------------------------------------*
FORM z_preenche_fieldcat USING p_table TYPE c
                               p_field TYPE c
                               p_desc  TYPE c
                               p_len   TYPE n.

  DATA sl_fcat TYPE lvc_s_fcat.

  sl_fcat-tabname   = p_table.
  sl_fcat-fieldname = p_field.
  sl_fcat-scrtext_l = p_desc.
  sl_fcat-scrtext_m = p_desc.
  sl_fcat-scrtext_s = p_desc.
  sl_fcat-outputlen = p_len.

  APPEND sl_fcat TO t_fcat.

ENDFORM.                    " Z_PREENCHE_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT                                                 *
*&---------------------------------------------------------------------*
*                            Monta Layout                              *
*----------------------------------------------------------------------*
FORM z_layout.

  CLEAR s_layout.

  s_layout-zebra = 'X'.

ENDFORM.                    " Z_LAYOUT

*&---------------------------------------------------------------------*
*&      Module  ZM_OBJ_ALV  OUTPUT                                     *
*&---------------------------------------------------------------------*
*                                 Obj Alv                              *
*----------------------------------------------------------------------*
MODULE zm_obj_alv OUTPUT.

* Instancia Container
  PERFORM: z_inst_cont ,
* Instancia Alv
           z_inst_alv  ,
* Instancia Eventos
           z_inst_event,
* Exibe Alv
           z_exibe_alv .

ENDMODULE.                 " ZM_OBJ_ALV  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_CONT                                              *
*&---------------------------------------------------------------------*
*                       Instancia Container                            *
*----------------------------------------------------------------------*
FORM z_inst_cont.

  CHECK s_cont IS INITIAL.

  CREATE OBJECT s_cont
    EXPORTING
      container_name              = 'CC_ALV'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE i836 WITH text-013.
  ENDIF.

ENDFORM.                    " Z_INST_CONT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_ALV                                               *
*&---------------------------------------------------------------------*
*                              Instancia Alv                           *
*----------------------------------------------------------------------*
FORM z_inst_alv.

  CHECK s_alv IS INITIAL.

  CREATE OBJECT s_alv
    EXPORTING
      i_parent          = s_cont
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE i836 WITH text-014.
  ENDIF.

ENDFORM.                    " Z_INST_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_INST_EVENT                                             *
*&---------------------------------------------------------------------*
*                           Instancia Eventos                          *
*----------------------------------------------------------------------*
FORM z_inst_event.

  CHECK s_event IS INITIAL.

  CREATE OBJECT s_event.
  SET HANDLER: s_event->zm_handle_user_command FOR s_alv,
               s_event->zm_handle_toolbar      FOR s_alv.

ENDFORM.                    " Z_INST_EVENT

*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_ALV                                              *
*&---------------------------------------------------------------------*
*                                Exibe Alv                             *
*----------------------------------------------------------------------*
FORM z_exibe_alv.

  DATA vl_int TYPE int4.

  CALL METHOD s_alv->set_table_for_first_display
    EXPORTING
*      i_save                        =
      i_default                     = 'X'
      is_layout                     = s_layout
      it_toolbar_excluding          = t_tool
    CHANGING
      it_outtab                     = t_nfs
      it_fieldcatalog               = t_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD s_alv->set_ready_for_input.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " Z_EXIBE_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_DELETA_BOT                                             *
*&---------------------------------------------------------------------*
*                             Deleta Botões                            *
*----------------------------------------------------------------------*
FORM z_deleta_bot USING p_bot TYPE c.

  DATA sl_tool TYPE ui_func.

  sl_tool = p_bot.
  APPEND sl_tool TO t_tool.

ENDFORM.                    " Z_DELETA_BOT

*&---------------------------------------------------------------------*
*&      Form  Z_VINCULAR_NF                                            *
*&---------------------------------------------------------------------*
*                            Vincular NF's                             *
*----------------------------------------------------------------------*
FORM z_vincular_nf.

  DATA: tl_rows TYPE lvc_t_row,
        sl_rows TYPE lvc_s_row,
        sl_nfs  TYPE type_nfs .

  REFRESH t_nfs_m.

* Verifica Seleção de Linhas
  CALL METHOD s_alv->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF tl_rows[] IS INITIAL.
    MESSAGE i836 WITH text-026.
    EXIT.
  ENDIF.

  LOOP AT tl_rows INTO sl_rows.
    READ TABLE t_nfs INTO sl_nfs
      INDEX sl_rows-index.
    APPEND sl_nfs TO t_nfs_m.
    CLEAR: sl_rows,
           sl_nfs .
  ENDLOOP.

  CALL SCREEN 0200.

ENDFORM.                    " Z_VINCULAR_NF

*&---------------------------------------------------------------------*
*&      Module  ZM_TEXT  OUTPUT                                        *
*&---------------------------------------------------------------------*
*                              Campo Texto                             *
*----------------------------------------------------------------------*
MODULE zm_text OUTPUT.

* Instancia Container
  PERFORM: z_instancia_cont,

* Instancia Obj text
           z_instancia_text.

ENDMODULE.                 " ZM_TEXT  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_INSTANCIA_CONT                                         *
*&---------------------------------------------------------------------*
*                           Instancia Container                        *
*----------------------------------------------------------------------*
FORM z_instancia_cont.

  CHECK v_textedit_container_itxt IS INITIAL.

  CREATE OBJECT v_textedit_container_itxt
    EXPORTING
      container_name              = 'CC_TEXT'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

ENDFORM.                    " Z_INSTANCIA_CONT

*&---------------------------------------------------------------------*
*&      Form  Z_INSTANCIA_TEXT                                         *
*&---------------------------------------------------------------------*
*                          Instancia Obj text                          *
*----------------------------------------------------------------------*
FORM z_instancia_text.

  CHECK v_editor_itxt IS INITIAL.

  CREATE OBJECT v_editor_itxt
    EXPORTING
      parent                     = v_textedit_container_itxt
      wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
      wordwrap_to_linebreak_mode = cl_gui_textedit=>true
    EXCEPTIONS
      OTHERS                     = 1.

  CALL METHOD v_editor_itxt->set_toolbar_mode
    EXPORTING
      toolbar_mode = 0. "Do not show toolbar.

  CALL METHOD v_editor_itxt->set_statusbar_mode
    EXPORTING
      statusbar_mode = 0. "Do not show statusbar.

ENDFORM.                    " Z_INSTANCIA_TEXT
*&---------------------------------------------------------------------*
*&      Module  ZM_NFS_MARC  INPUT                                     *
*&---------------------------------------------------------------------*
*                             Marcação Linha                           *
*----------------------------------------------------------------------*
MODULE zm_nfs_marc INPUT.

  MODIFY t_nfs_m FROM s_nfs INDEX tc_memo-current_line
    TRANSPORTING marc.

ENDMODULE.                 " ZM_NFS_MARC  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_NFS_QTD  INPUT                                      *
*&---------------------------------------------------------------------*
*                           Campo Quantidade                           *
*----------------------------------------------------------------------*
MODULE zm_nfs_qtd INPUT.

  s_nfs-saldo =  s_nfs-saldo_a - s_nfs-qtd.
  IF s_nfs-saldo LT 0.
    MESSAGE e836 WITH text-027.
    s_nfs-saldo = s_nfs-saldo_a.
    s_nfs-qtd   = 0.
  ENDIF.

  MODIFY t_nfs_m FROM s_nfs INDEX tc_memo-current_line
    TRANSPORTING saldo qtd.

ENDMODULE.                 " ZM_NFS_QTD  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_DEL                                                    *
*&---------------------------------------------------------------------*
*                              Deleta Linhas                           *
*----------------------------------------------------------------------*
FORM z_del.

  DELETE t_nfs_m WHERE marc NE space.

ENDFORM.                    " Z_DEL

*&---------------------------------------------------------------------*
*&      Form  Z_MARC_ALL                                               *
*&---------------------------------------------------------------------*
*                          Marca Todas as Linhas                       *
*----------------------------------------------------------------------*
FORM z_marc_all.

  s_nfs-marc = 'X'.
  MODIFY t_nfs_m FROM s_nfs
    TRANSPORTING marc
    WHERE marc IS INITIAL.

ENDFORM.                    " Z_MARC_ALL

*&---------------------------------------------------------------------*
*&      Form  Z_MARC_NONE                                              *
*&---------------------------------------------------------------------*
*                           Desmarca as Linhas                         *
*----------------------------------------------------------------------*
FORM z_marc_none.

  s_nfs-marc = space.
  MODIFY t_nfs_m FROM s_nfs
    TRANSPORTING marc
    WHERE marc NE space.

ENDFORM.                    " Z_MARC_NONE

*&---------------------------------------------------------------------*
*&      Form  Z_SAVE_MEMO                                              *
*&---------------------------------------------------------------------*
*                              Salva Memorando                         *
*----------------------------------------------------------------------*
FORM z_save_memo.

  CLEAR v_ok.

  IF t_nfs_m[] IS INITIAL.
    MESSAGE i836 WITH text-028.
    EXIT.
  ENDIF.

  REFRESH t_text.
* Recupera texto da tela
  CALL METHOD v_editor_itxt->get_text_as_r3table
    IMPORTING
      table = t_text[].

* Verifica Preenchimento dos Campos
  PERFORM z_verifica_campos USING: 'NUM_MEMO' ,
                                   'DATA'     ,
                                   'PAIS_DEST',
                                   'DESP_DDE' ,
                                   'DATA_DDE' ,
                                   'EMB_BL'   ,
                                   'DATA_BL'  ,
                                   'REG_RE'   ,
                                   'DATA_RE'  ,
                                   'NF_EXP'   .

  CHECK v_ok IS INITIAL.

* Verifica Preenchimento das Quantidades
  PERFORM z_verifica_quant.

  CHECK v_ok IS INITIAL.

* Salva Memorando
  PERFORM z_salva_memo.

ENDFORM.                    " Z_SAVE_MEMO

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_CAMPOS                                        *
*&---------------------------------------------------------------------*
*                   Verifica Preenchimento dos Campos                  *
*----------------------------------------------------------------------*
FORM z_verifica_campos USING p_campo TYPE c.

  DATA vl_campo TYPE char20.

  FIELD-SYMBOLS <campo> TYPE ANY.

  CHECK v_ok IS INITIAL.

  CONCATENATE c_memo
              p_campo
         INTO vl_campo SEPARATED BY '-'.

  ASSIGN (vl_campo) TO <campo>.

  CHECK <campo> IS ASSIGNED.

  CHECK <campo> IS INITIAL.

  v_ok = c_x.

  CASE p_campo.
    WHEN 'NUM_MEMO'.
      MESSAGE i836 WITH text-029.
    WHEN 'DATA'.
      MESSAGE i836 WITH text-030.
    WHEN 'PAIS_DEST'.
      MESSAGE i836 WITH text-031.
    WHEN 'DESP_DDE'.
      MESSAGE i836 WITH text-032.
    WHEN 'DATA_DDE'.
      MESSAGE i836 WITH text-033.
    WHEN 'EMB_BL'.
      MESSAGE i836 WITH text-034.
    WHEN 'DATA_BL'.
      MESSAGE i836 WITH text-035.
    WHEN 'REG_RE'.
      MESSAGE i836 WITH text-036.
    WHEN 'DATA_RE'.
      MESSAGE i836 WITH text-037.
    WHEN 'NF_EXP'.
      MESSAGE i836 WITH text-042.
  ENDCASE.

ENDFORM.                    " Z_VERIFICA_CAMPOS

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_QUANT                                         *
*&---------------------------------------------------------------------*
*                Verifica Preenchimento das Quantidades                *
*----------------------------------------------------------------------*
FORM z_verifica_quant.

  DATA sl_nfs TYPE type_nfs.

  LOOP AT t_nfs_m INTO sl_nfs.

    IF sl_nfs-qtd IS INITIAL.
      MESSAGE i836 WITH text-038.
      v_ok = c_x.
      EXIT.
    ENDIF.

    CLEAR sl_nfs.

  ENDLOOP.

ENDFORM.                    " Z_VERIFICA_QUANT

*&---------------------------------------------------------------------*
*&      Module  ZM_NFS_MEMO  INPUT                                     *
*&---------------------------------------------------------------------*
*                      Verifica Número Memorando                       *
*----------------------------------------------------------------------*
MODULE zm_nfs_memo INPUT.

* Verifica Número Memorando
  PERFORM z_verifica_nummemo.

ENDMODULE.                 " ZM_NFS_MEMO  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_SALVA_MEMO                                             *
*&---------------------------------------------------------------------*
*                            Salva Memorando                           *
*----------------------------------------------------------------------*
FORM z_salva_memo.

  DATA: tl_zsdt0004 TYPE TABLE OF zsdt0004,
        s_zsdt0004  TYPE zsdt0004         ,
        sl_nfs      TYPE type_nfs         ,
        sl_text     TYPE type_text        ,
        vl_text     TYPE string           .

  LOOP AT t_text INTO sl_text.

    CONCATENATE vl_text
                sl_text-line
           INTO vl_text.

    CLEAR sl_text.

  ENDLOOP.

  LOOP AT t_nfs_m INTO sl_nfs.

    s_zsdt0004-numme    = s_memo-num_memo.
    s_zsdt0004-nfenum   = sl_nfs-nfenum.
    s_zsdt0004-docnum   = sl_nfs-docnum.
    s_zsdt0004-vbeln    = sl_nfs-vbeln.
    s_zsdt0004-bukrs    = sl_nfs-bukrs.
    s_zsdt0004-werks    = sl_nfs-werks.
    s_zsdt0004-lgort    = sl_nfs-lgort.
    s_zsdt0004-matnr    = sl_nfs-matnr.
    s_zsdt0004-kunnr    = sl_nfs-kunnr.
    s_zsdt0004-saldo    = sl_nfs-saldo.
    s_zsdt0004-quanbai  = sl_nfs-qtd.
    s_zsdt0004-datbai   = s_memo-data.
    s_zsdt0004-paisdest = s_memo-pais_dest.
    s_zsdt0004-numde    = s_memo-desp_dde.
    s_zsdt0004-datdde   = s_memo-data_dde.
    s_zsdt0004-numbl    = s_memo-emb_bl.
    s_zsdt0004-databl   = s_memo-data_bl.
    s_zsdt0004-regre    = s_memo-reg_re.
    s_zsdt0004-datre    = s_memo-data_re.
    s_zsdt0004-obs      = vl_text.
    s_zsdt0004-nf_exp   = s_memo-nf_exp.

    APPEND s_zsdt0004 TO tl_zsdt0004.

    CLEAR: s_zsdt0004,
           sl_nfs    .

  ENDLOOP.

  INSERT zsdt0004 FROM TABLE tl_zsdt0004.
  COMMIT WORK AND WAIT.

  MESSAGE i836 WITH text-040.
  SUBMIT zsdi0002 VIA SELECTION-SCREEN.

ENDFORM.                    " Z_SALVA_MEMO

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_004                                          *
*&---------------------------------------------------------------------*
*                            Seleciona ZSDT0004                        *
*----------------------------------------------------------------------*
FORM z_seleciona_004.

  CHECK NOT t_doc[] IS INITIAL.

  SELECT *
    FROM zsdt0004
    INTO TABLE t_zsdt0004
    FOR ALL ENTRIES IN t_doc
  WHERE  nfenum EQ t_doc-nfenum.

  SORT t_zsdt0004 BY nfenum ASCENDING.

ENDFORM.                    " Z_SELECIONA_004

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_NUMMEMO                                       *
*&---------------------------------------------------------------------*
*                        Verifica Número Memorando                     *
*----------------------------------------------------------------------*
FORM z_verifica_nummemo.

  CHECK NOT s_memo-num_memo IS INITIAL.

  SELECT SINGLE numme
    FROM zsdt0004
    INTO s_memo-num_memo
  WHERE  numme EQ s_memo-num_memo.

  CHECK sy-subrc IS INITIAL.

  MESSAGE e836 WITH text-039.

ENDFORM.                    " Z_VERIFICA_NUMMEMO

*&---------------------------------------------------------------------*
*&      Form  Z_INICIA_PROC                                            *
*&---------------------------------------------------------------------*
*                         Lógica Processamento                         *
*----------------------------------------------------------------------*
FORM z_inicia_proc.

* Seleção dos Dados
  PERFORM: z_seleciona_dados,

* Monta Dados NF's
           z_monta_nfs      ,

* Monta FieldCat
           z_monta_fieldcat .

  IF t_nfs[] IS INITIAL.
    MESSAGE i836 WITH text-012.
  ELSE.
    CALL SCREEN 0100.
  ENDIF.

ENDFORM.                    " Z_INICIA_PROC

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_005                                          *
*&---------------------------------------------------------------------*
*                          Seleciona ZSDT005                           *
*----------------------------------------------------------------------*
FORM z_seleciona_005.

  SORT t_zsdt0005 BY cfop ASCENDING.

  SELECT *
    FROM zsdt0005
    INTO TABLE t_zsdt0005.

ENDFORM.                    " Z_SELECIONA_005

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBAK                                         *
*&---------------------------------------------------------------------*
*                            Seleciona VBAK                            *
*----------------------------------------------------------------------*
FORM z_seleciona_vbak.

  DATA tl_vbrp TYPE TABLE OF type_vbrp.

  REFRESH t_vbak.

  CHECK NOT t_vbrp[] IS INITIAL.
  tl_vbrp[] = t_vbrp[].
  SORT tl_vbrp BY aubel ASCENDING.
  DELETE: ADJACENT DUPLICATES FROM tl_vbrp COMPARING aubel,
           tl_vbrp WHERE aubel EQ space                   .

  SELECT vbeln auart
    FROM vbak
    INTO TABLE t_vbak
    FOR ALL ENTRIES IN tl_vbrp
  WHERE  vbeln EQ tl_vbrp-aubel.

  SORT t_vbak BY vbeln ASCENDING.

ENDFORM.                    " Z_SELECIONA_VBAK

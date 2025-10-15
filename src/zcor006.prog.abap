REPORT zcor006 NO STANDARD PAGE HEADING
                            LINE-SIZE 200 MESSAGE-ID z01.
*$*$ -------------------------------------------------------------- *$*$
*$*$                    GRUPO ANDRÉ MAGGI                           *$*$
*$*$ -------------------------------------------------------------- *$*$
*$*$ Autor     : Robson Motta - BBKO Consulting                     *$*$
*$*$ Data      : 12/07/2009                                         *$*$
*$*$ Descrição : Relat. baseado nas produções realizadas no período.*$*$
*$*$                                                                *$*$
*$*$ -------------------------------------------------------------- *$*$
*$*$ Histórico de modificações                                      *$*$
*$*$ -------------------------------------------------------------- *$*$
*$*$ Data      :                                                    *$*$
*$*$ Autor     :                                                    *$*$
*$*$ Solicit.  :                                                    *$*$
*$*$ Chamado   :                                                    *$*$
*$*$ Descrição :                                                    *$*$
*$*$ Versão    :                                                    *$*$
*$*$ ---------------------------------------------------------------*$*$
*&---------------------------------------------------------------------*
*& Definição da classe local                                           *
*&---------------------------------------------------------------------*
CLASS zcl_local DEFINITION DEFERRED.

*&---------------------------------------------------------------------*
*& Tabelas Transparentes                                               *
*&---------------------------------------------------------------------*
TABLES: ckmlhd  , "Ledger de material: registro de cabeçalho
        mara    , "Dados gerais de material
        ckmlkeph. "Ledger de materiais: estratificação (elementos)

*&---------------------------------------------------------------------*
*& Tipos                                                               *
*&---------------------------------------------------------------------*
TYPE-POOLS: ckmv0, ccs01.

TYPES: BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         matkl TYPE mara-matkl,
         mtart TYPE mara-mtart,
       END OF ty_mara,
       ty_mara_tab TYPE STANDARD TABLE OF ty_mara,

       BEGIN OF ty_zlayout,
         arbpl         TYPE arbpl,
         ktext(40)     TYPE c,
         matkl         TYPE matkl,
         wgbez         TYPE wgbez,
         tipo(1)       TYPE c,
         matnr_t       TYPE matnr,
         maktx_t       TYPE maktx,
         bwkey_t(4)    TYPE c,
         kalnr_t(12)   TYPE n,
         matnr_r       TYPE matnr,
         maktx_r       TYPE maktx,
         mtart_r       TYPE mtart,
         mtbez_r       TYPE mtbez,
         bwkey_r(4)    TYPE c,
         kalnr_r(12)   TYPE n,
         kostl_r(10)   TYPE c,
         ltext_r       TYPE ltext,                 "Descrição longa cc
         kalnr_prz(12) TYPE n,
         verid_nd(4)   TYPE c,
         otyp_inmat(2) TYPE c,
         otyp_prz(2)   TYPE c,
         mlast(1)      TYPE c,
         maktx         TYPE maktx,                 "Descrição PAI ou Filho
         bwkey(4)      TYPE c,                     "Centro PAI ou Filho
         kalnr(12)     TYPE n,                     "Cálculo Custo PAI ou Filho
         lbkum         TYPE ck_lbkum,
         meins         TYPE meins,
****
         vlu_std       TYPE stprs,
         vlu_real      TYPE stprs,
         vlt_std       TYPE stprs,
         vlt_real      TYPE stprs,
         tma_std       TYPE stprs,
         mod_std       TYPE stprs,
         tma_real      TYPE stprs,
         sal_real      TYPE stprs,
         enc_real      TYPE stprs,
         cont_real     TYPE stprs,
         cons_real     TYPE stprs,
         comb_real     TYPE stprs,
         manu_real     TYPE stprs,
         mset_real     TYPE stprs,
         serv_real     TYPE stprs,
         alug_real     TYPE stprs,
         leas_real     TYPE stprs,
         segu_real     TYPE stprs,
         srv_real      TYPE stprs,
         supo_real     TYPE stprs,
         comu_real     TYPE stprs,
         viag_real     TYPE stprs,
         impo_real     TYPE stprs,
         doac_real     TYPE stprs,
         desp_real     TYPE stprs,
         depr_real     TYPE stprs,
****
         tma_var       TYPE stprs,
**       mod_std       TYPE stprs,
         waers         TYPE waers,
       END OF ty_zlayout,

       BEGIN OF ty_makt,
         matnr TYPE mara-matnr,
         maktx TYPE makt-maktx,
       END OF ty_makt,

       BEGIN OF ty_ckmlhd,
         kalnr TYPE ckmlhd-kalnr,
         mlast TYPE ckmlhd-mlast,
         matnr TYPE ckmlhd-matnr,
         bwkey TYPE ckmlhd-bwkey,
         bwtar TYPE ckmlhd-bwtar,
       END OF ty_ckmlhd,
       ty_ckmlhd_tab TYPE STANDARD TABLE OF ty_ckmlhd WITH DEFAULT KEY,

       BEGIN OF ty_ckmlpp,
         kalnr  TYPE ckmlpp-kalnr,
         bdatj  TYPE ckmlpp-bdatj,
         poper  TYPE ckmlpp-poper,
         untper TYPE ckmlpp-untper,
         abkumo TYPE ckmlpp-abkumo,
         zukumo TYPE ckmlpp-zukumo,
       END OF ty_ckmlpp,

       BEGIN OF ty_ckmlcr,
         kalnr  TYPE ckmlcr-kalnr,
         bdatj  TYPE ckmlcr-bdatj,
         poper  TYPE ckmlcr-poper,
         untper TYPE ckmlcr-untper,
         curtp  TYPE ckmlcr-curtp,
         peinh  TYPE ckmlcr-peinh,
         vprsv  TYPE ckmlcr-vprsv,
         stprs  TYPE ckmlcr-stprs,
         pvprs  TYPE ckmlcr-pvprs,
       END OF ty_ckmlcr,

       BEGIN OF ty_mbew,
         matnr TYPE mbew-matnr,
         bwkey TYPE mbew-bwkey,
         bwtar TYPE mbew-bwtar,
         vprsv TYPE mbew-vprsv,
         mlast TYPE mbew-mlast,
       END OF ty_mbew,

       BEGIN OF ty_ckmlprkeph,
         kalnr  TYPE ckmlprkeph-kalnr,
         bdatj  TYPE ckmlprkeph-bdatj,
         poper  TYPE ckmlprkeph-poper,
         untper TYPE ckmlprkeph-untper,
         keart  TYPE ckmlprkeph-keart,
         prtyp  TYPE ckmlprkeph-prtyp,
         kkzst  TYPE ckmlprkeph-kkzst,
         patnr  TYPE ckmlprkeph-patnr,
         dipa   TYPE ckmlprkeph-dipa,
         curtp  TYPE ckmlprkeph-curtp,
         waers  TYPE ckmlprkeph-waers,
         kst001 TYPE ckmlprkeph-kst001,
         kst002 TYPE ckmlprkeph-kst002,
         kst003 TYPE ckmlprkeph-kst003,
         kst004 TYPE ckmlprkeph-kst004,
         kst005 TYPE ckmlprkeph-kst005,
         kst006 TYPE ckmlprkeph-kst006,
         kst007 TYPE ckmlprkeph-kst007,
         kst008 TYPE ckmlprkeph-kst008,
         kst009 TYPE ckmlprkeph-kst009,
         kst010 TYPE ckmlprkeph-kst010,
         kst011 TYPE ckmlprkeph-kst011,
         kst012 TYPE ckmlprkeph-kst012,
         kst013 TYPE ckmlprkeph-kst013,
         kst014 TYPE ckmlprkeph-kst014,
         kst015 TYPE ckmlprkeph-kst015,
         kst016 TYPE ckmlprkeph-kst016,
         kst017 TYPE ckmlprkeph-kst017,
         kst018 TYPE ckmlprkeph-kst018,
         kst019 TYPE ckmlprkeph-kst019,
         kst020 TYPE ckmlprkeph-kst020,
         kst021 TYPE ckmlprkeph-kst021,
         kst022 TYPE ckmlprkeph-kst022,
         kst023 TYPE ckmlprkeph-kst023,
         kst024 TYPE ckmlprkeph-kst024,
         kst025 TYPE ckmlprkeph-kst025,
         kst026 TYPE ckmlprkeph-kst026,
         kst027 TYPE ckmlprkeph-kst027,
         kst028 TYPE ckmlprkeph-kst028,
         kst029 TYPE ckmlprkeph-kst029,
         kst030 TYPE ckmlprkeph-kst030,
         kst031 TYPE ckmlprkeph-kst031,
         kst032 TYPE ckmlprkeph-kst032,
         kst033 TYPE ckmlprkeph-kst033,
         kst034 TYPE ckmlprkeph-kst034,
         kst035 TYPE ckmlprkeph-kst035,
         kst036 TYPE ckmlprkeph-kst036,
         kst037 TYPE ckmlprkeph-kst037,
         kst038 TYPE ckmlprkeph-kst038,
       END OF ty_ckmlprkeph,

       BEGIN OF ty_keko,
         bzobj TYPE keko-bzobj,
         kalnr TYPE keko-kalnr,
         kalka TYPE keko-kalka,
         kadky TYPE keko-kadky,
         tvers TYPE keko-tvers,
         bwvar TYPE keko-bwvar,
         kkzma TYPE keko-kkzma,
         loekz TYPE keko-loekz,
         losgr TYPE keko-losgr,
         meins TYPE keko-meins,
       END OF ty_keko,

       BEGIN OF ty_keph,
         kalnr  TYPE keph-kalnr,
         kalka  TYPE keph-kalka,
         kadky  TYPE keph-kadky,
         tvers  TYPE keph-tvers,
         bwvar  TYPE keph-bwvar,
         kkzma  TYPE keph-kkzma,
         kst001 TYPE keph-kst001,
         kst002 TYPE keph-kst002,
         kst003 TYPE keph-kst003,
         kst004 TYPE keph-kst004,
         kst005 TYPE keph-kst005,
         kst006 TYPE keph-kst006,
         kst007 TYPE keph-kst007,
         kst008 TYPE keph-kst008,
         kst009 TYPE keph-kst009,
         kst010 TYPE keph-kst010,
         kst011 TYPE keph-kst011,
         kst012 TYPE keph-kst012,
         kst013 TYPE keph-kst013,
         kst014 TYPE keph-kst014,
         kst015 TYPE keph-kst015,
         kst016 TYPE keph-kst016,
         kst017 TYPE keph-kst017,
         kst018 TYPE keph-kst018,
         kst019 TYPE keph-kst019,
         kst020 TYPE keph-kst020,
         kst021 TYPE keph-kst021,
         kst022 TYPE keph-kst022,
         kst023 TYPE keph-kst023,
         kst024 TYPE keph-kst024,
         kst025 TYPE keph-kst025,
         kst026 TYPE keph-kst026,
         kst027 TYPE keph-kst027,
         kst028 TYPE keph-kst028,
         kst029 TYPE keph-kst029,
         kst030 TYPE keph-kst030,
         kst031 TYPE keph-kst031,
         kst032 TYPE keph-kst032,
         kst033 TYPE keph-kst033,
         kst034 TYPE keph-kst034,
         kst035 TYPE keph-kst035,
         kst036 TYPE keph-kst036,
         kst037 TYPE keph-kst037,
         kst038 TYPE keph-kst038,
       END OF ty_keph,

       BEGIN OF ty_mlcd,
         kalnr  TYPE mlcd-kalnr,
         bdatj  TYPE mlcd-bdatj,
         poper  TYPE mlcd-poper,
         untper TYPE mlcd-untper,
         categ  TYPE mlcd-categ,
         ptyp   TYPE mlcd-ptyp,
         bvalt  TYPE mlcd-bvalt,
         curtp  TYPE mlcd-curtp,
         lbkum  TYPE mlcd-lbkum,
         meins  TYPE mlcd-meins,
         waers  TYPE mlcd-waers,
       END OF ty_mlcd,

       BEGIN OF ty_mbewh,
         matnr TYPE mbewh-matnr,
         bwkey TYPE mbewh-bwkey,
         bwtar TYPE mbewh-bwtar,
         verpr TYPE mbewh-verpr,
         stprs TYPE mbewh-stprs,
       END OF ty_mbewh,

       BEGIN OF ty_crhd,
         objty TYPE crhd-objty,
         objid TYPE crhd-objid,
         arbpl TYPE crhd-arbpl,
         ktext TYPE crtx-ktext,
       END OF ty_crhd,

       BEGIN OF ty_mv001,
         kalnr    TYPE ckmlmv001-kalnr,
         verid_nd TYPE ckmlmv001-verid_nd,
       END OF ty_mv001,

       BEGIN OF ty_ckmlmv001,
         kalnr TYPE ckmlmv001-kalnr,
         otyp  TYPE ckmlmv001-otyp,
         bwkey TYPE ckmlmv001-bwkey,
         werks TYPE ckmlmv001-werks,
         matnr TYPE ckmlmv001-matnr,
         bwtar TYPE ckmlmv001-bwtar,
       END OF ty_ckmlmv001,

       BEGIN OF ty_nodes,
         nivel TYPE i,
         nodek TYPE lvc_nkey,
       END OF ty_nodes,

       BEGIN OF ty_nivel,
         enivel TYPE i,
         cnivel TYPE i,
       END OF ty_nivel,
       ty_feh_sta TYPE RANGE OF keko-feh_sta.

*&---------------------------------------------------------------------*
*& Tabelas Internas / Estruturas                                       *
*&---------------------------------------------------------------------*
DATA: t_mara    TYPE STANDARD TABLE OF ty_mara,
      t_makt    TYPE STANDARD TABLE OF ty_makt,
      t_ckmlhd  TYPE STANDARD TABLE OF ty_ckmlhd,
      t_ckmlpp  TYPE STANDARD TABLE OF ty_ckmlpp,
***      t_mlcd    TYPE STANDARD TABLE OF ty_mlcd   ,
      t_mkal    TYPE STANDARD TABLE OF mkal,
      t_t023t   TYPE STANDARD TABLE OF t023t,
      t_crhd    TYPE STANDARD TABLE OF ty_crhd,
      t_filhos  TYPE STANDARD TABLE OF ty_ckmlhd,
      t_ckmlcr  TYPE STANDARD TABLE OF ty_ckmlcr,
      t_ckmlke  TYPE STANDARD TABLE OF ckmlprkeko,
      t_keko    TYPE STANDARD TABLE OF ty_keko,
      t_keph    TYPE STANDARD TABLE OF ty_keph,
      t_mv001   TYPE STANDARD TABLE OF ty_mv001,
      t_nodes   TYPE STANDARD TABLE OF ty_nodes,
      t_mbew    TYPE STANDARD TABLE OF ty_mbew,
      t_mbewh   TYPE STANDARD TABLE OF ty_mbewh,
      t_ckmlph  TYPE STANDARD TABLE OF ty_ckmlprkeph,
      t_saida   TYPE STANDARD TABLE OF ty_zlayout,
      t_saida2  TYPE STANDARD TABLE OF ty_zlayout,
      t_fdcat   TYPE lvc_t_fcat,
      e_mara    TYPE ty_mara,
      e_makt    TYPE ty_makt,
      e_ckmlhd  TYPE ty_ckmlhd,
      e_ckmlpp  TYPE ty_ckmlpp,
      e_ckmlke  TYPE ckmlprkeko,
      e_ckmlph  TYPE ty_ckmlprkeph,
      e_mlcd    TYPE ty_mlcd,
      e_mkal    TYPE mkal,
      e_keko    TYPE ty_keko,
      e_keph    TYPE ty_keph,
      e_t023t   TYPE t023t,
      e_t134t   TYPE t134t,
      e_crhd    TYPE ty_crhd,
      e_mv001   TYPE ty_mv001,
      e_variant TYPE disvariant,
      e_nivel   TYPE ty_nivel,
      e_nodes   TYPE ty_nodes,
      e_mbew    TYPE ty_mbew,
      e_mbewh   TYPE ty_mbewh,
      e_ckmlcr  TYPE ty_ckmlcr,
      e_table   TYPE ty_zlayout,
      e_saida   TYPE ty_zlayout.

*&---------------------------------------------------------------------*
*& Variáveis                                                           *
*&---------------------------------------------------------------------*
DATA: v_okcode   TYPE sy-ucomm,
      v_data     TYPE sy-datum,
      v_nmax     TYPE i,
      v_alv_tree TYPE REF TO cl_gui_alv_tree,
      v_class    TYPE REF TO zcl_local.

DATA: r_feh_sta TYPE ty_feh_sta.

*&---------------------------------------------------------------------*
* Constants                                                            *
*&---------------------------------------------------------------------*
CONSTANTS:
  c_back  TYPE sy-ucomm   VALUE 'BACK',
  c_ende  TYPE sy-ucomm   VALUE 'EXIT',
  c_canc  TYPE sy-ucomm   VALUE 'CANC',
  c_expg  TYPE sy-ucomm   VALUE '&EXPAND_ALL',
  c_comg  TYPE sy-ucomm   VALUE '&COLLAPSE_ALL',
  c_expa  TYPE sy-ucomm   VALUE 'EXPAND',
  c_coll  TYPE sy-ucomm   VALUE 'COLLAPSE',
  c_info  TYPE sy-ucomm   VALUE 'INFO',
  c_mgtyp TYPE ckml_mgtyp VALUE '00001'. "Estrutura quantitativa real

*&---------------------------------------------------------------------*
*& Tela de Seleção                                                     *
*&---------------------------------------------------------------------*
* Parâmetros de Seleção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
*** Materiais
  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
    SELECT-OPTIONS: s_matnr FOR ckmlhd-matnr,
                    s_werks FOR ckmlhd-bwkey MATCHCODE OBJECT ml_va_werks_prod,
                    s_bwtar FOR ckmlhd-bwtar,
                    s_matkl FOR mara-matkl  ,
                    s_mtart FOR mara-mtart.
  SELECTION-SCREEN END OF BLOCK b2.

*** Período
  SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
    PARAMETERS:
      p_period TYPE cki_doc_ml-sl_periode OBLIGATORY DEFAULT sy-datum+4(2),
      p_gjahr  TYPE ckmlrunplant-gjahr    OBLIGATORY DEFAULT sy-datum(4).
  SELECTION-SCREEN END OF BLOCK b3.

*** Moeda
  SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
    PARAMETERS
      p_curtp TYPE ckmlcr-curtp OBLIGATORY AS LISTBOX VISIBLE LENGTH 60.
  SELECTION-SCREEN END OF BLOCK b4.

*** Opção de Lista
  SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-005.
    PARAMETERS: p_varian TYPE disvariant-variant.
    SELECTION-SCREEN SKIP.
    PARAMETERS: p_ver TYPE char01 RADIOBUTTON GROUP g1 DEFAULT 'X',
                p_mat TYPE char01 RADIOBUTTON GROUP g1.
  SELECTION-SCREEN END OF BLOCK b5.
SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& Macros                                                              *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      _FILL
*&---------------------------------------------------------------------*
*       Macro para preencher o range
*----------------------------------------------------------------------*
DEFINE _fill.
  CLEAR: &1.
  &1     = 'IEQ'.
  &1-low = &3.
  APPEND &1 TO &2.
END-OF-DEFINITION.  "_FILL

*&----------------------------------------------------------------------
* Classes                                                              *
*&----------------------------------------------------------------------
*---------------------------------------------------------------------*
*       CLASS ZCL_LOCAL DEFINITION
*---------------------------------------------------------------------*
*       Classe local - Definição
*---------------------------------------------------------------------*
CLASS zcl_local DEFINITION.
  PUBLIC SECTION.

    METHODS handle_node_double_click
      FOR EVENT node_double_click OF cl_gui_alv_tree
      IMPORTING node_key sender.

    METHODS handle_item_double_click
      FOR EVENT item_double_click OF cl_gui_alv_tree
      IMPORTING node_key fieldname sender.

    METHODS handle_user_command
      FOR EVENT before_user_command OF cl_gui_alv_tree
      IMPORTING ucomm.

ENDCLASS.                    "ZCL_LOCAL DEFINITION
*---------------------------------------------------------------------*
*       CLASS ZCL_LOCAL IMPLEMENTATION
*---------------------------------------------------------------------*
*       Classe local - Implementação
*---------------------------------------------------------------------*
CLASS zcl_local IMPLEMENTATION.

  METHOD handle_node_double_click.
*.. Navega material topo para análise de preço
    PERFORM f_navega_ckm3 USING node_key.
  ENDMETHOD.                    "HANDLE_NODE_DOUBLE_CLICK

  METHOD handle_item_double_click.
*.. Navega material topo para análise de preço
    PERFORM f_navega_ckm3 USING node_key.
  ENDMETHOD.                    "HANDLE_ITEM_DOUBLE_CLICK

  METHOD handle_user_command.
    DATA: lt_nodes TYPE lvc_t_nkey.

    CASE ucomm.
      WHEN c_expg.
*...... Trata tabela de nó a ser expandido ou comprimido
        PERFORM f_trata_nodes USING 'A' CHANGING lt_nodes.

*...... Expande toda a hierarquia
        CHECK NOT lt_nodes IS INITIAL.
        CALL METHOD v_alv_tree->expand_nodes
          EXPORTING
            it_node_key             = lt_nodes
          EXCEPTIONS
            failed                  = 1
            cntl_system_error       = 2
            error_in_node_key_table = 3
            dp_error                = 4
            node_not_found          = 5
            OTHERS                  = 6.

      WHEN c_comg.
*...... Trata tabela de nó a ser expandido ou comprimido
        PERFORM f_trata_nodes USING 'B' CHANGING lt_nodes.

*...... Agrupa toda a hierarquia
        CALL METHOD v_alv_tree->collapse_all_nodes
          EXCEPTIONS
            failed            = 1
            cntl_system_error = 2
            OTHERS            = 3.

      WHEN c_expa.
*...... Trata tabela de nó a ser expandido ou comprimido
        PERFORM f_trata_nodes USING 'E' CHANGING lt_nodes.

*...... Expande toda a hierarquia
        CHECK NOT lt_nodes IS INITIAL.
        CALL METHOD v_alv_tree->expand_nodes
          EXPORTING
            it_node_key             = lt_nodes
          EXCEPTIONS
            failed                  = 1
            cntl_system_error       = 2
            error_in_node_key_table = 3
            dp_error                = 4
            node_not_found          = 5
            OTHERS                  = 6.

      WHEN c_coll.
*...... Trata tabela de nó a ser expandido ou comprimido
        PERFORM f_trata_nodes USING 'C' CHANGING lt_nodes.

*...... Expande toda a hierarquia
        LOOP AT lt_nodes INTO e_nodes-nodek.
          CALL METHOD v_alv_tree->collapse_subtree
            EXPORTING
              i_node_key        = e_nodes-nodek
            EXCEPTIONS
              failed            = 1
              node_not_found    = 2
              cntl_system_error = 3
              OTHERS            = 4.
        ENDLOOP.

      WHEN c_info.
*...... Executa tela
        CALL SCREEN 9002 STARTING AT 10 10 ENDING AT 75 15.
        SET SCREEN 9001.
        LEAVE SCREEN.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "ZCL_LOCAL IMPLEMENTATION

*&---------------------------------------------------------------------*
*& Evento: INITIALIZATION                                              *
*&---------------------------------------------------------------------*
INITIALIZATION.

*** Valores iniciais
  PERFORM f_valores_iniciais.

*&---------------------------------------------------------------------*
*& Evento: AT SELECTION SCREEN                                         *
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varian.

*** Ajuda de pesquisa para Layout do ALV
  PERFORM f_f4_for_variant.

AT SELECTION-SCREEN.

*** Valida Variante Informada
  PERFORM f_valida_variante.

*&---------------------------------------------------------------------*
*& Início do Programa                                                  *
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*** Valida Parâmetros de entrada
  PERFORM f_valida_parametros.

*** Inicializa dados
  PERFORM f_inicializa_dados.

*** Obtém dados para processamento
  PERFORM f_obtem_dados.

*** Processa dados obtidos
  PERFORM f_processa_dados.

*** Chama tela do relatório
  PERFORM f_exibe_relatorio.

*&---------------------------------------------------------------------*
*& Rotinas                                                             *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*       Ajuda de pesquisa para Layout do ALV
*----------------------------------------------------------------------*
FORM f_f4_for_variant.

  DATA: locl_variant TYPE disvariant,
        exit.

*** Lista variantes do ALV
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = e_variant
      i_save     = 'A'
    IMPORTING
      e_exit     = exit
      es_variant = locl_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF exit = space.
      p_varian = locl_variant-variant.
      MOVE locl_variant TO e_variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F_VALORES_INICIAIS
*&---------------------------------------------------------------------*
*       Valores iniciais
*----------------------------------------------------------------------*
FORM f_valores_iniciais.

  DATA: l_feh_sta TYPE LINE OF ty_feh_sta.

*** Atribui dados do programa
  CLEAR e_variant.
  e_variant-report = sy-repid.
  e_variant-username = sy-uname.

*** Obtém variante default do usuário
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = 'A'
    CHANGING
      cs_variant = e_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 0.
    p_varian = e_variant-variant.
  ENDIF.

  REFRESH: r_feh_sta.

*** Preencher Status do cálculo de custos
  _fill l_feh_sta r_feh_sta: 'FR', 'FM'.

ENDFORM.                    " F_VALORES_INICIAIS
*&---------------------------------------------------------------------*
*&      Form  F_INICIALIZA_DADOS
*&---------------------------------------------------------------------*
*       Inicializa dados
*----------------------------------------------------------------------*
FORM f_inicializa_dados.

  CLEAR: v_alv_tree, v_data, v_nmax, v_okcode, v_class.

  CLEAR: e_ckmlhd, e_ckmlph, e_crhd, e_keko, e_keph, e_makt, e_ckmlke,
         e_mara, e_mkal, e_mlcd, e_saida, e_t023t, e_mv001, e_nivel,
         e_nodes, e_table, e_mbew, e_ckmlcr, e_mbewh, e_ckmlpp.

  REFRESH: t_ckmlcr, t_ckmlhd, t_ckmlke, t_ckmlph, t_crhd, t_fdcat,
           t_filhos, t_keko, t_keph, t_makt, t_mara, t_mkal, "t_mlcd,
           t_mv001, t_nodes, t_saida, t_saida2, t_t023t, t_mbew,
           t_mbewh, t_ckmlpp.

*** Trata dada do período
  CONCATENATE p_gjahr p_period+1 '01' INTO v_data.

ENDFORM.                    " F_INICIALIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_OBTEM_DADOS
*&---------------------------------------------------------------------*
*       Obtém dados para processamento
*----------------------------------------------------------------------*
FORM f_obtem_dados.

  DATA: lt_key_m  TYPE ty_ckmlhd_tab,
        lt_mara   TYPE ty_mara_tab,
        lr_untper TYPE RANGE OF mlcd-untper,
        lr_bvalt  TYPE RANGE OF mlcd-bvalt.

  "Obtendo dados...
  PERFORM f_indica_processamento USING 08 TEXT-009.

  IF NOT s_matnr[] IS INITIAL OR NOT s_matkl[] IS INITIAL
    OR NOT s_mtart[] IS INITIAL.
*.. Obtém dados do material
    SELECT matnr matkl mtart FROM mara INTO TABLE t_mara
           WHERE matnr IN s_matnr
             AND matkl IN s_matkl
             AND mtart IN s_mtart.
    IF sy-subrc NE 0.
      MESSAGE s012 DISPLAY LIKE 'E'.
      "Nenhum material encontrado para a seleção
      STOP.                                                 "#EC *
    ENDIF.

    "Obtendo dados...
    PERFORM f_indica_processamento USING 16 TEXT-009.

*.. Obtém dados de Ledger de material: registro de cabeçalho
    SELECT kalnr mlast matnr bwkey bwtar
           FROM ckmlhd INTO TABLE t_ckmlhd
           FOR ALL ENTRIES IN t_mara
           WHERE matnr EQ t_mara-matnr
             AND bwkey IN s_werks
             AND bwtar IN s_bwtar.
    IF sy-subrc NE 0.
      MESSAGE s012 DISPLAY LIKE 'E'.
      "Nenhum material encontrado para a seleção
      STOP.                                                 "#EC *
    ELSE.
      SORT t_ckmlhd BY matnr bwtar kalnr.
    ENDIF.
  ELSE.
*.. Obtém dados de Ledger de material: registro de cabeçalho
    SELECT kalnr mlast matnr bwkey bwtar
           FROM ckmlhd INTO TABLE t_ckmlhd
           WHERE matnr IN s_matnr
             AND bwkey IN s_werks
             AND bwtar IN s_bwtar.

    IF t_ckmlhd[] IS INITIAL.
      MESSAGE s012 DISPLAY LIKE 'E'.
      "Nenhum material encontrado para a seleção
      STOP.                                                 "#EC *
    ELSE.
*.... Trata campo Material
      lt_key_m[] = t_ckmlhd[].
      SORT lt_key_m BY matnr.
      DELETE ADJACENT DUPLICATES FROM lt_key_m COMPARING matnr.

      "Obtendo dados...
      PERFORM f_indica_processamento USING 16 TEXT-009.

*.... Obtém dados do material
      SELECT matnr matkl mtart FROM mara INTO TABLE t_mara
             FOR ALL ENTRIES IN lt_key_m
             WHERE matnr EQ lt_key_m-matnr
               AND matkl IN s_matkl
               AND mtart IN s_mtart.
      IF sy-subrc NE 0.
        MESSAGE s012 DISPLAY LIKE 'E'.
        "Nenhum material encontrado para a seleção
        STOP.                                               "#EC *
      ENDIF.

      SORT t_ckmlhd BY matnr bwtar kalnr.
    ENDIF.
  ENDIF.

  "Obtendo dados...
  PERFORM f_indica_processamento USING 24 TEXT-009.

***  IF NOT t_ckmlhd[] IS INITIAL.
****.. Obtém Ledger de materiais: registros compactação (de documentos)
***    SELECT kalnr bdatj poper untper categ ptyp bvalt curtp lbkum
***           meins waers
***           FROM mlcd INTO TABLE t_mlcd
***           FOR ALL ENTRIES IN t_ckmlhd
***           WHERE kalnr  EQ t_ckmlhd-kalnr
***             AND bdatj  EQ p_gjahr
***             AND poper  EQ p_period
***             AND untper IN lr_untper
***             AND categ  EQ 'ZU'
***             AND ptyp   EQ 'BF'
***             AND bvalt  IN lr_bvalt
***             AND curtp  EQ p_curtp.
***
***    SORT t_mlcd BY kalnr untper bvalt.
***  ENDIF.

*** Trata dados de Ledger de material
  PERFORM f_restringe_ledger CHANGING lt_mara.

  "Obtendo dados...
  PERFORM f_indica_processamento USING 88 TEXT-009.

*** Obtém texto do material
  IF NOT lt_mara[] IS INITIAL.
    SELECT matnr maktx FROM makt INTO TABLE t_makt
           FOR ALL ENTRIES IN lt_mara
           WHERE matnr EQ lt_mara-matnr
             AND spras EQ sy-langu.
  ENDIF.

*** Obtém texto do grupo de mercadoria
  lt_mara[] = t_mara[].
  SORT lt_mara BY matkl.
  DELETE ADJACENT DUPLICATES FROM lt_mara COMPARING matkl.
  IF NOT lt_mara[] IS INITIAL.
    SELECT * FROM t023t INTO TABLE t_t023t
           FOR ALL ENTRIES IN lt_mara
           WHERE matkl EQ lt_mara-matkl
             AND spras EQ sy-langu.
  ENDIF.

  SORT: t_ckmlhd BY matnr bwtar kalnr,
        t_mara   BY matnr,
        t_makt   BY matnr,
        t_t023t  BY matkl,
        t_mkal   BY bdatu mdv01.

  "Obtendo dados...
  PERFORM f_indica_processamento USING 96 TEXT-009.

*** Trata dados das linhas de trabalho
  DELETE t_mkal WHERE bdatu LT v_data
                   OR mdv01 IS INITIAL.
  SORT t_mkal BY matnr werks verid.
  DELETE ADJACENT DUPLICATES FROM t_mkal COMPARING matnr werks.
  IF NOT t_mkal[] IS INITIAL.
    SELECT crhd~objty crhd~objid crhd~arbpl crtx~ktext FROM crhd
           INNER JOIN crtx ON crtx~objty EQ 'A'
                          AND crtx~objid EQ crhd~objid
                          AND crtx~spras EQ sy-langu
           INTO TABLE t_crhd
           FOR ALL ENTRIES IN t_mkal
           WHERE crhd~objty EQ 'A'
             AND crhd~arbpl EQ t_mkal-mdv01.
    SORT t_crhd BY arbpl.
  ENDIF.

ENDFORM.                    " F_OBTEM_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       Processa dados obtidos
*----------------------------------------------------------------------*
FORM f_processa_dados.

  DATA: l_kalnr TYPE keko-kalnr, l_matnr TYPE mbewh-matnr,
        l_prop  TYPE ck_lbkum, l_bwkey TYPE mbewh-bwkey,
        l_filho TYPE ty_ckmlhd, l_bwtar TYPE mbewh-bwtar.

  "Processando dados...
  PERFORM f_indica_processamento USING 15 TEXT-010.

*** Finaliza dados de saída
  LOOP AT t_saida INTO e_saida.

*.. Busca dados da linha
    READ TABLE t_mkal INTO e_mkal WITH KEY matnr = e_saida-matnr_t
                                           werks = e_saida-bwkey_t
                                                     BINARY SEARCH.
    IF sy-subrc EQ 0.
      READ TABLE t_crhd INTO e_crhd WITH KEY arbpl = e_mkal-mdv01
                                                    BINARY SEARCH.
    ENDIF.

*.. Busca dados do material
    READ TABLE t_makt INTO e_makt WITH KEY matnr = e_saida-matnr_t
                                                     BINARY SEARCH.

*.. Busca dados da família
    READ TABLE t_mara INTO e_mara WITH KEY matnr = e_saida-matnr_t
                                                     BINARY SEARCH.
    READ TABLE t_t023t INTO e_t023t WITH KEY matkl = e_mara-matkl
                                                     BINARY SEARCH.
*.. Busca dados de Alternativas de suprimento
    READ TABLE t_mv001 INTO e_mv001 WITH KEY kalnr = e_saida-kalnr_prz
                                                     BINARY SEARCH.
*.. Trata item correspondente (Pai e Filho)
    IF NOT e_saida-matnr_r IS INITIAL OR
      NOT e_saida-kostl_r IS INITIAL.
      l_kalnr = e_saida-kalnr_r.
      l_matnr = e_saida-matnr_r.
      l_bwkey = e_saida-bwkey_r.
    ELSE.
      l_kalnr = e_saida-kalnr_t.
      l_matnr = e_saida-matnr_t.
      l_bwkey = e_saida-bwkey_t.
    ENDIF.

*.. Trata valor unitário
    READ TABLE t_ckmlhd INTO e_ckmlhd WITH KEY kalnr = e_saida-kalnr_t
                                                        BINARY SEARCH.
    IF sy-subrc EQ 0.
      READ TABLE t_mbewh INTO e_mbewh WITH KEY matnr = l_matnr
                                               bwkey = l_bwkey
                                               bwtar = e_ckmlhd-bwtar
                                               BINARY SEARCH.
      IF sy-subrc EQ 0.
        e_saida-vlu_std  = e_mbewh-stprs.
        e_saida-vlu_real = e_mbewh-verpr.
      ENDIF.
    ENDIF.

*.. Trata estratificação standard de todos os itens
    READ TABLE t_keko INTO e_keko WITH KEY kalnr = l_kalnr
                                             BINARY SEARCH.
    IF e_keko-losgr NE 0 AND e_saida-lbkum NE 0.
      l_prop = e_saida-lbkum / e_keko-losgr.

      LOOP AT t_keph INTO e_keph WHERE kalnr EQ l_kalnr
                                   AND kalka EQ e_keko-kalka
                                   AND kadky EQ e_keko-kadky
                                   AND tvers EQ e_keko-tvers
                                   AND bwvar EQ e_keko-bwvar.
        e_saida-tma_std   = e_saida-tma_std +
                          ( e_keph-kst001 + e_keph-kst002 ) * l_prop.
**        e_saida-mod_std   = e_saida-mod_std +
**                          ( e_keph-kst003 + e_keph-kst004 ) * l_prop.
      ENDLOOP.
    ELSEIF e_saida-kostl_r IS INITIAL.
      e_saida-tma_std = e_saida-lbkum * e_saida-vlu_std.
*      IF e_saida-tipo EQ 'F'.
*        e_saida-tma_real = e_saida-lbkum * e_saida-vlu_real.
*      ENDIF.
    ENDIF.

*.. Outras informações
    e_saida-matkl    = e_mara-matkl.
    e_saida-verid_nd = e_mv001-verid_nd.
    e_saida-wgbez    = e_t023t-wgbez.
    e_saida-maktx_t  = e_makt-maktx.
    e_saida-arbpl    = e_mkal-mdv01.
    e_saida-ktext    = e_crhd-ktext.

*.. Linha em branco
    IF e_saida-arbpl IS INITIAL.
      e_saida-arbpl = '-'.
    ENDIF.

*.. Trata item filho
    IF e_saida-tipo EQ 'F' OR e_saida-tipo EQ 'C'.
*.... Versão em branco
      IF e_saida-verid_nd IS INITIAL.
        e_saida-verid_nd = '-'.
      ENDIF.

*.... Tratamento de matérias S2 ou V2
      READ TABLE t_filhos INTO l_filho WITH KEY kalnr = e_saida-kalnr_r
                                                         BINARY SEARCH.
      IF sy-subrc EQ 0 AND l_filho-mlast NE '3'.
        READ TABLE t_ckmlcr INTO e_ckmlcr WITH KEY kalnr = l_filho-kalnr
                                                          BINARY SEARCH.
        IF e_ckmlcr-vprsv = 'S'.
          IF e_ckmlcr-stprs NE 0 AND e_ckmlcr-peinh NE 0.
            e_saida-tma_real
            = e_saida-lbkum * ( e_ckmlcr-stprs / e_ckmlcr-peinh ).
          ENDIF.
        ELSEIF e_ckmlcr-vprsv = 'V'.
          IF e_ckmlcr-pvprs NE 0 AND e_ckmlcr-peinh NE 0.
            e_saida-tma_real
            = e_saida-lbkum * ( e_ckmlcr-pvprs / e_ckmlcr-peinh ).
          ENDIF.
        ENDIF.
      ENDIF.

      IF e_saida-mlast IS INITIAL.
        e_saida-mlast = l_filho-mlast.
      ENDIF.
    ENDIF.

*.. Cálcula variações
    e_saida-tma_var = e_saida-tma_real - e_saida-tma_std.

*.. Cálcula valor total
**    e_saida-vlt_std  = e_saida-tma_std  + e_saida-mod_std + e_saida-ovh_std.
**    e_saida-vlt_real = e_saida-tma_real + e_saida-mod_real + e_saida-ovh_real.

    e_saida-vlt_std  = e_saida-tma_std + e_saida-mod_std.
    e_saida-vlt_real = e_saida-tma_real + e_saida-sal_real.

    MODIFY t_saida FROM e_saida.

    CLEAR: e_saida, e_mkal, e_crhd, e_makt, e_mara, e_t023t, l_kalnr,
           e_keko, e_keph, e_mv001, l_filho, e_ckmlcr, e_mbewh, l_matnr,
           l_bwkey, l_bwtar, e_ckmlhd.
  ENDLOOP.

  SORT: t_ckmlhd BY kalnr.

ENDFORM.                    " F_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_RELATORIO
*&---------------------------------------------------------------------*
*       Chama tela do relatório
*----------------------------------------------------------------------*
FORM f_exibe_relatorio.

*** Indicador do processamento (Saída)
  PERFORM f_indica_processamento USING 099 TEXT-001.
  "Preparando layout de saída...

*** Preenche FIELDCAT apartir da estrutura
  PERFORM f_preenche_fieldcalt_s.

** Exibe tela principal
  IF p_ver EQ 'X'.
    SORT t_saida BY arbpl matkl matnr_t verid_nd tipo DESCENDING
                                              mtart_r DESCENDING
                                              matnr_r ASCENDING
                                           otyp_inmat DESCENDING
                                           kalnr_r    ASCENDING.
  ELSE.
    SORT t_saida BY arbpl matkl matnr_t tipo DESCENDING
                                     mtart_r DESCENDING
                                     matnr_r ASCENDING
                                  otyp_inmat DESCENDING
                                  kalnr_r    ASCENDING.
  ENDIF.

  CALL SCREEN 9001.

ENDFORM.                    " F_EXIBE_RELATORIO
*&---------------------------------------------------------------------*
*&      Form  F_INDICA_PROCESSAMENTO
*&---------------------------------------------------------------------*
*       Indicador do processamento
*----------------------------------------------------------------------*
*      -->P_PERCENTAGE - Percentual do processamento
*      -->P_TEXT       - Texto do Processamento
*----------------------------------------------------------------------*
FORM f_indica_processamento USING p_percentage TYPE any
                                  p_text TYPE any.

  CHECK sy-batch IS INITIAL.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = p_percentage
      text       = p_text.

ENDFORM.                    " F_INDICA_PROCESSAMENTO
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_FIELDCALT_S
*&---------------------------------------------------------------------*
*       Preenche FIELDCAT apartir da estrutura
*----------------------------------------------------------------------*
FORM f_preenche_fieldcalt_s.

  DATA: l_fdcat   TYPE lvc_s_fcat,
        li_numcol TYPE int4 VALUE 0.

  REFRESH t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'MAKTX'.
  l_fdcat-ref_table             = 'MAKT'.
  l_fdcat-ref_field             = 'MAKTX'.
  l_fdcat-coltext               = 'Descrição do Material / C.Custo'.
  l_fdcat-outputlen             = 40.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'BWKEY'.
  l_fdcat-inttype               = 'C'.
  l_fdcat-intlen                = 4.
  l_fdcat-coltext               = 'Centro'.
  l_fdcat-outputlen             = 10.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'KALNR'.
  l_fdcat-inttype               = 'C'.
  l_fdcat-intlen                = 12.
  l_fdcat-coltext               = 'Nº CálCusto'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-outputlen             = 12.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'LBKUM'.
  l_fdcat-ref_table             = 'MLCD'.
  l_fdcat-ref_field             = 'LBKUM'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Estoq. Avaliado Total'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'MEINS'.
  l_fdcat-ref_table             = 'MARA'.
  l_fdcat-ref_field             = 'MEINS'.
  l_fdcat-coltext               = 'UnMed'.
  l_fdcat-outputlen             = 5.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'VLU_STD'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Vlr Unitário STD'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'VLU_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Vlr Unitário Real'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'VLT_STD'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Vlr Total STD'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'VLT_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Vlr Total Real'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'TMA_STD'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Tot. Material STD'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'TMA_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Material Extrat.'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'TMA_VAR'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Tot. Mat. Variação'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'SAL_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Sálario/Ordenados'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'ENC_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Encargos Sociais'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'CONT_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Contencioso Judicial'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'CONS_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Cons.Ger.de Energia'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'COMB_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Combust. e Lubrifican'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'MANU_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Manutenção'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'MSET_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Mat p Setores Divs'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'SERV_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Serviço de Terceiros'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'ALUG_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Aluguel e Arrendamen'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'LEAS_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Leasing e Consórcios'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'SEGU_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Seguros'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'SRV_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Srv Cons Audit Asses'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'SUPO_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Sup. Administrativo'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'COMU_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Comunicações'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'VIAG_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Viagens e Estadias'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'IMPO_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Impost Taxas e Contr'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'DOAC_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Doacões Patrocinios'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'DESP_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Despesas Gerais'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'DEPR_REAL'.
  l_fdcat-ref_table             = 'MBEWH'.
  l_fdcat-ref_field             = 'STPRS'.
  l_fdcat-no_zero               = 'X'.
  l_fdcat-lzero                 = 'X'.
  l_fdcat-coltext               = 'Depreciaç e Exaustão'.
  APPEND l_fdcat TO t_fdcat.

  CLEAR l_fdcat.
  ADD 1 TO li_numcol.
  l_fdcat-col_pos               = li_numcol.
  l_fdcat-fieldname             = 'WAERS'.
  l_fdcat-ref_table             = 'CKMLPRKEPH'.
  l_fdcat-ref_field             = 'WAERS'.
  l_fdcat-coltext               = 'Moeda'.
  l_fdcat-outputlen             = 5.
  APPEND l_fdcat TO t_fdcat.

ENDFORM.                    " F_PREENCHE_FIELDCALT_S
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       Comando do usuário
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  sy-ucomm = v_okcode.
  CLEAR v_okcode.

  CASE sy-ucomm.
    WHEN c_back. "Voltar
*.... Sai da tela
      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN c_ende OR c_canc. "Encerrar / Cancelar
*.... Sai do programa
      LEAVE PROGRAM.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_TEXTO_DOMINIO
*&---------------------------------------------------------------------*
*       Carrega texto do dominio
*----------------------------------------------------------------------*
FORM f_texto_dominio USING VALUE(p_dominio)  TYPE any
                           VALUE(p_domvalue) TYPE any
                  CHANGING VALUE(p_text)     TYPE any.

  DATA: l_domvalue TYPE dd07v-domvalue_l,
        l_dominio  TYPE dd07v-domname,
        l_text     TYPE ddtext.

  l_dominio  = p_dominio.
  l_domvalue = p_domvalue.

  CALL FUNCTION 'DOMAIN_VALUE_GET'
    EXPORTING
      i_domname  = l_dominio
      i_domvalue = l_domvalue
    IMPORTING
      e_ddtext   = l_text
    EXCEPTIONS
      not_exist  = 1
      OTHERS     = 2.

  IF sy-subrc EQ 0.
    p_text = l_text.
  ENDIF.

ENDFORM.                    " F_TEXTO_DOMINIO
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_PARAMETROS
*&---------------------------------------------------------------------*
*       Valida Parâmetros de entrada
*----------------------------------------------------------------------*
FORM f_valida_parametros.

*** Valida entradas obrigatórias
  IF s_matnr[] IS INITIAL AND s_werks[] IS INITIAL AND
     s_matkl[] IS INITIAL AND s_bwtar[] IS INITIAL AND
     s_mtart[] IS INITIAL.
    MESSAGE s013 DISPLAY LIKE 'E'.
    "Informe um dos parâmetros de "Materiais"
    STOP.                                                   "#EC *
  ENDIF.

ENDFORM.                    " F_VALIDA_PARAMETROS
*&---------------------------------------------------------------------*
*&      Form  F_RESTRINGE_LEDGER
*&---------------------------------------------------------------------*
*       Trata dados das partidas obtidas
*----------------------------------------------------------------------*
FORM f_restringe_ledger CHANGING lt_mara TYPE ty_mara_tab.

  DATA: l_tabix   TYPE sy-tabix VALUE 1,
        l_key     TYPE ty_ckmlhd,
        l_matobj  TYPE LINE OF ckmv0_matobj_tbl,
        lt_kalnr  TYPE ckml_t_inkalnr,
        lt_matobj TYPE ckmv0_matobj_tbl,
        lt_key    TYPE ty_ckmlhd_tab,
        lt_key_k  TYPE ty_ckmlhd_tab,
        lt_key_o  TYPE ty_ckmlhd_tab,
        lt_key_z  TYPE ty_ckmlhd_tab,
        lt_key_m  TYPE STANDARD TABLE OF ty_mbew,
        lt_mkal   TYPE STANDARD TABLE OF mkal,
        lr_untper TYPE RANGE OF mlcd-untper,
        lr_bvalt  TYPE RANGE OF mlcd-bvalt.

  "Obtendo dados...
  PERFORM f_indica_processamento USING 32 TEXT-009.

  SORT t_ckmlhd BY bwkey kalnr.

*** Processa itens pais
  DO.
    READ TABLE t_ckmlhd INTO e_ckmlhd INDEX l_tabix.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

*.. Busca informações das linhas de trabalho
    CALL FUNCTION 'MKAL_GENERIC_READ_MATNR_PLANT'
      EXPORTING
        matnr      = e_ckmlhd-matnr
        werks      = e_ckmlhd-bwkey
      TABLES
        mkal_tab   = lt_mkal
      EXCEPTIONS
        wrong_call = 1
        not_found  = 2
        OTHERS     = 3.
    IF sy-subrc EQ 0.
      APPEND LINES OF lt_mkal TO t_mkal.
      REFRESH lt_mkal.
    ENDIF.

*.. Busca lista técnica real do material (3.3)
    APPEND e_ckmlhd-kalnr TO lt_kalnr.
    PERFORM f_busca_lt_real USING e_ckmlhd lt_kalnr
                         CHANGING t_saida  lt_key_k lt_key_o lt_key_z.

*.. Alimenta chave para busca do item (3.4)
    l_matobj-kalnr = e_ckmlhd-kalnr.
    l_matobj-bwkey = e_ckmlhd-bwkey.
    l_matobj-matnr = e_ckmlhd-matnr.
    APPEND l_matobj TO lt_matobj.
    PERFORM f_busca_co_produtos USING lt_matobj
                             CHANGING lt_key_k lt_key_z.

*.. Busca estratificação dos itens pais (3.5)
    PERFORM f_busca_estrat_pais USING e_ckmlhd 'P'.

*.. Materiais para busca do texto
    e_mara-matnr = e_ckmlhd-matnr.
    COLLECT: e_mara INTO lt_mara.
    CLEAR: e_mara.

*.. Todos os itens (Pais e Filhos)
    COLLECT: e_ckmlhd INTO lt_key.

    ADD 1 TO l_tabix.

    CLEAR: e_ckmlhd, l_matobj.
    REFRESH: lt_kalnr, lt_matobj.
  ENDDO.

  "Obtendo dados...
  PERFORM f_indica_processamento USING 40 TEXT-009.

  IF NOT lt_key_k[] IS INITIAL.
*.. Obtém dados de Ledger de material: registro de cabeçalho
    SELECT kalnr mlast matnr bwkey bwtar
           FROM ckmlhd INTO TABLE t_filhos
           FOR ALL ENTRIES IN lt_key_k
           WHERE kalnr EQ lt_key_k-kalnr.

*.. Obtém quantidades de regs.totais do período
* ---> S4 Migration - 06/07/2023 - DG
*    SELECT kalnr bdatj poper untper abkumo zukumo
*           FROM ckmlpp INTO TABLE t_ckmlpp
*           FOR ALL ENTRIES IN lt_key_k
*           WHERE kalnr  EQ lt_key_k-kalnr
*             AND bdatj  EQ p_gjahr
*             AND poper  EQ p_period
*             AND untper EQ 000.

    DATA: wa_kalnr1 TYPE ckmv0_matobj_str,
          lt_kalnr1 TYPE ckmv0_matobj_tbl,
          lt_ckmlcr LIKE ckmlcr OCCURS 0 WITH HEADER LINE,
          lt_ckmlpp LIKE ckmlpp OCCURS 0 WITH HEADER LINE.

    DATA: lv_bdatj_1 TYPE  ckmlpp-bdatj,
          lv_poper_1 TYPE  ckmlpp-poper,
          lv_jahrper TYPE mldoc-jahrper.

    IF NOT lt_key_k[] IS INITIAL.

      lv_bdatj_1 = p_gjahr.
      lv_poper_1 = p_period.

      LOOP AT lt_key_k INTO DATA(wa_key_k).
        wa_kalnr1-kalnr = wa_key_k-kalnr.
        APPEND wa_kalnr1 TO lt_kalnr1.
      ENDLOOP.


      CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
        EXPORTING
          i_bdatj_1               = lv_bdatj_1
          i_poper_1               = lv_poper_1
        TABLES
          t_kalnr                 = lt_kalnr1
          t_ckmlpp                = lt_ckmlpp
          t_ckmlcr                = lt_ckmlcr
        EXCEPTIONS
          no_data_found           = 1
          input_data_inconsistent = 2
          buffer_inconsistent     = 3
          OTHERS                  = 4.


      IF lt_ckmlpp[] IS NOT INITIAL OR lt_ckmlcr[] IS NOT INITIAL.

        DELETE lt_ckmlpp WHERE untper NE 000.

        DELETE lt_ckmlcr WHERE untper NE '000'
                           AND curtp  NE p_curtp.

        LOOP AT lt_ckmlpp INTO DATA(wa_ckmlpp).
          MOVE-CORRESPONDING wa_ckmlpp TO e_ckmlpp.
          APPEND e_ckmlpp TO t_ckmlpp.
        ENDLOOP.

        LOOP AT lt_ckmlcr INTO DATA(wa_ckmlcr).
          MOVE-CORRESPONDING wa_ckmlcr TO e_ckmlcr.
          APPEND e_ckmlcr TO t_ckmlcr.
        ENDLOOP.

        SORT t_ckmlcr.

      ENDIF.
    ENDIF.
* <--- S4 Migration - 06/07/2023 - DG


    SORT: t_filhos BY kalnr,
          t_ckmlpp BY kalnr bdatj poper untper.
    REFRESH: lt_key_k.
  ENDIF.
**  SORT: t_mlcd BY kalnr untper bvalt.

  IF NOT lt_key_z[] IS INITIAL.
*.. Obtém Alternativas de suprimento
    SELECT kalnr verid_nd FROM ckmlmv001 INTO TABLE t_mv001
           FOR ALL ENTRIES IN lt_key_z
           WHERE kalnr EQ lt_key_z-kalnr.
    SORT t_mv001 BY kalnr.
  ENDIF.

  "Obtendo dados...
  PERFORM f_indica_processamento USING 48 TEXT-009.

  IF NOT lt_key_o[] IS INITIAL.
*.. Obtém Ledger de materiais: estratificação (cabeçalho) para preços
    SELECT * FROM ckmlprkeko INTO TABLE t_ckmlke
             FOR ALL ENTRIES IN lt_key_o
             WHERE kalnr  EQ lt_key_o-kalnr
               AND bdatj  EQ p_gjahr
               AND poper  EQ p_period
               AND untper EQ '000'
               AND ( prtyp  EQ 'A' OR prtyp  EQ 'B' )
               AND curtp  EQ p_curtp.

*.. Obtém Ledger de materiais: estratificação (elementos) para preços
    SELECT kalnr bdatj poper untper keart prtyp kkzst patnr dipa curtp
           waers kst001 kst002 kst003 kst004 kst005 kst006 kst007 kst008
           kst009 kst010 kst011 kst012 kst013 kst014 kst015 kst016 kst017
           kst018 kst019 kst020 kst021 kst022 kst023 kst024 kst025 kst026
           kst027 kst028 kst029 kst030 kst031 kst032 kst033 kst034 kst035
           kst036 kst037 kst038
           FROM ckmlprkeph INTO TABLE t_ckmlph
             FOR ALL ENTRIES IN lt_key_o
             WHERE kalnr  EQ lt_key_o-kalnr
               AND bdatj  EQ p_gjahr
               AND poper  EQ p_period
               AND untper EQ '000'
               AND keart  EQ 'H'
               AND ( prtyp  EQ 'A' OR prtyp EQ 'B' )
               AND kkzst  EQ space
               AND curtp  EQ p_curtp.

    SORT: t_ckmlke BY kalnr prtyp,
          t_ckmlph BY kalnr prtyp.
  ENDIF.

  "Obtendo dados...
  PERFORM f_indica_processamento USING 56 TEXT-009.

*** Processa itens filhos (3.6)
  l_tabix = 1.
  DO.
    READ TABLE t_filhos INTO e_ckmlhd INDEX l_tabix.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    CASE e_ckmlhd-mlast.
      WHEN '3'.
*...... Busca estratificação dos itens pais (3.5)
        PERFORM f_busca_estrat_pais USING e_ckmlhd 'F'.

      WHEN OTHERS.  "tratamento de matérias (S2, V2)
        e_mbew-matnr = e_ckmlhd-matnr.
        e_mbew-bwkey = e_ckmlhd-bwkey.
        e_mbew-bwtar = e_ckmlhd-bwtar.
        COLLECT: e_mbew INTO lt_key_m.
        CLEAR: e_mbew.

        l_key-kalnr = e_ckmlhd-kalnr.
        l_key-matnr = e_ckmlhd-matnr.
        l_key-bwkey = e_ckmlhd-bwkey.
        l_key-bwtar = e_ckmlhd-bwtar.
        COLLECT: l_key INTO lt_key_k.
        CLEAR: l_key.
    ENDCASE.

*.. Todos os itens (Pais e Filhos)
    COLLECT: e_ckmlhd INTO lt_key.

    ADD 1 TO l_tabix.

    CLEAR: e_ckmlhd, l_key.
    REFRESH: lt_kalnr.
  ENDDO.

  "Obtendo dados...
  PERFORM f_indica_processamento USING 64 TEXT-009.

*** Processa itens filhos - Somente Atividade (3.7)
  l_tabix = 1.
  DO.
    READ TABLE lt_key_o INTO e_ckmlhd INDEX l_tabix.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

*.. Processa consumos de atividade
    PERFORM f_processa_atividade USING e_ckmlhd.

*.. Todos os itens (Pais e Filhos)
    COLLECT: e_ckmlhd INTO lt_key.

    ADD 1 TO l_tabix.

    CLEAR: e_ckmlhd.
  ENDDO.

  "Obtendo dados...
  PERFORM f_indica_processamento USING 72 TEXT-009.

*** Obtém dados para Avaliação do material
  IF NOT lt_key_m[] IS INITIAL.
    SELECT matnr bwkey bwtar vprsv mlast FROM mbew INTO TABLE t_mbew
           FOR ALL ENTRIES IN lt_key_m
           WHERE matnr EQ lt_key_m-matnr
             AND bwkey EQ lt_key_m-bwkey
             AND bwtar EQ lt_key_m-bwtar.
    PERFORM f_restringe_mbew CHANGING lt_key_k.
  ENDIF.

* ---> S4 Migration - 06/07/2023 - DG
*  IF NOT lt_key_k[] IS INITIAL.
**.. Obtém Ledger de material: valores dos regs.totais do período
*    SELECT kalnr bdatj poper untper curtp peinh vprsv stprs pvprs
*           FROM ckmlcr INTO TABLE t_ckmlcr
*           FOR ALL ENTRIES IN lt_key_k
*           WHERE kalnr  EQ lt_key_k-kalnr
*             AND bdatj  EQ p_gjahr
*             AND poper  EQ p_period
*             AND untper EQ '000'
*             AND curtp  EQ p_curtp.
*
*    SORT t_ckmlcr.
*  ENDIF.
* <--- S4 Migration - 06/07/2023 - DG

  "Obtendo dados...
  PERFORM f_indica_processamento USING 80 TEXT-009.

  IF NOT lt_key[] IS INITIAL.
*.. Obtém Cálculo de custos do produto - informações de cabeçalho
    SELECT bzobj kalnr kalka kadky tvers bwvar kkzma loekz losgr meins
           FROM keko INTO TABLE t_keko
           FOR ALL ENTRIES IN lt_key
           WHERE bzobj   EQ '0'
             AND kalnr   EQ lt_key-kalnr
             AND kadky   LT v_data
             AND feh_sta IN r_feh_sta.
    IF sy-subrc EQ 0.
*.... Elementos de custos de produção do custeio de produto
      SELECT kalnr kalka kadky tvers bwvar kkzma kst001 kst002 kst003
             kst004 kst005 kst006 kst007 kst008 kst009 kst010 kst011
             kst012 kst013 kst014 kst015 kst016 kst017 kst018 kst019
             kst020 kst021 kst022 kst023 kst024 kst025 kst026 kst027
             kst028 kst029 kst030 kst031 kst032 kst033 kst034 kst035
             kst036 kst037 kst038
             FROM keph INTO TABLE t_keph
             FOR ALL ENTRIES IN t_keko
             WHERE bzobj EQ t_keko-bzobj
               AND kalnr EQ t_keko-kalnr
               AND kalka EQ t_keko-kalka
               AND kadky EQ t_keko-kadky
               AND tvers EQ t_keko-tvers
               AND bwvar EQ t_keko-bwvar
               AND kkzma EQ space
               AND keart EQ 'H'.

      SORT: t_keko BY kalnr ASCENDING kadky DESCENDING,
            t_keph BY kalnr kalka kadky tvers bwvar.
    ENDIF.

*.. Obtém Avaliação de material - histórico
    SELECT matnr bwkey bwtar verpr stprs
           FROM mbewh INTO TABLE t_mbewh
           FOR ALL ENTRIES IN lt_key
           WHERE matnr EQ lt_key-matnr
             AND bwkey EQ lt_key-bwkey
             AND bwtar EQ lt_key-bwtar
             AND lfgja EQ p_gjahr
             AND lfmon EQ p_period.
    SORT t_mbewh BY matnr bwkey bwtar.
  ENDIF.

ENDFORM.                    " F_RESTRINGE_LEDGER
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_LT_REAL
*&---------------------------------------------------------------------*
*       Busca lista técnica real do material
*----------------------------------------------------------------------*
FORM f_busca_lt_real USING VALUE(p_ckmlhd)  TYPE ty_ckmlhd
                           VALUE(pt_kalnr)  TYPE ckml_t_inkalnr
                  CHANGING VALUE(pt_saida)  LIKE t_saida
                           VALUE(pt_ckmlhd) TYPE ty_ckmlhd_tab
                           VALUE(pt_keko)   TYPE ty_ckmlhd_tab
                           VALUE(pt_mv001)  TYPE ty_ckmlhd_tab.

  DATA: lt_et_out   TYPE STANDARD TABLE OF ckmlmv003,
        lt_et_in    TYPE STANDARD TABLE OF ckmlmv004,
        l_text      TYPE ckml_activity_info_str,
        l_et_out    TYPE ckmlmv003,
        l_et_in     TYPE ckmlmv004,
        l_ckmlhd    TYPE ty_ckmlhd,
        l_qbr_kostl TYPE kostl,
        l_txt_kostl TYPE kltxt.

  DATA: BEGIN OF lt_mtart OCCURS 0,
          matnr TYPE matnr,
          mtart TYPE mtart,
          mtbez TYPE mtbez,
          maktx TYPE maktx,
        END   OF lt_mtart.

  CHECK NOT pt_kalnr IS INITIAL.

*** Seleção da lista técnica real do material
  CALL FUNCTION 'CKML_MGV_BOM_READ_KALNR'
    EXPORTING
      i_mgtyp             = c_mgtyp
      i_gjahr             = p_gjahr
      i_perio             = p_period
      i_bwkey             = p_ckmlhd-bwkey
      it_kalnr            = pt_kalnr
    TABLES
      et_out              = lt_et_out
      et_in               = lt_et_in
    EXCEPTIONS
      bwkey_not_found     = 1
      plant_not_found     = 2
      last_pday_not_found = 3
      OTHERS              = 4.

  CHECK: sy-subrc EQ 0.

  SELECT t1~matnr t2~mtart t2~mtbez t3~maktx
    INTO TABLE lt_mtart
    FROM mara AS t1
   INNER JOIN t134t AS t2
      ON t2~spras = sy-langu
     AND t2~mtart = t1~mtart
   INNER JOIN makt AS t3
      ON t3~spras = sy-langu
     AND t3~matnr = t1~matnr
     FOR ALL ENTRIES IN lt_et_in
   WHERE t1~matnr = lt_et_in-matnr.

  SORT lt_mtart BY matnr mtart.

  LOOP AT lt_et_in INTO l_et_in.
    e_saida-tipo       = 'F'.
    e_saida-matnr_t    = p_ckmlhd-matnr.
    e_saida-bwkey_t    = p_ckmlhd-bwkey.
    e_saida-kalnr_t    = p_ckmlhd-kalnr.
    e_saida-matnr_r    = l_et_in-matnr.
    e_saida-bwkey_r    = l_et_in-bwkey.
    e_saida-kalnr_r    = l_et_in-kalnr_inmat.
    e_saida-otyp_inmat = l_et_in-otyp_inmat.
    e_saida-otyp_prz   = l_et_in-otyp_prz.
    e_saida-meins      = l_et_in-meinh.
    e_saida-lbkum      = l_et_in-in_menge.
    READ TABLE lt_mtart WITH KEY matnr = l_et_in-matnr
                                 BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      e_saida-mtart_r = lt_mtart-mtart.
      e_saida-mtbez_r = lt_mtart-mtbez.
      e_saida-maktx_r = lt_mtart-maktx.
    ENDIF.

    IF p_ver EQ 'X'.
      READ TABLE lt_et_out INTO l_et_out WITH KEY
                     kalnr_in = l_et_in-kalnr_prz.
      l_ckmlhd-kalnr = e_saida-kalnr_prz = l_et_out-kalnr_bal.
      COLLECT: l_ckmlhd INTO pt_mv001.
    ENDIF.

    l_ckmlhd-kalnr = l_et_in-kalnr_inmat.
*.... Apenas registros Tipo (Req.) = "MB"
    IF l_et_in-otyp_inmat EQ 'MB'.
      COLLECT: l_ckmlhd INTO pt_ckmlhd.
*.... Apenas registros Tipo (Req.) = "LA"
    ELSEIF l_et_in-otyp_inmat EQ 'LA'.
      l_ckmlhd-matnr = l_et_in-matnr.
      l_ckmlhd-bwkey = l_et_in-bwkey.
      l_ckmlhd-bwtar = l_et_in-bwtar.
      COLLECT: l_ckmlhd INTO pt_keko.

      CALL FUNCTION 'CKML_LA_ACTIVITY_INFO'
        EXPORTING
          i_kalnr             = e_saida-kalnr_r
          i_date              = l_et_in-last_pday
        IMPORTING
          e_text              = l_text
        EXCEPTIONS
          no_valid_info_found = 1
          OTHERS              = 2.

      IF sy-subrc EQ 0.
        e_saida-kostl_r = l_text-kostl.
*       Busca descrição do Centro de Custo
        IF l_qbr_kostl <> l_text-kostl.
          l_qbr_kostl = l_text-kostl.
          CLEAR l_txt_kostl.
          SELECT ltext
            INTO l_txt_kostl
            FROM cskt
              UP TO 1 ROWS
           WHERE spras = sy-langu
             AND kokrs = 'MAGI'
             AND kostl = l_text-kostl
             AND datbi >= sy-datum.
          ENDSELECT.
        ENDIF.
        e_saida-ltext_r = l_txt_kostl.
      ENDIF.
    ENDIF.

    COLLECT: e_saida INTO pt_saida.

    CLEAR: l_ckmlhd, e_saida, l_et_out, l_text.
  ENDLOOP.

ENDFORM.                    " F_BUSCA_LT_REAL
*&---------------------------------------------------------------------*
*&      Form  F_TRATA_NODES
*&---------------------------------------------------------------------*
*       Trata tabela de nó a ser expandido ou comprimido
*----------------------------------------------------------------------*
FORM f_trata_nodes USING VALUE(p_ucomm)  TYPE char01
                CHANGING VALUE(pt_nodes) TYPE lvc_t_nkey.

  CASE p_ucomm.
    WHEN 'E'. "Expandir
      LOOP AT t_nodes INTO e_nodes WHERE nivel EQ e_nivel-enivel.
        APPEND e_nodes-nodek TO pt_nodes.
      ENDLOOP.
      e_nivel-cnivel = e_nivel-enivel.
      CHECK e_nivel-enivel LT v_nmax.
      ADD 1 TO e_nivel-enivel.

    WHEN 'C'. "Comprimir
      LOOP AT t_nodes INTO e_nodes WHERE nivel EQ e_nivel-cnivel.
        APPEND e_nodes-nodek TO pt_nodes.
      ENDLOOP.
      e_nivel-enivel = e_nivel-cnivel.
      CHECK e_nivel-cnivel GT 1.
      SUBTRACT 1 FROM e_nivel-cnivel.

    WHEN 'A'. "Expandir tudo
      LOOP AT t_nodes INTO e_nodes.
        APPEND e_nodes-nodek TO pt_nodes.
      ENDLOOP.
      e_nivel-enivel = e_nivel-cnivel = v_nmax.

    WHEN 'B'. "Comprimir tudo
      e_nivel-enivel = e_nivel-cnivel = 1.

    WHEN OTHERS.
  ENDCASE.

  CLEAR: e_nodes.

ENDFORM.                    " F_TRATA_NODES
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_CO_PRODUTOS
*&---------------------------------------------------------------------*
*       Busca co-produtos
*----------------------------------------------------------------------*
FORM f_busca_co_produtos USING VALUE(pt_mats)   TYPE ckmv0_matobj_tbl
                      CHANGING VALUE(pt_ckmlhd) TYPE ty_ckmlhd_tab
                               VALUE(pt_mv001)  TYPE ty_ckmlhd_tab.

  DATA: lt_et_out TYPE STANDARD TABLE OF ckmlmv003,
        lt_et_in  TYPE STANDARD TABLE OF ckmlmv004,
        lt_mv001  TYPE STANDARD TABLE OF ty_ckmlmv001,
        l_et_out  TYPE ckmlmv003,
        l_et_in   TYPE ckmlmv004,
        l_ckmlhd  TYPE ty_ckmlhd.

  CHECK NOT pt_mats[] IS INITIAL.

*** Seleção dos "co-produtos"
  SELECT kalnr otyp bwkey werks matnr bwtar
         FROM ckmlmv001 INTO TABLE lt_mv001
         FOR ALL ENTRIES IN pt_mats
         WHERE werks EQ pt_mats-bwkey
           AND matnr EQ pt_mats-matnr
           AND btyp  EQ 'BF'
           AND otyp  EQ 'PR'.
  CHECK sy-subrc EQ 0.

*** Seleção de entradas/estratégias mistas
  SELECT * FROM ckmlmv003 INTO TABLE lt_et_out
         FOR ALL ENTRIES IN lt_mv001
         WHERE mgtyp     EQ '00001'
           AND gjahr     EQ p_gjahr
           AND perio     EQ p_period
           AND kalnr_out EQ lt_mv001-kalnr.
  CHECK sy-subrc EQ 0.

  LOOP AT lt_et_in INTO l_et_in.
*.. Não pegar transferências
    CHECK e_ckmlhd-matnr NE l_et_in-matnr.

    e_saida-tipo       = 'C'.
    e_saida-matnr_t    = e_ckmlhd-matnr.
    e_saida-bwkey_t    = e_ckmlhd-bwkey.
    e_saida-kalnr_t    = e_ckmlhd-kalnr.
    e_saida-matnr_r    = l_et_in-matnr.
    e_saida-bwkey_r    = l_et_in-bwkey.
    e_saida-kalnr_r    = l_et_in-kalnr_inmat.
    e_saida-otyp_inmat = l_et_in-otyp_inmat.
    e_saida-otyp_prz   = l_et_in-otyp_prz.
    e_saida-meins      = l_et_in-meinh.
    e_saida-lbkum      = l_et_in-in_menge * -1.

    IF p_ver EQ 'X'.
      READ TABLE lt_et_out INTO l_et_out WITH KEY
                     kalnr_in = l_et_in-kalnr_prz.
      l_ckmlhd-kalnr = e_saida-kalnr_prz = l_et_out-kalnr_bal.
      COLLECT: l_ckmlhd INTO pt_mv001.
    ENDIF.

*.. Apenas registros Tipo (Req.) = "MB"
    IF l_et_in-otyp_inmat EQ 'MB'.
      l_ckmlhd-kalnr = l_et_in-kalnr_inmat.
      COLLECT: l_ckmlhd INTO pt_ckmlhd.
    ENDIF.

    COLLECT: e_saida INTO t_saida.

    CLEAR: l_ckmlhd, e_saida.
  ENDLOOP.

ENDFORM.                    " F_BUSCA_CO_PRODUTOS
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_ESTRAT_PAIS
*&---------------------------------------------------------------------*
*       Busca estratificação dos itens pais
*----------------------------------------------------------------------*
FORM f_busca_estrat_pais USING VALUE(p_ckmlhd) TYPE ty_ckmlhd
                               VALUE(p_tipo)   TYPE c.

  DATA: lr_keart     TYPE RANGE OF ckmlkeph-keart,
        lr_mlcct     TYPE RANGE OF ckmlkeph-mlcct,
        lr_kkzst     TYPE RANGE OF ckmlkeph-kkzst,
        lr_curtp     TYPE RANGE OF ckmlcr-curtp,
        lt_keph_mlcd TYPE ccs01_t_keph_mlcd,
        lt_kalnr     TYPE ckmv0_matobj_tbl,
        l_keart      LIKE LINE OF lr_keart,
        l_mlcct      LIKE LINE OF lr_mlcct,
        l_kkzst      LIKE LINE OF lr_kkzst,
        l_curtp      LIKE LINE OF lr_curtp,
        l_keph_mlcd  TYPE ccs01_keph_mlcd,
        l_kalnr      TYPE ckmv0_matobj_str,
        l_tabix      TYPE sy-tabix,
        l_total      TYPE ck_umkum,
        l_prop       TYPE f.

*****  IF p_tipo = 'P'.  "Item Pai
******.. Verifica se teve qtde. produzida
*****    READ TABLE t_mlcd INTO e_mlcd WITH KEY kalnr = p_ckmlhd-kalnr
*****                                                    BINARY SEARCH.
*****    CHECK sy-subrc EQ 0.
*****  ENDIF.

  l_kalnr-kalnr = p_ckmlhd-kalnr.
  l_kalnr-bwkey = p_ckmlhd-bwkey.
  APPEND l_kalnr TO lt_kalnr.

  l_keart-sign   = 'I'.
  l_keart-option = 'EQ'.
  l_keart-low    = 'H'.
  APPEND l_keart TO lr_keart.

  l_mlcct-sign   = 'I'.
  l_mlcct-option = 'EQ'.
  l_mlcct-low    = space.
  APPEND l_mlcct TO lr_mlcct.

  l_kkzst-sign   = 'I'.
  l_kkzst-option = 'EQ'.
  l_kkzst-low    = space.
  APPEND l_kkzst TO lr_kkzst.

  l_curtp-sign   = 'I'.
  l_curtp-option = 'EQ'.
  l_curtp-low    = p_curtp.
  APPEND l_curtp TO lr_curtp.

*** Ledger de materiais: estratificação (elementos)
  CALL FUNCTION 'MLCCS_KEPH_MLCD_READ'
    EXPORTING
      i_refresh_buffer = 'X'
      it_kalnr         = lt_kalnr
      i_from_bdatj     = p_gjahr
      i_from_poper     = p_period
      ir_keart         = lr_keart
      ir_mlcct         = lr_mlcct
      ir_kkzst         = lr_kkzst
      ir_curtp         = lr_curtp
    IMPORTING
      et_keph_mlcd     = lt_keph_mlcd.

  IF p_tipo = 'P'.  "Item Pai
    LOOP AT lt_keph_mlcd INTO l_keph_mlcd WHERE categ = 'ZU'
                                            AND ptyp  = 'BF'.
*.... Para os registros selecionados, totalizar
      ckmlkeph-kst001 = ckmlkeph-kst001 + l_keph_mlcd-kst001.
      ckmlkeph-kst002 = ckmlkeph-kst002 + l_keph_mlcd-kst002.
      ckmlkeph-kst003 = ckmlkeph-kst003 + l_keph_mlcd-kst003.
      ckmlkeph-kst004 = ckmlkeph-kst004 + l_keph_mlcd-kst004.
      ckmlkeph-kst005 = ckmlkeph-kst005 + l_keph_mlcd-kst005.
      ckmlkeph-kst006 = ckmlkeph-kst006 + l_keph_mlcd-kst006.
      ckmlkeph-kst007 = ckmlkeph-kst007 + l_keph_mlcd-kst007.
      ckmlkeph-kst008 = ckmlkeph-kst008 + l_keph_mlcd-kst008.
      ckmlkeph-kst009 = ckmlkeph-kst009 + l_keph_mlcd-kst009.
      ckmlkeph-kst010 = ckmlkeph-kst010 + l_keph_mlcd-kst010.
      ckmlkeph-kst011 = ckmlkeph-kst011 + l_keph_mlcd-kst011.
      ckmlkeph-kst012 = ckmlkeph-kst012 + l_keph_mlcd-kst012.
      ckmlkeph-kst013 = ckmlkeph-kst013 + l_keph_mlcd-kst013.
      ckmlkeph-kst014 = ckmlkeph-kst014 + l_keph_mlcd-kst014.
      ckmlkeph-kst015 = ckmlkeph-kst015 + l_keph_mlcd-kst015.
      ckmlkeph-kst016 = ckmlkeph-kst016 + l_keph_mlcd-kst016.
      ckmlkeph-kst017 = ckmlkeph-kst017 + l_keph_mlcd-kst017.
      ckmlkeph-kst018 = ckmlkeph-kst018 + l_keph_mlcd-kst018.
      ckmlkeph-kst019 = ckmlkeph-kst019 + l_keph_mlcd-kst019.
      ckmlkeph-kst020 = ckmlkeph-kst020 + l_keph_mlcd-kst020.
      ckmlkeph-kst021 = ckmlkeph-kst021 + l_keph_mlcd-kst021.
      ckmlkeph-kst022 = ckmlkeph-kst022 + l_keph_mlcd-kst022.
      ckmlkeph-kst023 = ckmlkeph-kst023 + l_keph_mlcd-kst023.
      ckmlkeph-kst024 = ckmlkeph-kst024 + l_keph_mlcd-kst024.
      ckmlkeph-kst025 = ckmlkeph-kst025 + l_keph_mlcd-kst025.
      ckmlkeph-kst026 = ckmlkeph-kst026 + l_keph_mlcd-kst026.
      ckmlkeph-kst027 = ckmlkeph-kst027 + l_keph_mlcd-kst027.
      ckmlkeph-kst028 = ckmlkeph-kst028 + l_keph_mlcd-kst028.
      ckmlkeph-kst029 = ckmlkeph-kst029 + l_keph_mlcd-kst029.
      ckmlkeph-kst030 = ckmlkeph-kst030 + l_keph_mlcd-kst030.
      ckmlkeph-kst031 = ckmlkeph-kst031 + l_keph_mlcd-kst031.
      ckmlkeph-kst032 = ckmlkeph-kst032 + l_keph_mlcd-kst032.
      ckmlkeph-kst033 = ckmlkeph-kst033 + l_keph_mlcd-kst033.
      ckmlkeph-kst034 = ckmlkeph-kst034 + l_keph_mlcd-kst034.
      ckmlkeph-kst035 = ckmlkeph-kst035 + l_keph_mlcd-kst035.
      ckmlkeph-kst036 = ckmlkeph-kst036 + l_keph_mlcd-kst036.
      ckmlkeph-kst037 = ckmlkeph-kst037 + l_keph_mlcd-kst037.
      ckmlkeph-kst038 = ckmlkeph-kst038 + l_keph_mlcd-kst038.
    ENDLOOP.

*.. Atribui dados do item Pai
    e_saida-tipo       = 'P'.
    e_saida-matnr_t    = p_ckmlhd-matnr.
    e_saida-bwkey_t    = p_ckmlhd-bwkey.
    e_saida-kalnr_t    = p_ckmlhd-kalnr.
    e_saida-meins      = e_mlcd-meins.
    e_saida-lbkum      = e_mlcd-lbkum.
    e_saida-waers      = e_mlcd-waers.

    e_saida-tma_real   = ckmlkeph-kst001.                   "Total Mat. – Real
    e_saida-sal_real   = ckmlkeph-kst002 + ckmlkeph-kst003. "Sálario/Ordenados
    e_saida-enc_real   = ckmlkeph-kst004 + ckmlkeph-kst005. "Encargos Sociais
    e_saida-cont_real  = ckmlkeph-kst006 + ckmlkeph-kst007. "Contencioso Judicial
    e_saida-cons_real  = ckmlkeph-kst008 + ckmlkeph-kst009. "Cons.Ger.de Energia
    e_saida-comb_real  = ckmlkeph-kst010 + ckmlkeph-kst011. "Combust. e Lubrifican
    e_saida-manu_real  = ckmlkeph-kst012 + ckmlkeph-kst013. "Manutenção
    e_saida-mset_real  = ckmlkeph-kst014 + ckmlkeph-kst015. "Mat p Setores Divs
    e_saida-serv_real  = ckmlkeph-kst016 + ckmlkeph-kst017. "Serviço de Terceiros
    e_saida-alug_real  = ckmlkeph-kst018 + ckmlkeph-kst019. "Aluguel e Arrendamen
    e_saida-leas_real  = ckmlkeph-kst020 + ckmlkeph-kst021. "Leasing e Consórcios
    e_saida-segu_real  = ckmlkeph-kst022 + ckmlkeph-kst023. "Seguros
    e_saida-srv_real   = ckmlkeph-kst024 + ckmlkeph-kst025. "Srv Cons Audit Asses
    e_saida-supo_real  = ckmlkeph-kst026 + ckmlkeph-kst027. "Sup. Administrativo
    e_saida-comu_real  = ckmlkeph-kst028 + ckmlkeph-kst029. "Comunicações
    e_saida-viag_real  = ckmlkeph-kst030 + ckmlkeph-kst031. "Viagens e Estadias
    e_saida-impo_real  = ckmlkeph-kst032 + ckmlkeph-kst033. "Impost Taxas e Contr
    e_saida-doac_real  = ckmlkeph-kst034 + ckmlkeph-kst035. "Doacões Patrocinios
    e_saida-desp_real  = ckmlkeph-kst036 + ckmlkeph-kst037. "Despesas Gerais
    e_saida-depr_real  = ckmlkeph-kst038 + ckmlkeph-kst039. "Depreciaç e Exaustão

    COLLECT e_saida INTO t_saida.

    CLEAR: ckmlkeph, e_saida.

  ELSEIF p_tipo = 'F'.  "Item Filho
    LOOP AT t_saida INTO e_saida WHERE tipo       = 'F'
                                   AND kalnr_r    = p_ckmlhd-kalnr.
      l_tabix = sy-tabix.
      CHECK e_saida-lbkum NE 0.
      READ TABLE t_ckmlpp INTO e_ckmlpp WITH KEY kalnr  = e_saida-kalnr_r
                                                 bdatj  = p_gjahr
                                                 poper  = p_period
                                                 untper = 000
                                                 BINARY SEARCH.
      l_total = e_ckmlpp-abkumo + e_ckmlpp-zukumo.
      IF l_total IS INITIAL.
        l_prop = 1.
      ELSE.
        l_prop = e_saida-lbkum / l_total.
      ENDIF.

      LOOP AT lt_keph_mlcd INTO l_keph_mlcd
        WHERE ( categ = 'ZU' OR categ = 'AB' ) AND ( ptyp = space )
          AND objtyp = 'CA'.
*        WHERE ( categ = 'ZU' AND ptyp = 'BF' ) OR ( categ = 'AB' ).

*...... Para os registros selecionados, totalizar
        ckmlkeph-kst001 = ckmlkeph-kst001 + l_keph_mlcd-kst001.
*        ckmlkeph-kst002 = ckmlkeph-kst002 + l_keph_mlcd-kst002.
        ckmlkeph-kst003 = ckmlkeph-kst003 + l_keph_mlcd-kst003.
*        ckmlkeph-kst004 = ckmlkeph-kst004 + l_keph_mlcd-kst004.
        ckmlkeph-kst005 = ckmlkeph-kst005 + l_keph_mlcd-kst005.
*        ckmlkeph-kst006 = ckmlkeph-kst006 + l_keph_mlcd-kst006.
        ckmlkeph-kst007 = ckmlkeph-kst007 + l_keph_mlcd-kst007.
*        ckmlkeph-kst008 = ckmlkeph-kst008 + l_keph_mlcd-kst008.

      ENDLOOP.

*.... Atribui dados do item Filho
      e_saida-waers      = e_mlcd-waers.
      e_saida-tma_real   = ( ckmlkeph-kst001 + ckmlkeph-kst002 ) * l_prop.
      e_saida-mlast      = p_ckmlhd-mlast.

      MODIFY t_saida FROM e_saida INDEX l_tabix TRANSPORTING
             waers mlast tma_real.

      CLEAR: ckmlkeph, e_saida, e_ckmlpp, l_total, l_prop.
    ENDLOOP.
  ENDIF.

  CLEAR: e_mlcd, e_ckmlpp.

ENDFORM.                    " F_BUSCA_ESTRAT_PAIS
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_ATIVIDADE
*&---------------------------------------------------------------------*
*       Processa consumos de atividade
*----------------------------------------------------------------------*
FORM f_processa_atividade USING VALUE(p_ckmlhd) TYPE ty_ckmlhd.

  DATA: l_tabix TYPE sy-tabix,
        l_prop  TYPE f.

*** Processa dados de saída (somente os consumos de atividade)
  LOOP AT t_saida INTO e_saida WHERE tipo       = 'F'
                                 AND kalnr_r    = p_ckmlhd-kalnr
                                 AND otyp_inmat = 'LA'.
    l_tabix = sy-tabix.

    LOOP AT t_ckmlph INTO e_ckmlph WHERE kalnr EQ p_ckmlhd-kalnr.
*.... Busca estratificação (cabeçalho) para preços
      READ TABLE t_ckmlke INTO e_ckmlke WITH KEY kalnr = p_ckmlhd-kalnr
                                                 prtyp = e_ckmlph-prtyp
                                                          BINARY SEARCH.
      CHECK sy-subrc EQ 0.
      CHECK e_ckmlke-losgr NE 0 AND e_saida-lbkum NE 0.
      l_prop = ( e_saida-lbkum / e_ckmlke-losgr ).

      CASE e_ckmlph-prtyp.
        WHEN 'A'.
          e_saida-vlu_std  = ( e_ckmlph-kst001 + e_ckmlph-kst002 +
                               e_ckmlph-kst004 + e_ckmlph-kst006 +
                               e_ckmlph-kst008 + e_ckmlph-kst010 +
                               e_ckmlph-kst012 + e_ckmlph-kst014 +
                               e_ckmlph-kst016 + e_ckmlph-kst018 +
                               e_ckmlph-kst020 + e_ckmlph-kst022 +
                               e_ckmlph-kst024 + e_ckmlph-kst026 +
                               e_ckmlph-kst028 + e_ckmlph-kst030 +
                               e_ckmlph-kst032 + e_ckmlph-kst034 +
                               e_ckmlph-kst036 + e_ckmlph-kst038 ) /
                               e_ckmlke-losgr.

          e_saida-tma_std   = e_saida-tma_std + (
                            ( e_ckmlph-kst001 ) * l_prop ).

          e_saida-mod_std   = e_saida-mod_std + (
                            ( e_ckmlph-kst003 ) * l_prop ).

          e_saida-waers      = e_ckmlph-waers.

          e_saida-tma_real   = e_saida-tma_std.

        WHEN 'B'.
          e_saida-vlu_real = ( ( e_ckmlph-kst001 + e_ckmlph-kst002 +
                                e_ckmlph-kst004 + e_ckmlph-kst006 +
                                e_ckmlph-kst008 + e_ckmlph-kst010 +
                                e_ckmlph-kst012 + e_ckmlph-kst014 +
                                e_ckmlph-kst016 + e_ckmlph-kst018 +
                                e_ckmlph-kst020 + e_ckmlph-kst022 +
                                e_ckmlph-kst024 + e_ckmlph-kst026 +
                                e_ckmlph-kst028 + e_ckmlph-kst030 +
                                e_ckmlph-kst032 + e_ckmlph-kst034 +
                                e_ckmlph-kst036 + e_ckmlph-kst038 ) +
                               ( e_saida-vlu_std * e_ckmlke-losgr ) )
                             / e_ckmlke-losgr.

          e_saida-tma_real = e_saida-tma_real + (
                            ( e_ckmlph-kst001 ) * l_prop ).

          e_saida-sal_real   = e_saida-sal_real  + ckmlkeph-kst002.
          e_saida-enc_real   = e_saida-enc_real  + ckmlkeph-kst004.
          e_saida-cont_real  = e_saida-cont_real + ckmlkeph-kst006.
          e_saida-cons_real  = e_saida-cons_real + ckmlkeph-kst008.
          e_saida-comb_real  = e_saida-comb_real + ckmlkeph-kst010.
          e_saida-manu_real  = e_saida-manu_real + ckmlkeph-kst012.
          e_saida-mset_real  = e_saida-mset_real + ckmlkeph-kst014.
          e_saida-serv_real  = e_saida-serv_real + ckmlkeph-kst016.
          e_saida-alug_real  = e_saida-alug_real + ckmlkeph-kst018.
          e_saida-leas_real  = e_saida-leas_real + ckmlkeph-kst020.
          e_saida-segu_real  = e_saida-segu_real + ckmlkeph-kst022.
          e_saida-srv_real   = e_saida-srv_real  + ckmlkeph-kst024.
          e_saida-supo_real  = e_saida-supo_real + ckmlkeph-kst026.
          e_saida-comu_real  = e_saida-comu_real + ckmlkeph-kst028.
          e_saida-viag_real  = e_saida-viag_real + ckmlkeph-kst030.
          e_saida-impo_real  = e_saida-impo_real + ckmlkeph-kst032.
          e_saida-doac_real  = e_saida-doac_real + ckmlkeph-kst034.
          e_saida-desp_real  = e_saida-desp_real + ckmlkeph-kst036.
          e_saida-depr_real  = e_saida-depr_real + ckmlkeph-kst038.
          e_saida-waers      = e_ckmlph-waers.
        WHEN OTHERS.
      ENDCASE.

      CLEAR: e_ckmlph.
    ENDLOOP.

    MODIFY t_saida FROM e_saida INDEX l_tabix.

    CLEAR: e_saida, l_prop.
  ENDLOOP.

  CLEAR e_ckmlke.

ENDFORM.                    " F_PROCESSA_ATIVIDADE
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       Módulo da tela 9001
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.

*** Atribui status e título da tela
  SET PF-STATUS 'STATUS9000'.
  SET TITLEBAR  'TITLE9000' WITH sy-uname.

* Carrega Controles do Relatório
  PERFORM f_carrega_controles_tree.

* Registra eventos
  PERFORM f_registra_eventos.

  CHECK NOT v_alv_tree IS INITIAL.

* Envia dados para o frontend.
  CALL METHOD v_alv_tree->frontend_update.

ENDMODULE.                 " STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_CONTROLES_TREE
*&---------------------------------------------------------------------*
*       Carrega Controles do Relatório
*----------------------------------------------------------------------*
FORM f_carrega_controles_tree.

  DATA: l_tree_container_name(30) TYPE c,
        l_custom_container        TYPE REF TO cl_gui_custom_container.

  CHECK v_alv_tree IS INITIAL.

*** Cria container para alv-tree
  l_tree_container_name = 'CONTAINER'.
  CREATE OBJECT l_custom_container
    EXPORTING
      container_name              = l_tree_container_name
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc NE 0.
    MESSAGE a000(tree_control_msg).
  ENDIF.

*** Cria controle do tree
  CREATE OBJECT v_alv_tree
    EXPORTING
      parent                      = l_custom_container
      node_selection_mode         = cl_gui_column_tree=>node_sel_mode_multiple
      item_selection              = 'X'
      no_html_header              = ''
      no_toolbar                  = ''
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.

  IF sy-subrc <> 0.
    MESSAGE a000(tree_control_msg).
  ENDIF.

*** Carrega dados do Relatório ALV Tree
  PERFORM f_carrega_alv.

*** Cria Hierarquia do ALV
  PERFORM f_cria_hierarquia.

* Optimiza colunas do relatório
  CALL METHOD v_alv_tree->column_optimize
    EXCEPTIONS
      start_column_not_found = 1
      end_column_not_found   = 2
      OTHERS                 = 3.
  IF sy-subrc NE 0.
*   Nothing to do
  ENDIF.

ENDFORM.                    " F_CARREGA_CONTROLES_TREE
*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_ALV
*&---------------------------------------------------------------------*
*       Carrega dados do Relatório ALV Tree
*----------------------------------------------------------------------*
FORM f_carrega_alv.

  DATA: lt_list_commentary  TYPE slis_t_listheader,
        l_line              TYPE slis_listheader,
        l_txt_dd(60)        TYPE c,
        el_hierarchy_header TYPE treev_hhdr,
        l_toolbar           TYPE REF TO cl_gui_toolbar,
        lt_buttons          TYPE ttb_button,
        l_buttons           TYPE LINE OF ttb_button.

  CHECK NOT v_alv_tree IS INITIAL.

* Linha de Cabeçalho: TYPE H
  CLEAR l_line.
  l_line-typ  = 'H'.
  l_line-key  = ' '.
  l_line-info = 'Grupo André Maggi'(008).
  APPEND l_line TO lt_list_commentary.

* Linha de Ação: Exercício/Período
  CLEAR: l_line.
  l_line-typ  = 'A'.
  l_line-key  = ' '.
  WRITE p_period TO l_line-info.
  CONCATENATE l_line-info p_gjahr INTO l_line-info SEPARATED BY '/'.
  CONCATENATE TEXT-006 l_line-info INTO l_line-info SEPARATED BY ' '.
  APPEND l_line TO lt_list_commentary.

* Linha de Ação: Data / Moeda
  CLEAR: l_line.
  l_line-typ  = 'A'.
  l_line-key  = ' '.
  WRITE sy-datum TO l_line-info USING EDIT MASK '__/__/____'.
  CONCATENATE 'Data:'(007) l_line-info INTO l_line-info
                                     SEPARATED BY space.
  PERFORM f_texto_dominio USING 'CURTP' p_curtp CHANGING l_txt_dd.
  CONCATENATE l_line-info '-' l_txt_dd INTO l_line-info
                               SEPARATED BY space.
  APPEND l_line TO lt_list_commentary.

* Dimensões do cabeçalho
  el_hierarchy_header-heading   = ''.
  el_hierarchy_header-width     = 40.
  el_hierarchy_header-width_pix = ''.

  CALL METHOD v_alv_tree->set_table_for_first_display
    EXPORTING
      is_hierarchy_header = el_hierarchy_header
      it_list_commentary  = lt_list_commentary
      is_variant          = e_variant
      i_save              = 'A'
      i_background_id     = 'BANDAG_BACK'
      i_logo              = 'BANDAG_LOGO'
    CHANGING
      it_fieldcatalog     = t_fdcat
      it_outtab           = t_saida2.

*** Obtém objeto TOOLBAR do Tree
  CALL METHOD v_alv_tree->get_toolbar_object
    IMPORTING
      er_toolbar = l_toolbar.
  lt_buttons = l_toolbar->m_table_button.

*** Elimina botões existentes
  CALL METHOD l_toolbar->delete_all_buttons
    EXCEPTIONS
      cntl_error = 1
      OTHERS     = 2.

  IF sy-subrc EQ 0.
*.. Adiciona botão "Expandir tudo"
    CALL METHOD l_toolbar->add_button
      EXPORTING
        fcode            = '&EXPAND_ALL'
        icon             = '@68@'
        butn_type        = 0
        text             = ''
        quickinfo        = 'Expandir tudo'(011)
      EXCEPTIONS
        cntl_error       = 1
        cntb_btype_error = 2
        cntb_error_fcode = 3
        OTHERS           = 4.

*.. Trata botão "Expandir"
    READ TABLE lt_buttons INTO l_buttons WITH KEY
             function = v_alv_tree->mc_fc_expand.
    IF sy-subrc EQ 0.
      CALL METHOD l_toolbar->add_button
        EXPORTING
          fcode            = c_expa
          icon             = '@3S@'
          butn_type        = l_buttons-butn_type
          text             = l_buttons-text
          quickinfo        = 'Expandir'(012)
        EXCEPTIONS
          cntl_error       = 1
          cntb_btype_error = 2
          cntb_error_fcode = 3
          OTHERS           = 4.
      IF sy-subrc EQ 0.
        DELETE lt_buttons WHERE function EQ v_alv_tree->mc_fc_expand.
      ENDIF.
    ENDIF.

*.. Trata botão "Comprimir"
    READ TABLE lt_buttons INTO l_buttons WITH KEY
               function = v_alv_tree->mc_fc_collapse.
    IF sy-subrc EQ 0.
      CALL METHOD l_toolbar->add_button
        EXPORTING
          fcode            = c_coll
          icon             = '@3T@'
          butn_type        = l_buttons-butn_type
          text             = l_buttons-text
          quickinfo        = 'Comprimir'(013)
        EXCEPTIONS
          cntl_error       = 1
          cntb_btype_error = 2
          cntb_error_fcode = 3
          OTHERS           = 4.
      IF sy-subrc EQ 0.
        DELETE lt_buttons WHERE function EQ v_alv_tree->mc_fc_collapse.
      ENDIF.
    ENDIF.

*.. Adiciona botão "Comprimir tudo"
    CALL METHOD l_toolbar->add_button
      EXPORTING
        fcode            = '&COLLAPSE_ALL'
        icon             = '@69@'
        butn_type        = 0
        text             = ''
        quickinfo        = 'Comprimir tudo'(014)
      EXCEPTIONS
        cntl_error       = 1
        cntb_btype_error = 2
        cntb_error_fcode = 3
        OTHERS           = 4.

*.. Elimina função de Somar
    DELETE lt_buttons WHERE function EQ v_alv_tree->mc_fc_calculate.

*.. Atualiza restante dos botões
    CALL METHOD l_toolbar->add_button_group
      EXPORTING
        data_table       = lt_buttons
      EXCEPTIONS
        dp_error         = 2
        cntb_error_fcode = 2
        OTHERS           = 3.

*.. Adiciona separador
    CALL METHOD l_toolbar->add_button
      EXPORTING
        fcode            = space
        icon             = space
        butn_type        = 3
        text             = ''
        quickinfo        = space
      EXCEPTIONS
        cntl_error       = 1
        cntb_btype_error = 2
        cntb_error_fcode = 3
        OTHERS           = 4.

*.. Adiciona botão "Informações"
    CALL METHOD l_toolbar->add_button
      EXPORTING
        fcode            = c_info
        icon             = '@0S@'
        butn_type        = 0
        text             = ''
        quickinfo        = 'Legenda'(015)
      EXCEPTIONS
        cntl_error       = 1
        cntb_btype_error = 2
        cntb_error_fcode = 3
        OTHERS           = 4.
  ENDIF.

ENDFORM.                    " F_CARREGA_ALV
*&---------------------------------------------------------------------*
*&      Form  F_REGISTRA_EVENTOS
*&---------------------------------------------------------------------*
*       Registra eventos
*----------------------------------------------------------------------*
FORM f_registra_eventos.

  DATA: lt_events TYPE cntl_simple_events,
        l_event   TYPE cntl_simple_event.

  CHECK v_class IS INITIAL.

  CALL METHOD v_alv_tree->get_registered_events
    IMPORTING
      events = lt_events.

  l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
  APPEND l_event TO lt_events.

  l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  APPEND l_event TO lt_events.

  CALL METHOD v_alv_tree->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.
*   Nothing to do
  ENDIF.

  CREATE OBJECT v_class.
  SET HANDLER v_class->handle_node_double_click FOR v_alv_tree.
  SET HANDLER v_class->handle_item_double_click FOR v_alv_tree.
  SET HANDLER v_class->handle_user_command      FOR v_alv_tree.

ENDFORM.                    " F_REGISTRA_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CRIA_HIERARQUIA
*&---------------------------------------------------------------------*
*       Cria Hierarquia do ALV
*----------------------------------------------------------------------*
FORM f_cria_hierarquia.

  DATA: l_node_text       TYPE lvc_value,
        l_node_key_campo1 TYPE lvc_nkey,
        l_node_key_campo2 TYPE lvc_nkey,
        l_node_key_campo3 TYPE lvc_nkey,
        l_node_key_campo4 TYPE lvc_nkey,
        l_node_key_campo5 TYPE lvc_nkey,
        l_node_key_versao TYPE lvc_nkey,
        l_layout          TYPE lvc_s_layn,
        l_saida           TYPE ty_zlayout,
        l_nivel           TYPE i,
        l_tabix           TYPE i,
        lt_saida          TYPE STANDARD TABLE OF ty_zlayout,
        lt_nodes          TYPE STANDARD TABLE OF ty_nodes.

  lt_saida[] = t_saida[].
  SORT lt_saida BY kalnr_t tipo.

*** Processa dados de saída
  LOOP AT t_saida INTO e_saida.

    l_tabix = sy-tabix.

*.. Centro de Trabalho
    IF l_saida-arbpl NE e_saida-arbpl.
      CLEAR: e_table, l_node_text.
      CLEAR: l_saida.
      IF NOT e_saida-arbpl IS INITIAL.
        CONCATENATE e_saida-arbpl e_saida-ktext INTO l_node_text
                    SEPARATED BY ' - '.
      ENDIF.
      l_layout-exp_image = l_layout-n_image = '@ND@'.

      PERFORM f_add_node USING l_node_text
                               e_table
                               space
                               l_layout 1
                     CHANGING  l_node_key_campo1.
    ENDIF.

*.. Grupo de Mercadorias
    IF l_saida-arbpl NE e_saida-arbpl OR
       l_saida-matkl NE e_saida-matkl.
      CLEAR: e_table, l_node_text.
      CLEAR: l_saida-matnr_t, l_saida-mtart_r, l_saida-matnr_r.
      CONCATENATE e_saida-matkl e_saida-wgbez INTO l_node_text
                  SEPARATED BY ' - '.
      l_layout-exp_image = l_layout-n_image = '@A9@'.

      PERFORM f_add_node USING l_node_text
                               e_table
                               l_node_key_campo1
                               l_layout 2
                     CHANGING  l_node_key_campo2.
    ENDIF.

*.. Material - Topo
    IF l_saida-arbpl   NE e_saida-arbpl OR
       l_saida-matkl   NE e_saida-matkl OR
       l_saida-matnr_t NE e_saida-matnr_t.
      CLEAR e_table.
      CLEAR: l_saida-mtart_r, l_saida-matnr_r.
*.... Inclui dados do material pai
      IF e_saida-tipo = 'P'.

        e_saida-maktx = e_saida-maktx_t.
        e_saida-bwkey = e_saida-bwkey_t.
        e_saida-kalnr = e_saida-kalnr_t.
        MODIFY t_saida INDEX l_tabix FROM e_saida
                       TRANSPORTING maktx bwkey kalnr.

*       Limpa dados do filho
        e_table = e_saida.
        CLEAR: e_table-bwkey_r, e_table-kalnr_r.

*...... Material Pai, sem filho
        READ TABLE lt_saida TRANSPORTING NO FIELDS WITH KEY
                                kalnr_t = e_saida-kalnr_t
                                tipo    = 'F' BINARY SEARCH.
        IF sy-subrc EQ 0.
          l_nivel = 3.
        ELSE.
          l_nivel = 0.
        ENDIF.
      ELSE.
        l_nivel = 3.
      ENDIF.

      WRITE e_saida-matnr_t TO l_node_text USING EDIT MASK '==ALPHA'.
      l_layout-exp_image = l_layout-n_image = '@AT@'.

      PERFORM f_add_node USING l_node_text
                               e_table
                               l_node_key_campo2
                               l_layout l_nivel
                     CHANGING  l_node_key_campo3.
    ENDIF.

*   Tipo de material
    IF e_saida-tipo      EQ 'F' AND e_saida-matnr_r  GT space AND
       ( l_saida-arbpl   NE e_saida-arbpl   OR
         l_saida-matkl   NE e_saida-matkl   OR
         l_saida-matnr_t NE e_saida-matnr_t OR
         l_saida-mtart_r NE e_saida-mtart_r ).
      CLEAR: e_table, l_node_text.
      CLEAR: l_saida-matnr_r, l_saida-verid_nd.
      CONCATENATE e_saida-mtart_r e_saida-mtbez_r INTO l_node_text
                  SEPARATED BY ' - '.
      l_layout-exp_image = l_layout-n_image = '@HS@'.

      PERFORM f_add_node USING l_node_text
                               e_table
                               l_node_key_campo3
                               l_layout 4
                     CHANGING  l_node_key_campo4.
    ENDIF.

    IF e_saida-tipo NE 'F' AND e_saida-tipo NE 'C'.
      l_saida = e_saida.
      CONTINUE.
    ENDIF.

*.. Quebra por Versão
    IF p_ver EQ 'X'.
      IF l_saida-arbpl    NE e_saida-arbpl    OR
         l_saida-matkl    NE e_saida-matkl    OR
         l_saida-matnr_t  NE e_saida-matnr_t  OR
         l_saida-verid_nd NE e_saida-verid_nd.
        CLEAR: e_table, l_node_text.
        CONCATENATE 'Vs.prod:' e_saida-verid_nd
               INTO l_node_text SEPARATED BY space.
        l_layout-exp_image = l_layout-n_image = '@OO@'.

        PERFORM f_add_node USING l_node_text
                                 e_table
                                 l_node_key_campo4
                                 l_layout 5
                       CHANGING  l_node_key_versao.
      ENDIF.
    ELSE.
      l_node_key_versao = l_node_key_campo4.
    ENDIF.

    IF e_saida-otyp_inmat EQ 'LA'.
      e_saida-maktx =  e_saida-ltext_r.
      CLEAR e_saida-bwkey_r.
      l_node_key_versao = l_node_key_campo3.
      l_layout-exp_image = l_layout-n_image = '@BL@'.
      WRITE e_saida-kostl_r TO l_node_text USING EDIT MASK '==ALPHA'.
    ELSE.
      e_saida-maktx =  e_saida-maktx_r.
      l_layout-exp_image = l_layout-n_image = '@A6@'.
      WRITE e_saida-matnr_r TO l_node_text USING EDIT MASK '==ALPHA'.
    ENDIF.

    e_saida-bwkey = e_saida-bwkey_r.
    e_saida-kalnr = e_saida-kalnr_r.
    MODIFY t_saida INDEX l_tabix FROM e_saida
                   TRANSPORTING maktx bwkey kalnr.

*   Limpa dados do pai
    e_table = e_saida.
    CLEAR: e_table-matnr_t, e_table-bwkey_t, e_table-kalnr_t.

    PERFORM f_add_node USING l_node_text
                             e_table
                             l_node_key_versao
                             l_layout 0
                   CHANGING  l_node_key_campo5.

    CALL METHOD v_alv_tree->expand_node
      EXPORTING
        i_node_key       = l_node_key_campo1
        i_expand_subtree = space.

    l_saida = e_saida.
    CLEAR: l_layout, l_nivel.
  ENDLOOP.

*** Obtém maior nível
  lt_nodes = t_nodes.
  SORT lt_nodes BY nivel DESCENDING.
  LOOP AT lt_nodes INTO e_nodes.
    v_nmax = e_nodes-nivel.
    CLEAR: e_nodes.
    EXIT.
  ENDLOOP.

  e_nivel-enivel = 2.
  e_nivel-cnivel = 1.

ENDFORM.                    " F_CRIA_HIERARQUIA
*&---------------------------------------------------------------------*
*&      Form  F_ADD_NODE
*&---------------------------------------------------------------------*
*       Adiciona Nó no Relatório
*----------------------------------------------------------------------*
FORM f_add_node USING VALUE(p_node_text) TYPE lvc_value
                      VALUE(p_table)     TYPE ty_zlayout
                      VALUE(p_node_key1) TYPE lvc_nkey
                      VALUE(p_layout)    TYPE lvc_s_layn
                      VALUE(p_nivel)     TYPE i
             CHANGING VALUE(p_node_key2) TYPE lvc_nkey.

  CALL METHOD v_alv_tree->add_node
    EXPORTING
      i_relat_node_key = p_node_key1
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = p_node_text
      is_outtab_line   = p_table
      is_node_layout   = p_layout
    IMPORTING
      e_new_node_key   = p_node_key2.

  IF p_nivel GT 0.
    e_nodes-nivel = p_nivel.
    e_nodes-nodek = p_node_key2.
    APPEND e_nodes TO t_nodes. CLEAR: e_nodes.
  ENDIF.

ENDFORM.                    " F_ADD_NODE
*&---------------------------------------------------------------------*
*&      Form  F_RESTRINGE_MBEW
*&---------------------------------------------------------------------*
*       Restringe dados de Avaliação do material
*----------------------------------------------------------------------*
FORM f_restringe_mbew CHANGING VALUE(pt_key) TYPE ty_ckmlhd_tab.

  DATA: l_tabix TYPE sy-tabix VALUE 1.

  SORT pt_key BY matnr bwkey bwtar.
  SORT t_mbew BY matnr bwkey bwtar. "---> S4 MIGRATION 08/07/2023 - MA
  DO.
    READ TABLE t_mbew INTO e_mbew INDEX l_tabix.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

*.. Tratamento de matérias S2 ou V2
    IF ( e_mbew-vprsv NE 'S' AND e_mbew-vprsv NE 'V' ) OR
       ( e_mbew-mlast NE '2' ).
      DELETE t_mbew INDEX l_tabix.
      DELETE pt_key WHERE matnr EQ e_mbew-matnr
                      AND bwkey EQ e_mbew-bwkey
                      AND bwtar EQ e_mbew-bwtar.
      CONTINUE.
    ENDIF.

    ADD 1 TO l_tabix.
  ENDDO.

  SORT pt_key BY kalnr.

  CLEAR: e_mbew.

ENDFORM.                    " F_RESTRINGE_MBEW
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_VARIANTE
*&---------------------------------------------------------------------*
*       Valida Variante Informada
*----------------------------------------------------------------------*
FORM f_valida_variante.

  CHECK p_varian NE e_variant-variant.

  IF p_varian IS INITIAL.
*.. Atribui dados do programa
    CLEAR e_variant.
    e_variant-report = sy-repid.
    e_variant-username = sy-uname.
    EXIT.
  ENDIF.

*** Valida Variante Informada
  CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
    EXPORTING
      i_save        = 'A'
    CHANGING
      cs_variant    = e_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F_VALIDA_VARIANTE
*&---------------------------------------------------------------------*
*&      Form  F_NAVEGA_CKM3
*&---------------------------------------------------------------------*
*       Navega material topo para análise de preço
*----------------------------------------------------------------------*
FORM f_navega_ckm3 USING VALUE(p_node_key) TYPE lvc_nkey.

  DATA: l_value TYPE lvc_value,                             "#EC NEEDED
        l_layi  TYPE lvc_t_layi,                            "#EC NEEDED
        l_layn  TYPE lvc_s_layn.                            "#EC NEEDED

  CLEAR: e_saida, e_ckmlhd.

*** Busca linha selecionada
  CALL METHOD v_alv_tree->get_outtab_line
    EXPORTING
      i_node_key     = p_node_key
    IMPORTING
      e_outtab_line  = e_saida
      e_node_text    = l_value
      et_item_layout = l_layi
      es_node_layout = l_layn
    EXCEPTIONS
      node_not_found = 1
      OTHERS         = 2.

  IF sy-subrc EQ 0 AND NOT e_saida-matnr_t IS INITIAL.
    READ TABLE t_ckmlhd INTO e_ckmlhd WITH KEY kalnr = e_saida-kalnr_t
                                                        BINARY SEARCH.
    IF sy-subrc EQ 0.
*.... Exibe análise de preço
      CALL FUNCTION 'CKM8N_ML_DATA_DISPLAY'
        EXPORTING
          i_matnr = e_saida-matnr_t
          i_bwkey = e_saida-bwkey_t
          i_bwtar = e_ckmlhd-bwtar
          i_bdatj = p_gjahr
          i_poper = p_period
          i_curtp = p_curtp.
    ENDIF.
  ENDIF.

  CLEAR: e_saida, e_ckmlhd.

ENDFORM.                    " F_NAVEGA_CKM3
*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       Módulo da tela 9002
*----------------------------------------------------------------------*
MODULE status_9002 OUTPUT.

  SUPPRESS DIALOG.

* Atribui status e título da tela
  SET PF-STATUS 'STATUS9002'.
  SET TITLEBAR 'TITLE9002'.

* Imprime legenda do ALV
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  PERFORM f_imprime_legenda.

ENDMODULE.                 " STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_LEGENDA
*&---------------------------------------------------------------------*
*       Imprime legenda do ALV
*----------------------------------------------------------------------*
FORM f_imprime_legenda.

  DATA: BEGIN OF l_saida,
          col01 TYPE char10,
          col02 TYPE char50,
        END OF l_saida.

  NEW-PAGE NO-TITLE LINE-SIZE 65.
  FORMAT RESET.

*** Linha de Produção
  WRITE: TEXT-016 TO l_saida-col02 LEFT-JUSTIFIED.
  WRITE: AT /4(7) '@ND@' COLOR OFF INTENSIFIED OFF,
           l_saida-col02 COLOR OFF INTENSIFIED OFF.
  CLEAR: l_saida.

*** Família do Material
  WRITE: TEXT-017 TO l_saida-col02 LEFT-JUSTIFIED.
  WRITE: AT /4(7) '@A9@' COLOR OFF INTENSIFIED OFF,
           l_saida-col02 COLOR OFF INTENSIFIED OFF.
  CLEAR: l_saida.

*** Material Topo
  WRITE: TEXT-018 TO l_saida-col02 LEFT-JUSTIFIED.
  WRITE: AT /4(7) '@AT@' COLOR OFF INTENSIFIED OFF,
           l_saida-col02 COLOR OFF INTENSIFIED OFF.
  CLEAR: l_saida.

*** Tipo de Material
  WRITE: TEXT-022 TO l_saida-col02 LEFT-JUSTIFIED.
  WRITE: AT /4(7) '@HS@' COLOR OFF INTENSIFIED OFF,
           l_saida-col02 COLOR OFF INTENSIFIED OFF.
  CLEAR: l_saida.

*** Versão de Produção
  WRITE: TEXT-019 TO l_saida-col02 LEFT-JUSTIFIED.
  WRITE: AT /4(7) '@OO@' COLOR OFF INTENSIFIED OFF,
           l_saida-col02 COLOR OFF INTENSIFIED OFF.
  CLEAR: l_saida.

*** Material (Req.)
  WRITE: TEXT-020 TO l_saida-col02 LEFT-JUSTIFIED.
  WRITE: AT /4(7) '@A6@' COLOR OFF INTENSIFIED OFF,
           l_saida-col02 COLOR OFF INTENSIFIED OFF.
  CLEAR: l_saida.

*** Centro de Custo
  WRITE: TEXT-021 TO l_saida-col02 LEFT-JUSTIFIED.
  WRITE: AT /4(7) '@BL@' COLOR OFF INTENSIFIED OFF,
           l_saida-col02 COLOR OFF INTENSIFIED OFF.
  CLEAR: l_saida.

ENDFORM.                    " F_IMPRIME_LEGENDA

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9002 INPUT.

  sy-ucomm = v_okcode.
  CLEAR v_okcode.

  SET SCREEN 9001. LEAVE SCREEN.

ENDMODULE.                 " USER_COMMAND_9002  INPUT

**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*'

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Paulo Quevedo ( paulo.quevedo@amaggi.com.br )                        |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Cadastro de Contratos - Algodão                                           |*
**/===========================================================================\*

REPORT  zsdr0070.

*&--------------------------------------------------------------------&*
*& TABLES
*&--------------------------------------------------------------------&*
TABLES: zsdt0143.

*&--------------------------------------------------------------------&*
*& INCLUDE
*&--------------------------------------------------------------------&*
INCLUDE <cl_alv_control>.

*&--------------------------------------------------------------------&*
*& TYPES
*&--------------------------------------------------------------------&*

TYPES:
  BEGIN OF ty_saida.
    INCLUDE STRUCTURE zsdt0143.
TYPES: edit(4)                TYPE c,
    excluir(4)             TYPE c,
    s_sol(4)               TYPE c,
    s_con(4)               TYPE c,
    desc_empresa(50)       TYPE c,
    desc_cliente(50)       TYPE c,
    desc_cliente_final(50) TYPE c,
    desc_vendedor(50)      TYPE c,
    desc_corretor(50)      TYPE c,
    desc_tipo_padrao(5)    TYPE c,
    desc_tp_venda(50)      TYPE c,
    nro_sol_ov(255)        TYPE c,
    pedido(255)            TYPE c, "US - 77507
    t_log(4)               TYPE c,
    visao_desc(15)         TYPE c,
  END OF ty_saida,

  ty_zsdt0143 TYPE TABLE OF zsdt0143,

*** US - 77507 - CBRAND
  BEGIN OF ty_pedido.
TYPES: contrato    TYPE bstkd,
       pedido(255) TYPE c,
       END OF ty_pedido,
*** US - 77507 - CBRAND

       BEGIN OF ty_relatorio.
TYPES: safra        TYPE name1,
       desc_cliente TYPE name1,
       contrato     TYPE bstkd,
       tipo         TYPE normt,
       preco        TYPE p DECIMALS 4,
       preco_lb     TYPE dmbtr,
       jan          TYPE int4,
       fev          TYPE int4,
       mar          TYPE int4,
       abr          TYPE int4,
       mai          TYPE int4,
       jun          TYPE int4,
       jul          TYPE int4,
       ago          TYPE int4,
       set          TYPE int4,
       out          TYPE int4,
       nov          TYPE int4,
       dev          TYPE int4,
       qtd_tons     TYPE int4,
       total        TYPE p DECIMALS 2,
       ordem        TYPE p DECIMALS 1,
       color(4)     TYPE c,
       END OF ty_relatorio,
       BEGIN OF ty_preco_n.
         INCLUDE TYPE zsds006.
TYPES: posnr        TYPE zsdt0059-posnr,
         cbot(6),
         invisible(1),
         waers        TYPE bsid-waers,
         preco        TYPE zsdt0059-preco,
         ocbot        TYPE zsdt0059-ocbot,
         posnr1       TYPE zsdt0059-posnr1,
         valdt_hedge  TYPE zsdt0059-valdt_hedge,
       END OF ty_preco_n,

       BEGIN OF ty_preco.
         INCLUDE TYPE zsds006.
*TYPES:  FORMULA2 TYPE ZSDS006-FORMULA,
TYPES:  "NIVEL2     TYPE ZSDS006-NIVEL,
  formula2(20),   "TYPE BSID-DMBTR,
         waers         TYPE bsid-waers,
         ocbot         TYPE zsdt0059-ocbot,
         preco         TYPE zsdt0059-preco,
*        C_DECIMAIS TYPE ZSDT0059-C_DECIMAIS,
         cbot(6),
         posnr         TYPE zsdt0059-posnr,
         zmeng         TYPE zsdt0059-zmeng,
         valdt         TYPE zsdt0059-valdt,
         safra         TYPE zsdt0059-safra,
         monat         TYPE zsdt0059-monat,
         item_key      TYPE lvc_nkey,
         style         TYPE lvc_t_styl,
         line_color(4) TYPE c,     "Used to store row color attributes
       END OF ty_preco,

       BEGIN OF ty_mes.
TYPES: jan TYPE dzmeng,
       fev TYPE dzmeng,
       mar TYPE dzmeng,
       abr TYPE dzmeng,
       mai TYPE dzmeng,
       jun TYPE dzmeng,
       jul TYPE dzmeng,
       ago TYPE dzmeng,
       set TYPE dzmeng,
       out TYPE dzmeng,
       nov TYPE dzmeng,
       dev TYPE dzmeng,
       END OF ty_mes,

       BEGIN OF ty_projecao.
TYPES: qtd           TYPE zsdt0171-quantidade,
       qtd_c         TYPE char50,
       preco         TYPE dmbtr,
       preco_c       TYPE char50,
       porcentagem   TYPE numc3,
       porcentagem_c TYPE char50,
       END OF ty_projecao.


TYPES: BEGIN OF tl_ucomm,
         ucomm TYPE  sy-ucomm,
       END OF tl_ucomm.


TYPES: BEGIN OF ty_token,
         token_type     TYPE string,
         expires_in     TYPE i,
         ext_expires_in TYPE i,
         expires_on     TYPE i,
         not_before     TYPE i,
         resource       TYPE string,
         access_token   TYPE string,
       END OF ty_token.


TYPES: BEGIN OF ty_contratos,
         referenciaintegracaocontrato TYPE string,
         referenciaintegracaoempresa  TYPE string,
         referenciaintegracaocliente  TYPE string,
         nomecliente                  TYPE string,
         cnpj                         TYPE string,
         classificacao                TYPE string,
         safra                        TYPE string,
         contrato                     TYPE string,
         datainicial                  TYPE string,
         datafinal                    TYPE string,
         quantidade                   TYPE i,
         codigoacts                   TYPE boole_d, "*-CS2023000189-04.04.2023-#108693-JT
       END OF ty_contratos.

TYPES: BEGIN OF ty_del_contratos,
         referenciaintegracao TYPE string,
         safra                TYPE string,
         contrato             TYPE string,
       END OF ty_del_contratos.

TYPES: BEGIN OF ty_return_api,
         "   errors
         title  TYPE string,
         status TYPE i,
         detail TYPE string,
         "  instance
       END OF ty_return_api.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura,

BEGIN OF ty_ucomm_solic,
  ucomm    TYPE sy-ucomm,
  nr_solic TYPE zsded013,
END OF ty_ucomm_solic,

BEGIN OF ty_qtd_embarcada.
TYPES: contrato      TYPE zsdt0143-contrato,
       empresa       TYPE zsdt0143-empresa,
       qtd_embarcada TYPE zsdt0143-qdt_embarcada,
       END OF ty_qtd_embarcada.

DATA: fcode TYPE TABLE OF tl_ucomm.

CONSTANTS vlr TYPE p DECIMALS 5 VALUE '2.2046'.

*&--------------------------------------------------------------------&*
*& Internal Table
*&--------------------------------------------------------------------&*
DATA: it_saida            TYPE TABLE OF ty_saida,
      it_pedido           TYPE TABLE OF ty_pedido,
      it_relatorio        TYPE TABLE OF ty_relatorio,
      it_cliente          TYPE TABLE OF ty_relatorio,
      it_safra            TYPE TABLE OF ty_relatorio,
      it_geral            TYPE TABLE OF ty_relatorio,
      it_projecao         TYPE TABLE OF zsdt0171,
      it_zauth_webservice TYPE TABLE OF zauth_webservice,
      it_contratos        TYPE TABLE OF ty_contratos,
      t_preco             TYPE TABLE OF ty_preco_n WITH HEADER LINE,
      t_preco_f           TYPE TABLE OF ty_preco WITH HEADER LINE,
      t_0227              TYPE TABLE OF zsdt0227,
      t_fields            TYPE TABLE OF zsdt0072  WITH HEADER LINE,
      tg_0059             TYPE TABLE OF zsdt0059   WITH HEADER LINE,
      tg_0059_2           TYPE TABLE OF zsdt0059_2 WITH HEADER LINE,
      tg_0059_old         TYPE TABLE OF zsdt0059   WITH HEADER LINE,
      t_new_table         TYPE REF TO data,
      t_new_line          TYPE REF TO data,
      t_empre             TYPE TABLE OF rgsb4,
      w_empre             TYPE rgsb4,
      l_envia_trace       TYPE c,
      l_erro              TYPE c,
      l_copia             TYPE c,
      lv_acao             TYPE c,
      t_ucomm_solic       TYPE TABLE OF ty_ucomm_solic,
      t_solic             TYPE TABLE OF zsded013,
      tg_qtdembarcada     TYPE TABLE OF ty_qtd_embarcada WITH HEADER LINE.

DATA: vmatnr18            TYPE matnr18.

RANGES: r_empre            FOR t001-bukrs.

*&--------------------------------------------------------------------&*
*& Work Area
*&--------------------------------------------------------------------&*
DATA: wa_saida            TYPE ty_saida,
      wa_pedido           TYPE ty_pedido,
      w_contrato          TYPE ty_saida,
      wa_desc             TYPE ty_saida,
      wa_novo             TYPE zsdt0143,
      wa_relatorio        TYPE ty_relatorio,
      wa_mes              TYPE ty_mes,
      wa_projecao         TYPE zsdt0171,
      _projecao           TYPE ty_projecao,
      wa_geral            TYPE ty_relatorio,
      w_preco             TYPE ty_preco_n,
      w_0227              TYPE zsdt0227,
      wa_token            TYPE ty_token,
      wa_zauth_webservice TYPE zauth_webservice,
      wa_zauth_ws_0001    TYPE zauth_ws_0001,
      wa_contratos        TYPE ty_contratos,
      wa_del_contratos    TYPE ty_del_contratos,
      wa_return_api       TYPE ty_return_api,
      wa_qtdembarcada     TYPE ty_qtd_embarcada.


DATA w_valor(30).
DATA gv_ultimo_nvl TYPE c LENGTH 4.
DATA w_valor_aux(30).
DATA w_tabix TYPE sy-tabix.

DATA: p_2(13)     TYPE p DECIMALS 2,
      p_5(13)     TYPE p DECIMALS 5,
      p_4(13)     TYPE p DECIMALS 4,
      btn_enc(30).
DATA state(3).
DATA acao(3).

DATA: ti_bdcdata       TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      wa_bdcdata       LIKE LINE OF ti_bdcdata,
      t_messtab        TYPE TABLE OF bdcmsgcoll,
      wl_shdbnr        TYPE zshdbt0001-shdbnr,
      "vobj_key         TYPE zib_contabil_err-obj_key,
      "vst_lc_moeda     TYPE zglt035-st_lc_moeda,
      "account_name(20) TYPE c,
      wg_documento(10).


FIELD-SYMBOLS: <fs_table>       TYPE STANDARD TABLE,
               <fs_table_frame> TYPE STANDARD TABLE,
               <fs_line>        TYPE any,
               <fs_line_aux>    TYPE any,
               <fs_campo>       TYPE any.


DATA: at_soma_qtd  TYPE dzmeng,
      desc_empresa TYPE butxt,
      desc_centro  TYPE name1.

DATA: it_rsparams TYPE TABLE OF rsparams,
      wa_rsparams TYPE  rsparams.


DATA lv_redist TYPE c VALUE space.
DATA: wa_dfies TYPE dfies.
DATA: it_field_tab TYPE STANDARD TABLE OF dfies.
DATA: gva_id_contrato_referencia TYPE zsdt0143-id_contrato_referencia.


*&--------------------------------------------------------------------&*
*& ALV
*&--------------------------------------------------------------------&*
DATA: cl_container     TYPE REF TO cl_gui_custom_container,
      cc_2             TYPE REF TO cl_gui_custom_container,
      _cont            TYPE REF TO cl_gui_custom_container,
      p_cont           TYPE REF TO cl_gui_custom_container,
      cl_grid          TYPE REF TO cl_gui_alv_grid,
      _grid            TYPE REF TO cl_gui_alv_grid,
      p_grid           TYPE REF TO cl_gui_alv_grid,
      it_sort          TYPE lvc_t_sort,
      it_fcat          TYPE lvc_t_fcat,
      wa_fcat          TYPE lvc_s_fcat,
      wa_layout        TYPE lvc_s_layo,
      wa_variant       TYPE disvariant,
      wa_stable        TYPE lvc_s_stbl VALUE 'XX',
      wa_estrutura     TYPE ty_estrutura,
      it_fieldcat_comp TYPE TABLE OF ty_estrutura,
      wa_fieldcat_comp TYPE ty_estrutura.

DATA: it_sel_rows	TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA t_fieldcatalog TYPE lvc_t_fcat.
DATA w_fieldcatalog LIKE LINE OF t_fieldcatalog.
DATA style          TYPE lvc_t_styl.
DATA w_style        TYPE lvc_s_styl.

DATA at_id TYPE int4.

*&--------------------------------------------------------------------&*
*& Constants
*&--------------------------------------------------------------------&*
CONSTANTS: tela_01(4) TYPE c VALUE '0100',
           tela_02(4) TYPE c VALUE '0200',
           tela_03(4) TYPE c VALUE '0300',
           tela_04(4) TYPE c VALUE '0400',
           tela_05(4) TYPE c VALUE '0500',
           post       TYPE string VALUE 'POST',
           put        TYPE string VALUE 'PUT',
           delete     TYPE string VALUE 'DELETE',
           c_x        TYPE c      VALUE 'X',
           c_c        TYPE c      VALUE 'C',
           c_r        TYPE c      VALUE 'R',
           c_f        TYPE c      VALUE 'F'.



*    "//Objects
DATA docking             TYPE REF TO cl_gui_docking_container.
DATA splitter            TYPE REF TO cl_gui_splitter_container.
DATA splitter_f          TYPE REF TO cl_gui_splitter_container.
DATA splitter1           TYPE REF TO cl_gui_splitter_container.
DATA custom_header       TYPE REF TO cl_gui_container.
DATA custom_grid         TYPE REF TO cl_gui_container.
DATA custom_grid1        TYPE REF TO cl_gui_container.
DATA tree1               TYPE REF TO cl_gui_alv_tree.
DATA container_s         TYPE REF TO cl_gui_custom_container.
DATA container_f         TYPE REF TO cl_gui_custom_container.
DATA container_spliter1  TYPE REF TO cl_gui_container.
DATA container_spliter2  TYPE REF TO cl_gui_container.
DATA grid_s              TYPE REF TO cl_gui_alv_grid.
DATA grid_f              TYPE REF TO cl_gui_alv_grid.
DATA grid                TYPE REF TO cl_gui_alv_grid.
DATA grid1               TYPE REF TO cl_gui_alv_grid.
DATA alv_tree            TYPE REF TO cl_gui_alv_tree.
DATA _layout             TYPE lvc_s_layo.
DATA _sort               TYPE lvc_t_sort.
DATA _extension          TYPE int4 VALUE 5000.

DATA it_exclud TYPE ui_functions.

DATA: tl_function TYPE ui_functions.
DATA: wl_repid    TYPE sy-repid,
      wl_function LIKE tl_function WITH HEADER LINE,
      lt_f4       TYPE lvc_t_f4 WITH HEADER LINE.
DATA: l_hierarchy_header TYPE treev_hhdr,
      ls_variant         TYPE disvariant.

DATA: lt_events TYPE cntl_simple_events,
      l_event   TYPE cntl_simple_event.

DATA: t_msg     TYPE TABLE OF zfiwrs0002,
      w_msg(30),
      i_show(1).


DATA: v_url      TYPE string,
      v_username TYPE string,
      v_password TYPE string,
      v_resorce  TYPE string,
      v_token    TYPE string.


DATA: ob_web_service TYPE REF TO zcl_webservice_trace,
      ob_web_token   TYPE REF TO zcl_webservice.
DATA: e_reason       TYPE string,
      json_retorno   TYPE string,
      e_http         TYPE REF TO  if_http_client,
      i_url          TYPE string,
      e_xml          TYPE string,
      e_json         TYPE string,
      r_json         TYPE string,
      e_return_code  TYPE i,
      v_metodo       TYPE string,
      v_return       TYPE string,
      v_id_contr_ant TYPE vbeln_va.

DATA: l_id_integracao  TYPE zintegracao-id_integracao,
      l_url            TYPE string,
      w_integracao     TYPE zintegracao,
      w_integracao_log TYPE zintegracao_log.


DATA: it_msg TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
      wa_msg TYPE bdcmsgcoll,

      it_dta TYPE STANDARD TABLE OF bdcdata,
      wa_dta LIKE LINE OF it_dta,

      BEGIN OF it_msgtext OCCURS 0,
        texto TYPE t100-text,
      END OF it_msgtext.

CONSTANTS: BEGIN OF c_tab_strip,
             tab1 LIKE sy-ucomm VALUE 'TAB_STRIP_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_STRIP_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAB_STRIP_FC3',
             tab4 LIKE sy-ucomm VALUE 'TAB_STRIP_FC4',
             tab5 LIKE sy-ucomm VALUE 'TAB_STRIP_FC5',
             tab6 LIKE sy-ucomm VALUE 'TAB_STRIP_FC6',
             tab7 LIKE sy-ucomm VALUE 'TAB_STRIP_FC7',
             tab8 LIKE sy-ucomm VALUE 'TAB_STRIP_FC8',
             tab9 LIKE sy-ucomm VALUE 'TAB_STRIP_FC9',
           END OF c_tab_strip.

DATA: BEGIN OF g_tab_strip,
        subscreen   LIKE sy-dynnr, "VALUE '0521',
        prog        LIKE sy-repid VALUE 'ZSDR0070',
        pressed_tab LIKE sy-ucomm VALUE c_tab_strip-tab1,
      END OF g_tab_strip,

      BEGIN OF tabs,
        tab_strip_tab1 TYPE char10,
        tab_strip_tab2 TYPE char10,
        tab_strip_tab3 TYPE char10,
        tab_strip_tab4 TYPE char10,
        tab_strip_tab5 TYPE char10,
        tab_strip_tab6 TYPE char10,
        tab_strip_tab7 TYPE char10,
        tab_strip_tab8 TYPE char10,
        tab_strip_tab9 TYPE char10,
      END OF   tabs.

CONTROLS:  tab_strip TYPE TABSTRIP.

CREATE OBJECT ob_web_token.
CREATE OBJECT ob_web_service.

*&--------------------------------------------------------------------&*
*& Variables
*&--------------------------------------------------------------------&*
DATA: edit          TYPE c,
      v_param_espec TYPE zsded028,
      v_tp_venda    TYPE zsded012.
DATA:      ok_code LIKE sy-ucomm.
*&--------------------------------------------------------------------&*
*& Parameters
*&--------------------------------------------------------------------&*
SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-002.
  PARAMETER: r_contr RADIOBUTTON GROUP b2 DEFAULT 'X' USER-COMMAND a,
             r_proje RADIOBUTTON GROUP b2 ,
             r_relat RADIOBUTTON GROUP b2 .
SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_empre FOR zsdt0143-empresa NO INTERVALS NO-EXTENSION MODIF ID id1 OBLIGATORY, "BUG - 78952 - CBRAND
                  p_safra FOR zsdt0143-safra NO INTERVALS NO-EXTENSION MODIF ID id1,
                  p_clien FOR zsdt0143-cliente NO INTERVALS NO-EXTENSION MODIF ID id1,
                  p_contr FOR zsdt0143-contrato NO INTERVALS NO-EXTENSION MODIF ID id2.
SELECTION-SCREEN: END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE abap_true.
      WHEN r_proje OR r_relat.
        IF screen-group1 EQ 'ID2'.
          screen-active = '0'.
          CLEAR: p_safra, p_clien, p_contr.
          MODIFY SCREEN.
          CONTINUE.
        ENDIF.
    ENDCASE.
  ENDLOOP.

START-OF-SELECTION.

*-BUG 69276 - JT - 23.11.2021 - Inicio
*-------------------------------
* Empresas que disparam trace cotton
*-------------------------------
  FREE: t_empre, r_empre.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'ZSDT0118_BUKRS'
    TABLES
      set_values    = t_empre
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  LOOP AT t_empre INTO w_empre.
    r_empre-sign   = 'I'.
    r_empre-option = 'EQ'.
    r_empre-low    = w_empre-from.
    APPEND r_empre.
  ENDLOOP.

  IF r_empre[] IS INITIAL.
    MESSAGE 'SET de dados ZSDT0118_BUKRS está vazio!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
*-BUG 69276 - JT - 23.11.2021 - fim
*-------------------------------

  IF p_empre IS INITIAL.
    MESSAGE 'O campo Empresa é Obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*-BUG 69276 - JT - 23.11.2021 - inicio
  IF p_empre-low IN r_empre[].
    l_envia_trace = abap_true.
  ELSE.
    l_envia_trace = abap_false.
  ENDIF.
*-BUG 69276 - JT - 23.11.2021 - fim

  AUTHORITY-CHECK OBJECT 'M_MATE_BUK'
  ID 'ACTVT' DUMMY
  ID 'BUKRS' FIELD p_empre-low.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE |Sem autorização para a Empresa { p_empre-low }!| TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM: seleciona_dados.

  CASE abap_true.
    WHEN r_relat.
      CALL SCREEN tela_03.
    WHEN r_proje.
      CALL SCREEN tela_04.
    WHEN OTHERS.
*-BUG 69276 - JT - 23.11.2021 - inicio
*     IF p_empre-low EQ '0001'.
      "CS2023000851 Algodão - PT1 - Atualização da transação ZSDT0118 - Formato já executado na Amaggi e mais detalhado --128361 - BG
      "IF l_envia_trace = abap_false.
      CALL SCREEN tela_05.
      "ELSE.
      " CALL SCREEN tela_01.
      "ENDIF.
*-BUG 69276 - JT - 23.11.2021 - fim
  ENDCASE.

CLASS consolidar DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_normt IMPORTING input         TYPE matnr
                RETURNING VALUE(return) TYPE normt,
      get_preco IMPORTING input         TYPE bstkd
                RETURNING VALUE(return) TYPE zsded053,
      set_qtd IMPORTING input         TYPE dzmeng
              RETURNING VALUE(return) TYPE dzmeng,
      add_projecao IMPORTING input TYPE zsdt0171,
      del_projecao,
      get_porto,
      selecao_projecao,
      get_empresa IMPORTING input TYPE bukrs RETURNING VALUE(return) TYPE butxt,
      get_centro IMPORTING input TYPE werks_d  RETURNING VALUE(return) TYPE name1,
      qtd_projecao RETURNING VALUE(return) TYPE zqtdea,
      gera_rel,
      consolida_dados,
      creat_doc,
      set_valores IMPORTING fieldname TYPE lvc_fname CHANGING vl_any TYPE data,
      get_valores IMPORTING fieldname TYPE lvc_fname CHANGING vl_any TYPE data,
      get_estrutura_preco,
      get_dados  IMPORTING p_tipo TYPE c OPTIONAL,
      convert_vl IMPORTING decimais TYPE zsded024 CHANGING vl TYPE char30,
      save_dados IMPORTING p_acao TYPE c OPTIONAL,
      encerrar,
      deletar,
      adm_botoes,
      verifica_erros,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no,
      dt_changed_f_preco FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,
      handle_hotspot_click2  FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.

CLASS consolidar IMPLEMENTATION.
  METHOD get_normt.
    SELECT SINGLE normt FROM mara INTO return WHERE matnr EQ input.
  ENDMETHOD.
  METHOD get_preco.
    SELECT SINGLE preco FROM zsdt0143 INTO return WHERE contrato EQ input.
  ENDMETHOD.
  METHOD set_qtd.
    ADD input TO at_soma_qtd.
    return = at_soma_qtd.
  ENDMETHOD.
  METHOD add_projecao.

    IF input-safra IS INITIAL OR
       input-empresa IS INITIAL OR
       input-fazenda IS INITIAL OR
       input-quantidade IS INITIAL.
      MESSAGE 'Todos os campos são Obrigatório' TYPE 'W'.
      EXIT.
    ENDIF.

    MODIFY zsdt0171 FROM input.

    consolidar=>selecao_projecao( ).

    CALL METHOD p_grid->refresh_table_display.

  ENDMETHOD.

  METHOD del_projecao.
    DATA: _rows      TYPE lvc_t_row.

    CALL METHOD p_grid->get_selected_rows
      IMPORTING
        et_index_rows = _rows.

    LOOP AT _rows INTO DATA(wa).
      DATA(wa_proj) = it_projecao[ wa-index ].

      DELETE FROM zsdt0171
      WHERE safra EQ wa_proj-safra
      AND empresa EQ wa_proj-empresa
      AND fazenda EQ wa_proj-fazenda.

    ENDLOOP.

    consolidar=>selecao_projecao( ).

    CALL METHOD p_grid->refresh_table_display.

  ENDMETHOD.

  METHOD get_porto.


    DATA t_return TYPE TABLE OF ddshretval.

    TYPES: BEGIN OF ty_porto,
             seq      TYPE  sy-tabix,
             ds_porto TYPE zds_porto,
           END OF ty_porto.

    DATA t_porto TYPE TABLE OF ty_porto.

    SELECT ds_porto
    FROM znom_transporte
    INTO CORRESPONDING FIELDS OF TABLE t_porto.

    DELETE t_porto WHERE ds_porto IS INITIAL.

    SORT t_porto BY ds_porto.
    DELETE ADJACENT DUPLICATES FROM t_porto COMPARING ds_porto.

    LOOP AT t_porto ASSIGNING FIELD-SYMBOL(<l_por>).
      <l_por>-seq = sy-tabix.
    ENDLOOP.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'DS_PORTO'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        value_org       = 'S'
      TABLES
        value_tab       = t_porto
        return_tab      = t_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc IS INITIAL.
      TRY .
          wa_novo-porto = t_return[ 1 ]-fieldval.
          w_contrato-porto = t_return[ 1 ]-fieldval.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD selecao_projecao.

    SELECT * FROM zsdt0171
    INTO TABLE it_projecao
    WHERE safra IN p_safra
      AND empresa IN p_empre.

  ENDMETHOD.

  METHOD get_empresa.
    SELECT SINGLE butxt FROM t001 INTO return WHERE bukrs EQ input.
  ENDMETHOD.

  METHOD get_centro.
    SELECT SINGLE name1 FROM t001w INTO return WHERE werks EQ input.
  ENDMETHOD.

  METHOD gera_rel.

    DATA: w_excel     TYPE ole2_object,
          w_workbooks TYPE ole2_object,
          w_workbook  TYPE ole2_object,
          h_int       TYPE ole2_object,
          h_f         TYPE ole2_object,
          h_rows      TYPE ole2_object,
          h_font      TYPE ole2_object,
          h_columns   TYPE ole2_object,
          h_entirecol TYPE ole2_object,
          w_cell      TYPE ole2_object,
          w_cells     TYPE ole2_object,
          w_cell1     TYPE ole2_object.

    DATA: w_line TYPE i.

    DATA(r_table) = it_fcat.
    DATA: lo_interior TYPE ole2_object.

    DELETE r_table WHERE no_out IS NOT INITIAL.

    CREATE OBJECT w_excel 'Excel.Application'.

    CALL METHOD OF w_excel 'Workbooks' = w_workbooks.
    CALL METHOD OF w_workbooks 'Add' = w_workbook.

    SET PROPERTY OF w_excel 'Visible' = 1.

    w_line = 1.
    DATA(qts_line) = lines( r_table ).

    CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = w_line #2 = w_line.
    CALL METHOD OF w_excel 'Cells' = w_cell1 EXPORTING #1 = w_line #2 = qts_line.
    CALL METHOD OF w_excel 'Range' = w_cells EXPORTING #1 = w_cell #2 = w_cell1.
    CALL METHOD OF w_cells 'Merge'.

    CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = 1 #2 = 1.
    SET PROPERTY OF w_cell 'Value' = 'Vendas Algodão para Exportação'.
    GET PROPERTY OF w_cell 'Interior'   = h_int.
    SET PROPERTY OF h_int  'Colorindex' = 15. "Cor Titulo Fundo
    SET PROPERTY OF w_cell 'HorizontalAlignment' = -4108 .

    CALL METHOD OF w_excel 'Rows' = h_rows EXPORTING #1 = '1:1'.
    GET PROPERTY OF h_rows 'Font' = h_font.
    SET PROPERTY OF h_font 'Bold' = 1.

    qts_line = lines( it_relatorio ).

    CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = 4 #2 = 1.
    CALL METHOD OF w_excel 'Cells' = w_cell1 EXPORTING #1 = qts_line #2 = 1.
    CALL METHOD OF w_excel 'Range' = w_cells EXPORTING #1 = w_cell #2 = w_cell1.

    CALL METHOD OF w_cells 'Rows' = h_rows.
    GET PROPERTY OF h_rows 'Font' = h_font.
    SET PROPERTY OF h_font 'Bold' = 1.


    ADD 2 TO w_line.
    LOOP AT r_table ASSIGNING FIELD-SYMBOL(<table>).
      DATA(tabix) = sy-tabix.
      CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = w_line #2 = tabix.
      SET PROPERTY OF w_cell 'Value' = <table>-scrtext_l.
      GET PROPERTY OF w_cell 'Interior'   = h_int.
      SET PROPERTY OF h_int  'Colorindex' = 50. "Cor Cabeçario Fundo
      GET PROPERTY OF w_cell 'Font'    = h_f.
      SET PROPERTY OF h_f    'Colorindex' = 2. "Cor Cabeçario Letra
      SET PROPERTY OF w_cell 'HorizontalAlignment' = -4108.
    ENDLOOP.

    CALL METHOD OF w_excel 'Rows' = h_rows EXPORTING #1 = '3:3'.
    GET PROPERTY OF h_rows 'Font' = h_font.
    SET PROPERTY OF h_font 'Bold' = 1.

    FIELD-SYMBOLS: <fs_campo> TYPE any.
    DATA: vl_string      TYPE char255,
          vl_convert     TYPE char255,
          vl_valor_0     TYPE int4,
          vl_valor_2(16) TYPE p DECIMALS 2,
          vl_valor_4(16) TYPE p DECIMALS 4.

    DATA: cor_total  TYPE c,
          cor_number TYPE int4.

    ADD 1 TO w_line.
    LOOP AT it_relatorio ASSIGNING FIELD-SYMBOL(<excel>).

      cor_total = abap_false.
      IF <excel>-desc_cliente(5) EQ 'Total'.
        cor_total = abap_true.
        cor_number = 15.

        DATA(line_range) = |{ w_line }:{ w_line }|.
        CALL METHOD OF w_excel 'Rows' = h_rows EXPORTING #1 = line_range.
        GET PROPERTY OF h_rows 'Font' = h_font.
        SET PROPERTY OF h_font 'Bold' = 1.

      ENDIF.

      IF <excel>-safra(5) EQ 'Total'.
        cor_total = abap_true.
        cor_number = 48.

        line_range = |{ w_line }:{ w_line }|.
        CALL METHOD OF w_excel 'Rows' = h_rows EXPORTING #1 = line_range.
        GET PROPERTY OF h_rows 'Font' = h_font.
        SET PROPERTY OF h_font 'Bold' = 1.
      ENDIF.

      IF <excel>-safra(7) EQ 'Total G'.

        cor_total = abap_true.
        cor_number = 48.

        line_range = |{ w_line }:{ w_line }|.
        CALL METHOD OF w_excel 'Rows' = h_rows EXPORTING #1 = line_range.
        GET PROPERTY OF h_rows 'Font' = h_font.
        SET PROPERTY OF h_font 'Bold' = 1.
      ENDIF.

      LOOP AT r_table ASSIGNING <table>.

        tabix = sy-tabix.
        ASSIGN COMPONENT <table>-fieldname OF STRUCTURE <excel> TO <fs_campo>.

        CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = w_line #2 = tabix.
        SET PROPERTY OF w_cell 'Value' = <fs_campo>.

        IF cor_total EQ abap_true.
          CALL METHOD OF w_cell 'INTERIOR' = lo_interior.
          SET PROPERTY OF lo_interior 'COLORINDEX' = cor_number.
        ENDIF.

        CASE <table>-fieldname.
          WHEN 'SAFRA'.
            CALL METHOD OF w_cell 'INTERIOR' = lo_interior.
            SET PROPERTY OF lo_interior 'COLORINDEX' = 48.

          WHEN 'PRECO'.
            vl_string = <fs_campo>.
            TRANSLATE vl_string USING ',.'.

            vl_valor_4 = vl_string.
            WRITE vl_valor_4 TO vl_convert.
            CONDENSE vl_convert NO-GAPS.

            CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = w_line #2 = tabix.
            SET PROPERTY OF w_cell  'HorizontalAlignment' = 4.
            SET PROPERTY OF w_cell 'Value' = vl_convert.

          WHEN 'TOTAL' OR 'PRECO_LB'.

            vl_string = <fs_campo>.
            TRANSLATE vl_string USING ',.'.

            vl_valor_2 = vl_string.
            WRITE vl_valor_2 TO vl_convert.
            CONDENSE vl_convert NO-GAPS.

            CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = w_line #2 = tabix.
            SET PROPERTY OF w_cell  'HorizontalAlignment' = 4.
            SET PROPERTY OF w_cell 'Value' = vl_convert.

          WHEN OTHERS.

            CHECK <table>-col_pos > 6.

            vl_string = <fs_campo>.

            vl_valor_0 = vl_string.

            WRITE vl_valor_0 TO vl_convert.
            TRANSLATE vl_convert USING '.,'.
            CONDENSE vl_convert NO-GAPS.

            CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = w_line #2 = tabix.
            SET PROPERTY OF w_cell  'HorizontalAlignment' = 4.
            SET PROPERTY OF w_cell 'Value' = vl_convert.

        ENDCASE.

      ENDLOOP.

      ADD 1 TO w_line.
    ENDLOOP.

    DATA(line_) = w_line.

    ADD 1 TO w_line.
    CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = w_line #2 = 1.
    SET PROPERTY OF w_cell 'Value' = 'Projeção - Tons'.
    CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = w_line #2 = 2.
    SET PROPERTY OF w_cell  'HorizontalAlignment' = 4.
    SET PROPERTY OF w_cell 'Value' = _projecao-qtd_c.

    ADD 1 TO w_line.
    CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = w_line #2 = 1.
    SET PROPERTY OF w_cell 'Value' = 'Preço'.
    CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = w_line #2 = 2.
    SET PROPERTY OF w_cell  'HorizontalAlignment' = 4.
    SET PROPERTY OF w_cell 'Value' = _projecao-preco_c.

    ADD 1 TO w_line.
    CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = w_line #2 = 1.
    SET PROPERTY OF w_cell 'Value' = '% Vendido'.
    CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = w_line #2 = 2.
    SET PROPERTY OF w_cell  'HorizontalAlignment' = 4.
    SET PROPERTY OF w_cell 'Value' = _projecao-porcentagem_c.

    CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = line_ #2 = 1.
    CALL METHOD OF w_excel 'Cells' = w_cell1 EXPORTING #1 = w_line #2 = 2.
    CALL METHOD OF w_excel 'Range' = w_cells EXPORTING #1 = w_cell #2 = w_cell1.

    CALL METHOD OF w_cells 'Rows' = h_rows.
    GET PROPERTY OF h_rows 'Font' = h_font.
    SET PROPERTY OF h_font 'Bold' = 1.

    CALL METHOD OF w_excel 'Columns' = h_columns EXPORTING #1 = 'A:Q'.
    GET PROPERTY OF h_columns 'EntireColumn' = h_entirecol.
    SET PROPERTY OF h_entirecol 'Autofit' = 1.

    FREE OBJECT: w_excel,
                 w_workbooks,
                 w_workbook,
                 w_cell.

  ENDMETHOD.

  METHOD qtd_projecao.

    SELECT SUM( quantidade )
      FROM zsdt0171
        INTO return
        WHERE safra IN p_safra.

  ENDMETHOD.

  METHOD consolida_dados.

    DATA: vlr_s TYPE string,
          vlr_p TYPE p DECIMALS 1.


    it_safra = VALUE #( FOR ls IN it_relatorio
                            (
                                safra = ls-safra
                                desc_cliente = ls-desc_cliente
                            )
                        ).

    it_cliente = it_safra.

    SORT it_relatorio BY safra desc_cliente.
    SORT it_safra BY safra.
    SORT it_cliente BY  safra desc_cliente.

    DELETE ADJACENT DUPLICATES FROM it_safra COMPARING safra.
    DELETE ADJACENT DUPLICATES FROM it_cliente COMPARING safra desc_cliente.

    DATA: cont    TYPE p DECIMALS 1,
          cont1   TYPE p DECIMALS 1,
          safra   TYPE ajahr,
          cliente TYPE name1.

    LOOP AT it_safra ASSIGNING FIELD-SYMBOL(<_safra>).

      LOOP AT it_relatorio ASSIGNING FIELD-SYMBOL(<_rel>)  WHERE safra EQ <_safra>-safra.

        ADD 1 TO cont.
        cont1 = cont.
        <_rel>-ordem = cont.

        LOOP AT it_cliente ASSIGNING FIELD-SYMBOL(<_cliente>) WHERE safra EQ <_rel>-safra AND desc_cliente EQ <_rel>-desc_cliente.

          ADD 1 TO cont.
          <_cliente>-ordem = cont.

        ENDLOOP.

      ENDLOOP.

      ADD 1 TO cont.
      <_safra>-ordem = cont.

    ENDLOOP.

    LOOP AT it_cliente ASSIGNING FIELD-SYMBOL(<fs>).
      LOOP AT it_relatorio ASSIGNING <_rel>
        WHERE desc_cliente EQ <fs>-desc_cliente
          AND safra        EQ <fs>-safra.

        <fs>-safra        = <_rel>-safra.
        <fs>-desc_cliente = |Total - { <_rel>-desc_cliente }|.

        ADD <_rel>-jan      TO <fs>-jan.
        ADD <_rel>-fev      TO <fs>-fev.
        ADD <_rel>-mar      TO <fs>-mar.
        ADD <_rel>-abr      TO <fs>-abr.
        ADD <_rel>-mai      TO <fs>-mai.
        ADD <_rel>-jun      TO <fs>-jun.
        ADD <_rel>-jul      TO <fs>-jul.
        ADD <_rel>-ago      TO <fs>-ago.
        ADD <_rel>-set      TO <fs>-set.
        ADD <_rel>-out      TO <fs>-out.
        ADD <_rel>-nov      TO <fs>-nov.
        ADD <_rel>-dev      TO <fs>-dev.
        ADD <_rel>-qtd_tons TO <fs>-qtd_tons.
        ADD <_rel>-total    TO <fs>-total.

        TRY .
            <fs>-preco = ( ( <fs>-total / <fs>-qtd_tons ) / 1000 ) / vlr.
          CATCH cx_sy_zerodivide.
        ENDTRY.

        TRY .
            <fs>-preco_lb = ( ( <fs>-total / <fs>-qtd_tons ) / 1000 ) * 15.
          CATCH cx_sy_zerodivide.
        ENDTRY.

        <fs>-color = 'C300'.

      ENDLOOP.
    ENDLOOP.

    LOOP AT it_safra ASSIGNING <fs>.
      LOOP AT it_relatorio INTO DATA(wa_rel)
        WHERE safra        EQ <fs>-safra.

        <fs>-safra        = |Total { wa_rel-safra }|.
        <fs>-desc_cliente = ''.

        ADD wa_rel-jan      TO <fs>-jan.
        ADD wa_rel-fev      TO <fs>-fev.
        ADD wa_rel-mar      TO <fs>-mar.
        ADD wa_rel-abr      TO <fs>-abr.
        ADD wa_rel-mai      TO <fs>-mai.
        ADD wa_rel-jun      TO <fs>-jun.
        ADD wa_rel-jul      TO <fs>-jul.
        ADD wa_rel-ago      TO <fs>-ago.
        ADD wa_rel-set      TO <fs>-set.
        ADD wa_rel-out      TO <fs>-out.
        ADD wa_rel-nov      TO <fs>-nov.
        ADD wa_rel-dev      TO <fs>-dev.
        ADD wa_rel-qtd_tons TO <fs>-qtd_tons.
        ADD wa_rel-total    TO <fs>-total.

        TRY .
            <fs>-preco = ( ( <fs>-total / <fs>-qtd_tons ) / 1000 ) / vlr.
          CATCH cx_sy_zerodivide.
        ENDTRY.

        TRY .
            <fs>-preco_lb = ( ( <fs>-total / <fs>-qtd_tons ) / 1000 ) * 15.
          CATCH cx_sy_zerodivide.
        ENDTRY.

        <fs>-color = 'C310'.

      ENDLOOP.
    ENDLOOP.

    LOOP AT it_relatorio INTO wa_rel.

      wa_geral-ordem = 9999.

      wa_geral-safra        = |Total Geral|.

      ADD wa_rel-jan      TO wa_geral-jan.
      ADD wa_rel-fev      TO wa_geral-fev.
      ADD wa_rel-mar      TO wa_geral-mar.
      ADD wa_rel-abr      TO wa_geral-abr.
      ADD wa_rel-mai      TO wa_geral-mai.
      ADD wa_rel-jun      TO wa_geral-jun.
      ADD wa_rel-jul      TO wa_geral-jul.
      ADD wa_rel-ago      TO wa_geral-ago.
      ADD wa_rel-set      TO wa_geral-set.
      ADD wa_rel-out      TO wa_geral-out.
      ADD wa_rel-nov      TO wa_geral-nov.
      ADD wa_rel-dev      TO wa_geral-dev.
      ADD wa_rel-qtd_tons TO wa_geral-qtd_tons.
      ADD wa_rel-total    TO wa_geral-total.

      TRY .
          wa_geral-preco = ( ( wa_geral-total / wa_geral-qtd_tons ) / 1000 ) / vlr.
        CATCH cx_sy_zerodivide.
      ENDTRY.

      TRY .
          wa_geral-preco_lb = ( ( wa_geral-total / wa_geral-qtd_tons ) / 1000 ) * 15.
        CATCH cx_sy_zerodivide.
      ENDTRY.

      wa_geral-color = 'C500'.

    ENDLOOP.

    CASE abap_true.
      WHEN r_relat.

        IF consolidar=>qtd_projecao( ) IS INITIAL.
          MESSAGE 'Não foi encontrada projeção cadastrada!' TYPE 'S'.
          EXIT.
        ENDIF.

        _projecao = VALUE #(
                             qtd         = CONV #( consolidar=>qtd_projecao( ) / 1000 )
                             preco       = CONV #( wa_geral-total / wa_geral-qtd_tons )
                             porcentagem = CONV #( ( wa_geral-qtd_tons / ( consolidar=>qtd_projecao( ) / 1000 ) ) * 100 )
                          ).

        WRITE _projecao-qtd TO _projecao-qtd_c.
        WRITE _projecao-preco TO _projecao-preco_c.
        _projecao-porcentagem_c = _projecao-porcentagem.
        _projecao-porcentagem_c = |{ _projecao-porcentagem_c ALPHA = OUT }|.

        CONDENSE: _projecao-qtd_c,
                  _projecao-preco_c,
                  _projecao-porcentagem_c
         NO-GAPS.

        _projecao-preco_c = |${ _projecao-preco_c }|.
        _projecao-porcentagem_c = |{ _projecao-porcentagem_c }%|.

    ENDCASE.

    APPEND LINES OF it_cliente TO it_relatorio.
    APPEND LINES OF it_safra TO it_relatorio.

    SORT it_relatorio BY ordem safra desc_cliente.

    APPEND wa_geral TO it_geral.

    IF p_safra-low IS INITIAL.
      APPEND LINES OF it_geral TO it_relatorio.
    ENDIF.

  ENDMETHOD.

  METHOD creat_doc.

*    IF DOCKING IS NOT INITIAL.
*      CALL METHOD DOCKING->FREE.
*    ENDIF.

    CREATE OBJECT docking
      EXPORTING
        repid     = sy-repid
        dynnr     = '0500'
        side      = cl_gui_docking_container=>align_at_left
        extension = _extension.

    CREATE OBJECT splitter
      EXPORTING
        link_dynnr = sy-dynnr
        link_repid = sy-repid
        parent     = docking
        rows       = 1 "2
        columns    = 1.

    splitter->set_row_height( id = 1 height = 50 ).
    splitter->set_border( cl_gui_cfw=>false ).

    custom_grid   = splitter->get_container( row = 1 column = 1 ).

    CREATE OBJECT splitter1
      EXPORTING
        parent  = custom_grid
        rows    = 1
        columns = 1. "2

    splitter1->set_column_width( id = 1 width = 50 ).
    splitter1->set_border( cl_gui_cfw=>false ).

    custom_grid  = splitter1->get_container( row = 1 column = 1 ).

    PERFORM: fcat USING abap_true.

    _layout =
    VALUE #(
              zebra = abap_true
              cwidth_opt = abap_true
           ).

    IF grid IS INITIAL.
      CREATE OBJECT grid
        EXPORTING
          i_parent = custom_grid.

      SET HANDLER: consolidar=>handle_user_command FOR grid,
                   consolidar=>handle_double_click FOR grid,
                   consolidar=>handle_hotspot_click2 FOR grid.  "*-CS2023000189-04.04.2023-#108693-JT

*      it_exclud = VALUE #( ( cl_gui_alv_grid=>mc_fc_excl_all ) ).

      CALL METHOD grid->set_table_for_first_display
        EXPORTING
          is_layout            = _layout
          it_toolbar_excluding = it_exclud
        CHANGING
          it_outtab            = it_saida
          it_fieldcatalog      = it_fcat.
    ELSE.
      CALL METHOD grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.
  ENDMETHOD.

  METHOD handle_user_command.

    CALL METHOD grid->get_selected_rows
      IMPORTING
        et_index_rows = DATA(selected_row).

    CASE e_ucomm.
      WHEN 'CLOSE'.
*        SPLITTER1->SET_COLUMN_WIDTH( ID = 1 WIDTH = 100 ).
      WHEN 'REL'.
*        PRINT_SMARTFORMS( ).
    ENDCASE.

  ENDMETHOD.

  METHOD handle_double_click.
    CLEAR: lv_acao, l_copia.  "*-CS2021000532-#84648-09.08.2022-JT
    CLEAR at_id.
    at_id = es_row_no-row_id.
    get_dados( 'C' ).
    CALL SCREEN 0510.
  ENDMETHOD.

  METHOD handle_hotspot_click2.
    PERFORM handle_hotspot_click USING e_row_id e_column_id es_row_no.
  ENDMETHOD.                    "handle_hotspot_click


  METHOD dt_changed_f_preco.

    DATA: w_preco_n   LIKE LINE OF t_preco,
          w_preco_in  TYPE zsds006,
          t_preco_in  TYPE TABLE OF zsds006,
          t_preco_out TYPE TABLE OF zsds006,
          w_0101      TYPE zsdt0101.

    CLEAR w_tabix.

    IF w_contrato-tipo_padrao IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
        EXPORTING
          input  = w_contrato-tipo_padrao
        IMPORTING
          output = vmatnr18. "lwa_zsdt0143-tipo_padrao.

      SELECT SINGLE matkl
        FROM mara
        INTO @DATA(_matkl)
        WHERE matnr EQ @vmatnr18.

      IF _matkl IS NOT INITIAL.

        SELECT SINGLE conversor
          FROM zsdt0101
          INTO @DATA(_conversor)
          WHERE matkl EQ @_matkl.

      ENDIF.

    ENDIF.


    LOOP AT et_good_cells INTO DATA(l_good).

      UNASSIGN <fs_line>.
      CREATE DATA t_new_line LIKE LINE OF <fs_table>.
      ASSIGN t_new_line->* TO <fs_line>.

      TRY .
          <fs_line> = <fs_table>[ l_good-row_id ].
        CATCH cx_sy_itab_line_not_found.
          UNASSIGN <fs_line>.
      ENDTRY.

      get_valores(
        EXPORTING
          fieldname = 'COD_FP'
        CHANGING
          vl_any    = w_preco-cod_fp ).

      LOOP AT t_fields INTO DATA(w_fields).

        TRY.
            w_preco = t_preco[ cod_fp = w_preco-cod_fp
                               field  = w_fields-field ].
          CATCH cx_sy_itab_line_not_found.
            CLEAR w_preco.
        ENDTRY.

        CLEAR w_valor.
        get_valores(
          EXPORTING
            fieldname = w_fields-field
          CHANGING
            vl_any    = w_valor ).

        convert_vl( EXPORTING decimais = w_preco-c_decimais CHANGING vl = w_valor ).

        set_valores(
          EXPORTING
            fieldname = w_fields-field
          CHANGING
            vl_any    = w_valor ).

      ENDLOOP.

      MODIFY <fs_table> FROM <fs_line> INDEX l_good-row_id.

    ENDLOOP.

    IF sy-subrc IS INITIAL.

      LOOP AT <fs_table> ASSIGNING <fs_line>.

        get_valores(
          EXPORTING
            fieldname = 'COD_FP'
          CHANGING
            vl_any    = w_valor ).

        CONDENSE w_valor NO-GAPS.

        LOOP AT t_preco INTO w_preco
          WHERE cod_fp EQ w_valor.

          MOVE-CORRESPONDING: <fs_line> TO w_preco_in.

          MOVE: w_preco-field TO w_preco_in-field.
          MOVE: w_preco-nivel TO w_preco_in-nivel.
          MOVE: w_preco-c_decimais TO w_preco_in-c_decimais.

          IF w_preco-tipo_calc EQ 'C'
          OR w_preco-tipo_calc EQ 'R'.
            MOVE: w_preco-formula TO w_preco_in-formula.
          ELSE.
            IF w_preco_in-bezei NE 'CONVERSOR'.
              get_valores(
                EXPORTING
                  fieldname = w_preco-field
                CHANGING
                  vl_any    = w_valor ).

              TRANSLATE w_valor USING '. '.
              TRANSLATE w_valor USING ',.'.
            ELSE.
              MOVE _conversor TO w_valor.
            ENDIF.

            CONDENSE w_valor NO-GAPS.

            MOVE: w_valor TO w_preco_in-formula.
            CONDENSE w_preco_in-formula NO-GAPS.
          ENDIF.

          APPEND w_preco_in TO t_preco_in.
          CLEAR: w_preco_in.

        ENDLOOP.
      ENDLOOP.

      CALL FUNCTION 'ZSDMF003_REALIZA_CALCULO'
        EXPORTING
          i_repid     = sy-repid
        TABLES
          ti_esq_calc = t_preco_in
          te_esq_calc = t_preco_out.

      LOOP AT <fs_table> ASSIGNING <fs_line>.
        LOOP AT t_fields INTO w_fields.

          CLEAR: w_valor.
          get_valores(
            EXPORTING
              fieldname = 'NIVEL'
            CHANGING
              vl_any    = w_valor ).
          CONDENSE w_valor NO-GAPS.

          CLEAR: w_valor_aux.
          get_valores(
            EXPORTING
              fieldname = 'COD_FP'
            CHANGING
              vl_any    = w_valor_aux ).
          CONDENSE w_valor_aux NO-GAPS.

          w_tabix = sy-tabix.

          TRY.
              w_preco_in =
              t_preco_out[ field  = w_fields-field
                           cod_fp = w_valor_aux ].
            CATCH cx_sy_itab_line_not_found.
              CLEAR w_preco_in.
          ENDTRY.

          IF w_preco_in IS NOT INITIAL.

            MOVE-CORRESPONDING: w_preco_in TO <fs_line>.

            TRY.
                w_preco =
                t_preco[ field  = w_fields-field
                         cod_fp = w_valor_aux ].
              CATCH cx_sy_itab_line_not_found.
                CLEAR w_preco.
            ENDTRY.

            CONDENSE w_preco_in-formula NO-GAPS.

            CASE w_preco-c_decimais.
              WHEN 2.
                p_2 = w_preco_in-formula.
                WRITE p_2 TO w_preco_in-formula.
              WHEN 4.
                p_4 = COND #( WHEN w_preco_in-bezei NE 'CONVERSOR' THEN w_preco_in-formula ELSE _conversor ).
                WRITE p_4 TO w_preco_in-formula.
            ENDCASE.

            CONDENSE w_preco_in-formula NO-GAPS.
            set_valores(
              EXPORTING
                fieldname = w_fields-field
              CHANGING
                vl_any    = w_preco_in-formula ).
          ELSE.
            CLEAR: w_preco_in.
            CONDENSE w_preco_in-formula NO-GAPS.
            set_valores(
              EXPORTING
                fieldname = w_fields-field
              CHANGING
                vl_any    = w_preco_in-formula ).
          ENDIF.
        ENDLOOP.

        CLEAR:w_valor_aux.
        get_valores(
          EXPORTING
            fieldname = 'TIPO_CALC'
          CHANGING
            vl_any    = w_valor_aux ).

        CONDENSE w_valor_aux NO-GAPS.

        IF w_valor_aux EQ 'C'.
          CLEAR:w_valor_aux.
          get_valores(
            EXPORTING
              fieldname = 'NIVEL'
            CHANGING
              vl_any    = w_valor_aux ).
          CONDENSE w_valor_aux NO-GAPS.

          CLEAR:w_valor.
          get_valores(
            EXPORTING
              fieldname = 'COD_FP'
            CHANGING
              vl_any    = w_valor ).

          CONDENSE w_valor_aux NO-GAPS.
          TRY.
              w_preco_in =
                        t_preco_in[ nivel  = w_valor_aux
                                    cod_fp = w_valor ].
            CATCH cx_sy_itab_line_not_found.
              CLEAR w_preco_in.
          ENDTRY.
          set_valores(
            EXPORTING
              fieldname = 'FORMULA'
            CHANGING
              vl_any    = w_preco_in-formula ).

        ENDIF.
        CLEAR: w_preco_in, w_preco.

      ENDLOOP.
    ENDIF.

    IF grid1 IS NOT INITIAL.
      CALL METHOD grid1->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.

  ENDMETHOD.

  METHOD set_valores.
    ASSIGN COMPONENT fieldname  OF STRUCTURE <fs_line> TO <fs_campo>.
    MOVE vl_any TO <fs_campo>.
  ENDMETHOD.

  METHOD get_valores.
    ASSIGN COMPONENT fieldname  OF STRUCTURE <fs_line> TO <fs_campo>.
    MOVE <fs_campo> TO vl_any.
  ENDMETHOD.

  METHOD get_estrutura_preco.

    CLEAR w_tabix.

    SELECT SINGLE *
      FROM zsdt0057
       INTO @DATA(w_0057)
       WHERE tp_venda EQ @w_contrato-tp_venda
         AND param_espec IN ('A', 'Z').

    IF sy-subrc IS INITIAL.

      w_contrato-desc_tp_venda = w_0057-bezei.

      SELECT *
        FROM zsdt0058
        INTO TABLE @DATA(t_0058)
         WHERE esq_calc EQ @w_0057-esq_calc.

      IF sy-subrc IS INITIAL.

        SELECT *
          FROM zsdt0056
          INTO TABLE @DATA(t_0056)
           FOR ALL ENTRIES IN @t_0058
           WHERE cod_fp EQ @t_0058-cod_fp.

        IF sy-subrc IS INITIAL.

          SELECT *
            FROM zsdt0070
            INTO TABLE @DATA(t_0070)
             FOR ALL ENTRIES IN @t_0056
             WHERE cod_fp EQ @t_0056-cod_fp.

        ENDIF.
      ENDIF.

      FREE t_fieldcatalog.
      CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
        EXPORTING
          i_tp_venda      = w_0057-tp_venda
        IMPORTING
          e_table         = t_new_table
        TABLES
          te_fieldcatalog = t_fieldcatalog
          te_fields       = t_fields.

      IF grid1 IS NOT INITIAL.
        CALL METHOD grid1->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = t_fieldcatalog.
      ENDIF.

* Cria uma field-symbol como tabela interna
      ASSIGN t_new_table->* TO <fs_table>.
      CREATE DATA t_new_line LIKE LINE OF <fs_table>.
      CREATE DATA t_new_table LIKE <fs_table>.

* Cria uma field-symbol como work area
      ASSIGN t_new_line->* TO <fs_line>.
      ASSIGN t_new_line->* TO <fs_line_aux>.

      FREE t_preco.
      LOOP AT t_0058 INTO DATA(_0058).

        TRY .
            DATA(_0056) = t_0056[ cod_fp = _0058-cod_fp ].
          CATCH cx_sy_itab_line_not_found.
            CLEAR _0056.
        ENDTRY.

        MOVE-CORRESPONDING _0058 TO w_preco.
        MOVE-CORRESPONDING _0058 TO <fs_line>.

        MOVE-CORRESPONDING _0056 TO w_preco.
        MOVE-CORRESPONDING _0056 TO <fs_line>.

        consolidar=>set_valores(
          EXPORTING
            fieldname = 'WAERS'
          CHANGING
            vl_any    = _0056-waers ).

        consolidar=>set_valores(
          EXPORTING
            fieldname = 'OCBOT'
          CHANGING
            vl_any    = _0056-ocbot ).

        consolidar=>set_valores(
          EXPORTING
            fieldname = 'INVISIBLE'
          CHANGING
            vl_any    = _0056-invisible ).

        MOVE: _0056-waers     TO w_preco-waers,
              _0056-ocbot     TO w_preco-ocbot,
              _0056-invisible TO w_preco-invisible.

        TRY .
            DATA(_0070) = t_0070[ cod_fp = _0056-cod_fp
                                   field  = _0058-field ].
          CATCH cx_sy_itab_line_not_found.
            CLEAR _0070.
        ENDTRY.

        IF _0070 IS NOT INITIAL.

          consolidar=>set_valores(
            EXPORTING
              fieldname = 'TIPO_CALC'
            CHANGING
              vl_any    = _0070-tipo_calc ).

          consolidar=>set_valores(
            EXPORTING
              fieldname = 'C_DECIMAIS'
            CHANGING
              vl_any    = _0070-c_decimais ).

          consolidar=>set_valores(
            EXPORTING
              fieldname = 'PRECO_ITEM'
            CHANGING
              vl_any    = _0070-preco ).

          MOVE: _0070-field      TO w_preco-field,
                _0070-tipo_calc  TO w_preco-tipo_calc,
                _0070-c_decimais TO w_preco-c_decimais,
                _0070-preco      TO w_preco-preco.

        ENDIF.

        MOVE: _0058-formula TO w_preco-formula.
        consolidar=>set_valores(
          EXPORTING
            fieldname = 'FORMULA'
          CHANGING
            vl_any    = _0058-formula ).

        CLEAR w_valor.

        CASE w_preco-tipo_calc.
          WHEN 'C' OR 'R'.
            w_valor = 'C305'.

            consolidar=>set_valores(
              EXPORTING
                fieldname = 'LINE_COLOR'
              CHANGING
                vl_any    = w_valor ).

          WHEN OTHERS.
            consolidar=>set_valores(
              EXPORTING
                fieldname = 'LINE_COLOR'
              CHANGING
                vl_any    = w_valor ).
        ENDCASE.

        IF w_preco-tipo_calc EQ 'F'.
          CONDENSE _0058-formula NO-GAPS.

          CASE w_preco-c_decimais.
            WHEN 2.
              p_2 = _0058-formula.
              WRITE p_2 TO w_preco-formula.
            WHEN 4.
              p_4 = _0058-formula.
              WRITE p_4 TO w_preco-formula.
            WHEN 5.
              p_5 = _0058-formula.
              WRITE p_5 TO w_preco-formula.
          ENDCASE.

          CONDENSE w_preco-formula NO-GAPS.

          consolidar=>set_valores(
            EXPORTING
              fieldname = _0058-field
            CHANGING
              vl_any    = w_preco-formula ).

        ELSE.
          w_valor_aux = space.
          consolidar=>set_valores(
            EXPORTING
              fieldname = _0058-field
            CHANGING
              vl_any    = w_valor_aux ).
        ENDIF.

        APPEND w_preco TO t_preco.

        LOOP AT <fs_table> ASSIGNING <fs_line_aux>.
          CLEAR: w_valor, w_tabix.

          consolidar=>get_valores(
            EXPORTING
              fieldname = 'COD_FP'
            CHANGING
              vl_any    = w_valor ).

          CLEAR w_valor_aux.
          ASSIGN COMPONENT 'COD_FP'  OF STRUCTURE <fs_line_aux> TO <fs_campo>.
          w_valor_aux = <fs_campo>.
          IF w_valor EQ w_valor_aux.
            w_tabix = sy-tabix.
          ENDIF.
        ENDLOOP.
        IF w_tabix IS INITIAL.
          APPEND <fs_line> TO <fs_table>.
        ELSEIF w_tabix IS NOT INITIAL.
          MODIFY <fs_table> FROM <fs_line> INDEX w_tabix.
        ENDIF.

        CLEAR: _0056, w_preco, _0070, <fs_line>.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD get_dados.

    IF p_tipo <> 'C'.  "BUG #83141
      PERFORM: seleciona_dados.
    ENDIF.

    UNASSIGN <fs_table>.
    UNASSIGN <fs_line>.

    TRY .
        w_contrato = it_saida[ at_id ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR w_contrato.
    ENDTRY.

*    PERFORM get_frame.
*    SELECT *
*      FROM zsdt0227
*      INTO TABLE t_0227
*      WHERE id_contrato EQ w_contrato-id_contrato.
*
*    FREE: t_fieldcatalog.
*    CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
*      EXPORTING
*        i_tp_venda      = w_contrato-tp_venda
*      IMPORTING
*        e_table         = t_new_table
*      TABLES
*        te_fieldcatalog = t_fieldcatalog
*        te_fields       = t_fields.
*
*    ASSIGN t_new_table->* TO <fs_table>.
*    CREATE DATA t_new_line LIKE LINE OF <fs_table>.
*    CREATE DATA t_new_table LIKE <fs_table>.
*    ASSIGN t_new_line->* TO <fs_line>.
*
*    SORT: t_0227 BY nivel ASCENDING cod_fp ASCENDING.
*    LOOP AT t_0227 INTO w_0227.
*
*      MOVE-CORRESPONDING: w_0227 TO <fs_line>.
*
*      consolidar=>set_valores(
*        EXPORTING
*          fieldname = 'TIPO_CALC'
*        CHANGING
*          vl_any    = w_0227-tipo_calc ).
*
*      consolidar=>set_valores(
*        EXPORTING
*          fieldname = 'C_DECIMAIS'
*        CHANGING
*          vl_any    = w_0227-c_decimais ).
*
*      consolidar=>set_valores(
*        EXPORTING
*          fieldname = 'PRECO_ITEM'
*        CHANGING
*          vl_any    = w_0227-preco ).
*
*      consolidar=>set_valores(
*        EXPORTING
*          fieldname = 'WAERS'
*        CHANGING
*          vl_any    = w_0227-waers ).
*
*      consolidar=>set_valores(
*        EXPORTING
*          fieldname = 'OCBOT'
*        CHANGING
*          vl_any    = w_0227-ocbot ).
*
*      CONDENSE w_0227-formula2 NO-GAPS.
*      CASE w_0227-c_decimais.
*        WHEN 2.
*          p_2 = w_0227-formula2.
*          WRITE p_2 TO w_preco-formula.
*        WHEN 4.
*          p_4 = w_0227-formula2.
*          WRITE p_4 TO w_preco-formula.
*        WHEN 5.
*          p_5 = w_0227-formula2.
*          WRITE p_5 TO w_preco-formula.
*      ENDCASE.
*      CONDENSE w_preco-formula NO-GAPS.
*
*      consolidar=>set_valores(
*        EXPORTING
*          fieldname = w_0227-field
*        CHANGING
*          vl_any    = w_preco-formula ).
*
*      CLEAR: w_tabix.
*      LOOP AT <fs_table> ASSIGNING <fs_line_aux>.
*
*        CLEAR: w_valor, w_tabix.
*        consolidar=>get_valores(
*          EXPORTING
*            fieldname = 'COD_FP'
*          CHANGING
*            vl_any    = w_valor ).
*
*        CLEAR w_valor_aux.
*        ASSIGN COMPONENT 'COD_FP'  OF STRUCTURE <fs_line_aux> TO <fs_campo>.
*        w_valor_aux = <fs_campo>.
*        IF w_valor EQ w_valor_aux.
*          CLEAR w_valor_aux.
*          ASSIGN COMPONENT 'POSNR'  OF STRUCTURE <fs_line_aux> TO <fs_campo>.
*          w_valor_aux = <fs_campo>.
*          IF w_0227-posnr EQ w_valor_aux.
*            ASSIGN COMPONENT w_0227-field  OF STRUCTURE <fs_line_aux> TO <fs_campo>.
*            <fs_campo> = w_preco-formula.
*            w_tabix = sy-tabix.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*
*      IF w_tabix IS INITIAL.
*        APPEND <fs_line> TO <fs_table>.
*      ENDIF.
*
*      MOVE-CORRESPONDING: w_0227 TO w_preco.
*      IF w_0227-tipo_calc EQ 'C'
*      OR w_0227-tipo_calc EQ 'R'.
*        MOVE: w_0227-formula TO w_preco-formula.
*      ELSE.
*        CASE w_0227-c_decimais.
*          WHEN 2.
*            p_2 = w_0227-formula2.
*            WRITE p_2 TO w_preco-formula.
*          WHEN 4.
*            p_4 = w_0227-formula2.
*            WRITE p_4 TO w_preco-formula.
*          WHEN 5.
*            p_5 = w_0227-formula2.
*            WRITE p_5 TO w_preco-formula.
*        ENDCASE.
*        CONDENSE w_preco-formula NO-GAPS.
*      ENDIF.
*      APPEND w_preco TO t_preco.
*
*      CLEAR: w_preco, w_0227, <fs_line>.
*    ENDLOOP.
*
*    IF grid IS NOT INITIAL.
*      CALL METHOD grid->refresh_table_display
*        EXPORTING
*          is_stable = wa_stable.
*    ENDIF.

  ENDMETHOD.

  METHOD convert_vl.

    DATA: v_2(13) TYPE p DECIMALS 2,
          v_5(13) TYPE p DECIMALS 5,
          v_4(13) TYPE p DECIMALS 4.

    TRANSLATE vl USING '. '.
    TRANSLATE vl USING ',.'.
    CONDENSE vl NO-GAPS.

    TRY.
        CASE decimais.
          WHEN 2.
            v_2 = vl.
            WRITE v_2 TO vl.
          WHEN 4.
            v_4 = vl.
            WRITE v_4 TO vl.
          WHEN 5.
            v_5 = vl.
            WRITE v_5 TO vl.
        ENDCASE.
      CATCH cx_sy_conversion_no_number INTO DATA(obj_exc).
        DATA(exc_text) = obj_exc->get_text( ).
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH |Formato númerico não aceito: { exc_text }|.
    ENDTRY.

    CONDENSE vl NO-GAPS.
  ENDMETHOD.

  METHOD save_dados.
    DATA: l_error TYPE char1.
*-CS2021000532-#84648-09.08.2022-JT-inicio
    IF p_acao = 'C'.
      CLEAR w_contrato-id_contrato.
    ENDIF.
*-CS2021000532-#84648-09.08.2022-JT-fim

    consolidar=>verifica_erros( ).

    IF t_msg IS NOT INITIAL.
      MESSAGE |Existe Erros a serem verificados!| TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    DATA w_0043 TYPE zsdt0143.
    DATA: seq TYPE zsdt0143-id_contrato.

*** US - 77507 - cbrand - inicio
*    IF w_contrato-visao = 'E' AND w_contrato-intercompany = 'X' AND w_contrato-id_contrato_referencia IS INITIAL .
*      MESSAGE |Obrigatório preenchimento "ID Contrato Referência"!| TYPE 'S' DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
*** US - 77507 - CBRAND - Fim

*-CS2023000189-04.04.2023-#108693-JT-inicio
* verifica se ACTS modificou
    SELECT SINGLE *
      FROM zsdt0143
      INTO @DATA(w_modif)
     WHERE id_contrato EQ @w_contrato-id_contrato.

    IF w_modif-acts <> w_contrato-acts.
      PERFORM f_gravalog_acts USING w_contrato-id_contrato
                                    w_modif-acts
                                    w_contrato-acts.
    ENDIF.
*-CS2023000189-04.04.2023-#108693-JT-fim

    FREE: t_0227.

    CLEAR vmatnr18.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = w_contrato-tipo_padrao
      IMPORTING
        output       = vmatnr18
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
    DATA: v_cotton  TYPE c,
          v_usuario TYPE usnam,
          v_hora    TYPE uzeit,
          v_data    TYPE datum.

*             IF L_ENVIA_TRACE IS NOT INITIAL.
*               v_COTTON = 'X'.
*               v_USUARIO = sy-uname.
*               v_HORA   = sy-uzeit.
*               v_DATA   = sy-datum.
*               ENDIF.

    IF w_contrato-id_contrato IS NOT INITIAL.
      MOVE-CORRESPONDING w_contrato TO w_0043.
      w_0043-a_usnam = sy-uname.
      w_0043-a_data_atual = sy-datum.
      w_0043-a_hora_atual = sy-uzeit.
      w_0043-tipo_padrao = vmatnr18.
    ELSE.
      CLEAR lv_acao.  "*-CS2021000532-#84648-09.08.2022-JT-inicio

      SELECT COUNT( * ) FROM zsdt0143 INTO seq.
      ADD 1 TO seq.

      w_contrato-id_contrato = |{ seq ALPHA = IN }|.

      w_0043 =
      VALUE #(
              id_contrato = w_contrato-id_contrato
              tipo_padrao = vmatnr18 "|{ vmatnr18 ALPHA = IN }|
              corretor = |{ w_contrato-corretor ALPHA = IN }|
              cliente = |{ w_contrato-cliente ALPHA = IN }|
              cliente_final = |{ w_contrato-cliente_final ALPHA = IN }|
              contrato = w_contrato-contrato
              contrato_cliente = w_contrato-contrato_cliente
              safra = w_contrato-safra
              tp_venda = w_contrato-tp_venda
              dt_venda = w_contrato-dt_venda
              empresa = w_contrato-empresa
              vendedor = w_contrato-vendedor
              comissao = w_contrato-comissao
              porto = w_contrato-porto
              quatidade = w_contrato-quatidade
              tolerancia = w_contrato-tolerancia
              de_embarque = w_contrato-de_embarque
              ate_embarque = w_contrato-ate_embarque
              preco = w_contrato-preco
              preco_tons = w_contrato-preco_tons
              pctgem_ant = w_contrato-pctgem_ant
              qdt_embarcada = w_contrato-qdt_embarcada
*** US - 77507 - cbrand - inicio
              visao                    =     w_contrato-visao
              intercompany             =     w_contrato-intercompany
              id_contrato_referencia   =     w_contrato-id_contrato_referencia
*** US - 77507 - cbrand - Fim
              acts                     =     w_contrato-acts  "*-CS2023000189-04.04.2023-#108693-JT
              usnam = sy-uname
              data_atual = sy-datum
              hora_atual = sy-uzeit
              waerk = w_contrato-waerk
*              T_COTTON  =  v_COTTON
*              T_USUARIO =  v_USUARIO
*              T_HORA    =  v_HORA
*              T_DATA    =  v_DATA
      ).
      "CS2023000851 Algodão - PT1 - Atualização da transação ZSDT0118 - Formato já executado na Amaggi e mais detalhado --128361 - BG
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = w_contrato-tipo_padrao
*        IMPORTING
*          output = vmatnr18.
*      w_contrato-tipo_padrao = vmatnr18.

    ENDIF.

    MODIFY zsdt0143 FROM w_0043.

*-CS2023000189-04.04.2023-#108693-JT-inicio
*-Ajusta ACTS dos contratos dependentes
    SELECT *
      FROM zsdt0143
      INTO TABLE @DATA(t_0143a)
     WHERE id_contrato_referencia = @w_contrato-id_contrato.

    LOOP AT t_0143a    INTO DATA(w_0143a).
      w_0143a-acts        = w_contrato-acts.
      MODIFY zsdt0143  FROM w_0143a.
    ENDLOOP.
*-CS2023000189-04.04.2023-#108693-JT-fim

    COMMIT WORK.

* Preço
    UNASSIGN <fs_line>.
    LOOP AT <fs_table> ASSIGNING <fs_line>.

      CLEAR: w_valor.
      get_valores(
        EXPORTING
          fieldname = 'COD_FP'
        CHANGING
          vl_any    = w_valor ).
      CONDENSE w_valor NO-GAPS.

      LOOP AT t_preco INTO w_preco
        WHERE cod_fp EQ w_valor.

        MOVE-CORRESPONDING: w_preco TO w_0227.

        w_0227-id_contrato = w_contrato-id_contrato.
        w_0227-usnam       = sy-uname.
        w_0227-data_atual  = sy-datum.
        w_0227-hora_atual  = sy-uzeit.

        CLEAR: w_valor.
        get_valores(
          EXPORTING
            fieldname = w_preco-field
          CHANGING
            vl_any    = w_valor ).
        CONDENSE w_valor NO-GAPS.

        w_0227-formula2 = w_valor.

        TRANSLATE w_0227-formula2 USING '. '.
        TRANSLATE w_0227-formula2 USING ',.'.
        CONDENSE w_0227-formula2  NO-GAPS.

        IF w_0227-tipo_calc EQ 'V'.

          w_0227-formula = w_0227-formula2.

          TRANSLATE w_0227-formula USING '.,'.
          CONDENSE w_0227-formula  NO-GAPS.
        ENDIF.

        CLEAR: w_valor.
        get_valores(
          EXPORTING
            fieldname = 'WAERS'
          CHANGING
            vl_any    = w_valor ).
        CONDENSE w_valor NO-GAPS.
        w_0227-waers = w_valor.

        CLEAR: w_valor.
        get_valores(
          EXPORTING
            fieldname = 'INVISIBLE'
          CHANGING
            vl_any    = w_valor ).
        CONDENSE w_valor NO-GAPS.
        w_0227-invisible = w_valor.

        CLEAR: w_valor.
        get_valores(
          EXPORTING
            fieldname = 'CBOT'
          CHANGING
            vl_any    = w_valor ).
        CONDENSE w_valor NO-GAPS.
        w_0227-cbot = w_valor.

        CLEAR: w_valor.
        get_valores(
          EXPORTING
            fieldname = 'MONAT'
          CHANGING
            vl_any    = w_valor ).
        CONDENSE w_valor NO-GAPS.
        w_0227-monat = w_valor.

        CLEAR: w_valor.
        get_valores(
          EXPORTING
            fieldname = 'VALDT'
          CHANGING
            vl_any    = w_valor ).
        CONDENSE w_valor NO-GAPS.
        w_0227-valdt = w_valor.

        APPEND w_0227 TO t_0227.
        CLEAR: w_0227.

      ENDLOOP.
    ENDLOOP.

    DELETE FROM zsdt0227 WHERE id_contrato EQ w_contrato-id_contrato.
    COMMIT WORK.

    MODIFY zsdt0227 FROM TABLE t_0227.
    COMMIT WORK.

    CLEAR: acao, state.
    IF l_envia_trace IS NOT INITIAL.
*-----------------------------
*-------- cadastra contratos no Trace
*-----------------------------
      TRY .
          zcl_trace_cotton=>zif_trace_cotton~get_instance(
             )->set_cadastra_contratos( EXPORTING i_id_contrato = w_contrato-id_contrato ).

        CATCH zcx_integracao INTO DATA(ex_integra).
          l_error = abap_true.
*           l_mesg  = ex_integra->msgv1 && '-' && ex_integra->msgv2 && '-' && ex_integra->msgv3 && '-' && ex_integra->msgv4.

        CATCH zcx_error INTO DATA(ex_error).
          l_error = abap_true.
*           l_mesg  = ex_error->msgv1   && '-' && ex_error->msgv2   && '-' && ex_error->msgv3   && '-' && ex_error->msgv4.
      ENDTRY.


      IF edit IS NOT INITIAL.
        IF l_error = abap_false.
          MESSAGE |Contrato modificado e enviado ao Trace Cotton com Sucesso!| TYPE 'S'.
        ELSE.
          MESSAGE |Contrato modificado e salvo com sucesso. Erro ao enviar ao sistema Trace Cotton. Verificar Log!| TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      ELSE.
        IF l_error = abap_false.
          MESSAGE |Contrato Criado e enviado ao Sistema Trace Cotton com Sucesso!| TYPE 'S'.
        ELSE.
          MESSAGE |Contrato Criado e salvo com sucesso. Erro ao enviar ao sistema Trace Cotton. Verificar Log!| TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      ENDIF.


    ENDIF.

    consolidar=>get_dados( ).


    CALL METHOD grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    CLEAR: state, acao.

  ENDMETHOD.

  METHOD encerrar.
    CHECK w_contrato-id_contrato IS NOT INITIAL.
    CHECK w_contrato-contrato IS NOT INITIAL.

    SELECT *
      FROM zsdt0051
      INTO TABLE @DATA(t_solicitacao)
      WHERE bstkd EQ @w_contrato-contrato.



*    CHECK T_SOLICITACAO IS NOT INITIAL.
    IF ( t_solicitacao IS INITIAL ) AND ( w_contrato-sts_con = abap_false ).
      MESSAGE s836(sd) WITH 'Contrato não possui solicitação de venda.' DISPLAY LIKE 'E'.
      EXIT.
    ELSEIF  w_contrato-sts_con = 'E' .
      w_contrato-sts_con = COND #( WHEN w_contrato-sts_con EQ 'E' THEN abap_false ELSE 'E' ).
    ELSE.
      SELECT *
        FROM zsdt0053
        INTO TABLE @DATA(t_item)
        FOR ALL ENTRIES IN @t_solicitacao
        WHERE nro_sol_ov EQ @t_solicitacao-nro_sol_ov.

      IF w_contrato-sts_con EQ abap_false.
        w_contrato-qdt_embarcada =
        REDUCE zsded054( INIT x TYPE zsded054 FOR ls IN t_item
                                 NEXT x = x + ls-zmeng
                       ).
      ENDIF.
      w_contrato-sts_con = COND #( WHEN w_contrato-sts_con EQ 'E' THEN abap_false ELSE 'E' ).
    ENDIF.

    UPDATE zsdt0143 SET sts_con = w_contrato-sts_con
                        a_usnam = sy-uname
                        a_data_atual = sy-datum
                        a_hora_atual = sy-uzeit
                        qdt_embarcada = w_contrato-qdt_embarcada
    WHERE id_contrato EQ w_contrato-id_contrato.

    COMMIT WORK.

  ENDMETHOD.

  METHOD deletar.
    DATA: l_error TYPE char1.
    DATA: p_resp.

    CHECK w_contrato-id_contrato IS NOT INITIAL.

    IF NOT w_contrato-contrato IS INITIAL.

      SELECT SINGLE nro_sol_ov
        FROM zsdt0051
        INTO @DATA(nro_sol_ov)
        WHERE bstkd  EQ @w_contrato-contrato
          AND status <> 'D'.  "*-CS2023000189-04.04.2023-#108693-JT

      IF NOT sy-subrc IS INITIAL.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question         = 'Deseja Excluir o Contrato?'
            text_button_1         = 'Sim'
            text_button_2         = 'Não'
            display_cancel_button = ' '
          IMPORTING
            answer                = p_resp.

        CHECK p_resp EQ 1.

        UPDATE zsdt0143 SET cancelado    = abap_true
                            sts_con      = 'C'
                            c_usnam      = sy-uname
                            c_data_atual = sy-datum
                            c_hora_atual = sy-uzeit
                    WHERE id_contrato EQ w_contrato-id_contrato
                    AND cancelado EQ abap_false.
      ELSE.
        MESSAGE |Contrato Vinculado a Solicitação { nro_sol_ov }!| TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ELSE.
      MESSAGE |Contrato em Branco. Verificar!| TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

    FREE: it_fcat, it_saida.
*-----------------------------
*-------- exclui contratos no Trace
*-----------------------------
    TRY .
        zcl_trace_cotton=>zif_trace_cotton~get_instance(
           )->set_exclui_contratos( EXPORTING i_id_contrato = w_contrato-id_contrato ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        l_error = abap_true.
*             l_mesg  = ex_integra->msgv1 && '-' && ex_integra->msgv2 && '-' && ex_integra->msgv3 && '-' && ex_integra->msgv4.

      CATCH zcx_error INTO DATA(ex_error).
        l_error = abap_true.
*             l_mesg  = ex_error->msgv1   && '-' && ex_error->msgv2   && '-' && ex_error->msgv3   && '-' && ex_error->msgv4.
    ENDTRY.



    IF l_error = abap_false.
      "CALL METHOD cl_grid->refresh_table_display( is_stable = wa_stable ).
      MESSAGE |Envio de informações ao sistema TraceCotton Finalizada. Verifique o Log!| TYPE 'S'. "BUG - 65565
    ELSE.
      MESSAGE |Erro envio Trace Cotton. Ação de Estorno foi Cancelada. Consulte Log| TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

    PERFORM: seleciona_dados.

    CALL METHOD grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.

  METHOD adm_botoes.

    FREE fcode.

    SELECT COUNT(*)
      FROM setleaf
      WHERE setname EQ 'ZSDT0118_01'
        AND valfrom EQ sy-uname.

    IF sy-subrc IS NOT INITIAL.
      APPEND VALUE #( ucomm = 'ENCERRAR' ) TO fcode.
    ENDIF.

*-CS2021000532-#84648-09.08.2022-JT-inicio
    IF l_copia = abap_true.
      state = 'E'.
    ENDIF.
*-CS2021000532-#84648-09.08.2022-JT-fim

    CASE state.
      WHEN 'E'. "// Editar Item
        APPEND VALUE #( ucomm = 'EDITAR' ) TO fcode.
        APPEND VALUE #( ucomm = 'ENCERRAR' ) TO fcode.
        APPEND VALUE #( ucomm = 'DELETAR' ) TO fcode.
      WHEN 'N'. "// Novo Item
        APPEND VALUE #( ucomm = 'EDITAR' ) TO fcode.
        APPEND VALUE #( ucomm = 'ENCERRAR' ) TO fcode.
        APPEND VALUE #( ucomm = 'DELETAR' ) TO fcode.
    ENDCASE.

    IF w_contrato IS INITIAL.
      APPEND VALUE #( ucomm = 'EDITAR' ) TO fcode.
      APPEND VALUE #( ucomm = 'ENCERRAR' ) TO fcode.
    ENDIF.

    IF w_contrato-sts_con EQ 'E'.
      APPEND VALUE #( ucomm = 'EDITAR' ) TO fcode.
      APPEND VALUE #( ucomm = 'DELETAR' ) TO fcode.
    ENDIF.

    acao = COND #( WHEN state EQ 'N' OR state EQ 'E' THEN 'X' ELSE '' ).

  ENDMETHOD.

  METHOD verifica_erros.

    FREE t_msg.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_contrato-id_contrato_referencia
      IMPORTING
        output = w_contrato-id_contrato_referencia.

    IF w_contrato-visao = 'E' AND w_contrato-intercompany = 'X' AND w_contrato-id_contrato_referencia IS INITIAL .
      APPEND VALUE #(
                      field = 'W_CONTRATO-ID_CONTRATO_REFERENCIA'
                      msg   = |O campo Id Contrato Referência é Obrigatorio!|
                    ) TO t_msg.
    ENDIF.

    IF w_contrato-id_contrato_referencia <> v_id_contr_ant.
      IF w_contrato-id_contrato_referencia IS NOT INITIAL .
        SELECT *
          INTO @DATA(w_0143)
          FROM zsdt0143
            UP TO 1 ROWS
         WHERE id_contrato  = @w_contrato-id_contrato_referencia
           AND visao        = 'P'
           AND intercompany = 'X'.
        ENDSELECT.

        IF sy-subrc = 0.
          w_contrato-safra     = w_0143-safra.
          w_contrato-contrato  = w_0143-contrato.
          w_contrato-dt_venda  = w_0143-dt_venda.
          w_contrato-quatidade = w_0143-quatidade.

          CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
            EXPORTING
              input  = w_0143-tipo_padrao
            IMPORTING
              output = vmatnr18. "w_0143-tipo_padrao.

          w_0143-tipo_padrao = vmatnr18.

*        SELECT SINGLE matnr
*          INTO @DATA(lva_matnr)
*          FROM  mara
*          WHERE bismt = @w_0143-tipo_padrao.

          SELECT *
            INTO @DATA(w_cabn)
            FROM cabn
              UP TO 1 ROWS
            WHERE atnam = 'COD_ALGODAO_SIMILAR'.
          ENDSELECT.
          IF sy-subrc <> 0.
            CLEAR w_cabn.
          ENDIF.

          SELECT *
            INTO @DATA(w_ausp)
            FROM ausp
              UP TO 1 ROWS
            WHERE atinn = @w_cabn-atinn
              AND klart = '001'
              AND atwrt = @w_0143-tipo_padrao.
          ENDSELECT.

          IF sy-subrc = 0 AND w_0143-tipo_padrao IS NOT INITIAL.
            w_contrato-tipo_padrao = w_ausp-objek. "lva_matnr.
          ELSE.
            CLEAR: w_contrato-tipo_padrao.
          ENDIF.
        ELSE.
          CLEAR: w_contrato-safra, w_contrato-contrato,
                 w_contrato-dt_venda,  w_contrato-quatidade,
                 w_contrato-tipo_padrao.
          APPEND VALUE #(
                          field = 'W_CONTRATO-ID_CONTRATO_REFERENCIA'
                          msg   = |Contrato Id Referência não pode ser utilizado!|
                        ) TO t_msg.
        ENDIF.
      ENDIF.
    ENDIF.

    IF w_contrato-id_contrato_referencia IS NOT INITIAL .
      SELECT *
        INTO @DATA(w_0143x)
        FROM zsdt0143
          UP TO 1 ROWS
       WHERE id_contrato            <> @w_contrato-id_contrato
         AND id_contrato_referencia  = @w_contrato-id_contrato_referencia
         AND cancelado               = @abap_off.
      ENDSELECT.

      IF sy-subrc = 0.
        APPEND VALUE #(
                        field = 'W_CONTRATO-ID_CONTRATO_REFERENCIA'
                        msg   = |Contrato Id Referência ja está sendo utilizado por outro Contrato!|
                      ) TO t_msg.
      ENDIF.

*-CS2023000189-04.04.2023-#108693-JT-inicio
      IF w_contrato-empresa = '0015'.
        SELECT id_contrato, empresa
          INTO @DATA(w_0143y)
          FROM zsdt0143
            UP TO 1 ROWS
         WHERE id_contrato = @w_contrato-id_contrato_referencia
           AND cancelado   = @abap_off.
        ENDSELECT.

        IF sy-subrc = 0 AND w_contrato-empresa <> w_0143y-empresa.
          APPEND VALUE #(
                          field = 'W_CONTRATO-ID_CONTRATO_REFERENCIA'
                          msg   = |Somente pode ser utilizado Contrato da mesma Empresa!|
                        ) TO t_msg.
        ENDIF.
      ENDIF.
*-CS2023000189-04.04.2023-#108693-JT-fim

    ENDIF.

    IF w_contrato-safra IS INITIAL.
      APPEND VALUE #(
                      field = 'W_CONTRATO-SAFRA'
                      msg   = |O campo Safra é Obrigatorio!|
                    ) TO t_msg.
    ENDIF.

    IF w_contrato-contrato IS INITIAL.
      APPEND VALUE #(
                      field = 'W_CONTRATO-CONTRATO'
                      msg   = |O campo Contrato é Obrigatorio!|
                    ) TO t_msg.
    ENDIF.

    IF w_contrato-empresa IS INITIAL.
      APPEND VALUE #(
                      field = 'W_CONTRATO-EMPRESA'
                      msg   = |O campo Empresa é Obrigatorio!|
                    ) TO t_msg.
    ENDIF.

    "">>> BUG SOLTO 104268 PA
    IF w_contrato-porto IS INITIAL.
      APPEND VALUE #(
                      field = 'W_CONTRATO-PORTO'
                      msg   = |O campo Porto é Obrigatorio!|
                    ) TO t_msg.
    ENDIF.

    IF w_contrato-dt_venda IS INITIAL.
      APPEND VALUE #(
                      field = 'W_CONTRATO-DT_VENDA'
                      msg   = |O campo Data de Venda é Obrigatorio!|
                    ) TO t_msg.
    ENDIF.


    IF w_contrato-de_embarque IS INITIAL.
      APPEND VALUE #(
                      field = 'W_CONTRATO-DE_EMBARQUE'
                      msg   = |O campo Embarque é Obrigatorio!|
                    ) TO t_msg.
    ENDIF.

    ""<<< BUG SOLTO 104268 PA
    IF w_contrato-tipo_padrao IS NOT INITIAL.
      "CS2023000851 Algodão - PT1 - Atualização da transação ZSDT0118 - Formato já executado na Amaggi e mais detalhado --128361 - BG
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = w_contrato-tipo_padrao
        IMPORTING
          output       = vmatnr18
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.


      SELECT SINGLE matnr
        FROM mara
        INTO @DATA(l_matnr)
        WHERE matnr EQ @vmatnr18.
      IF sy-subrc <> 0.
        APPEND VALUE #(
                        field = 'W_CONTRATO-TIPO_PADRAO'
                        msg   = |Tipo Padrão não localizado!|
                      ) TO t_msg.
      ENDIF.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
        EXPORTING
          input        = w_contrato-tipo_padrao
        IMPORTING
          output       = w_contrato-tipo_padrao
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.
    ENDIF.

    IF l_envia_trace IS NOT INITIAL.
      "CS2023000851 Algodão - PT1 - Atualização da transação ZSDT0118 - Formato já executado na Amaggi e mais detalhado --128361 - BG

      IF w_contrato-visao = 'E' AND w_contrato-intercompany = 'X' AND w_contrato-id_contrato_referencia IS INITIAL .
        APPEND VALUE #(
                      field = 'W_CONTRATO-ID_CONTRATO_REFERENCIA'
                      msg   = |O campo ID Contrato Referência é Obrigatorio!|
                    ) TO t_msg.
      ENDIF.

      CONDENSE w_contrato-empresa NO-GAPS.

      IF w_contrato-empresa IS INITIAL OR w_contrato-contrato IS INITIAL.
        APPEND VALUE #(
                          field = 'W_CONTRATO-EMPRESA'
                          msg   = |O campo Empresa é Obrigatorio!|
                        ) TO t_msg.
      ELSEIF wa_novo-empresa EQ '*'.
        APPEND VALUE #(
                      field = 'W_CONTRATO-EMPRESA'
                      msg   = |Entre com uma Empresa Valida!|
                    ) TO t_msg.
      ENDIF.
    ENDIF.

    IF w_contrato-cliente_final IS NOT INITIAL.
      SELECT COUNT(*) INTO @DATA(lv_count)
        FROM kna1
        WHERE kunnr EQ @w_contrato-cliente_final.
      IF sy-subrc NE 0.
        APPEND VALUE #(
                field = 'W_CONTRATO-CLIENTE_FINAL'
                msg   = |Cliente final não existe!|
              ) TO t_msg.
      ENDIF.
    ENDIF.

    i_show = COND #( WHEN sy-ucomm EQ 'MSG' THEN COND #( WHEN t_msg IS INITIAL THEN abap_false ELSE abap_true ) ELSE abap_false ).

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen    = '0510'
        i_show      = i_show
        i_repid     = sy-repid
        i_popup     = 0
        i_set_field = 'X_FIELD'
        i_set_cell  = 'WG_CELL'
        i_set_obj   = 'WG_OBJ'
      IMPORTING
        e_messagem  = w_msg
      TABLES
        it_msgs     = t_msg.

*    CLEAR I_SHOW.
    v_id_contr_ant = w_contrato-id_contrato_referencia.

  ENDMETHOD.

ENDCLASS.
CLASS dynpro_utilities DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS value_help.
    CLASS-METHODS value_help_01.
ENDCLASS.
CLASS dynpro_utilities IMPLEMENTATION.
  METHOD value_help.
    DATA: BEGIN OF s_data,
            domvalue_l TYPE domvalue_l,
            ddtext     TYPE val_text,
          END OF s_data,
          t_data LIKE TABLE OF s_data.
    DATA: t_dd07v TYPE STANDARD TABLE OF dd07v,
          s_dd07v TYPE dd07v.

    REFRESH: t_data.

* Get the domain values
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'ZVISAOCONT'   " Give your domain here
      TABLES
        values_tab      = t_dd07v
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

* Prepare the data.
    LOOP AT t_dd07v INTO s_dd07v.
      MOVE-CORRESPONDING s_dd07v TO s_data.
      APPEND s_data TO t_data.
    ENDLOOP.

* F4
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        pvalkey          = ' '
        retfield         = 'DOMVALUE_L'
        dynpprog         = sy-repid
        dynpnr           = sy-dynnr
        dynprofield      = 'WA_NOVO-VISAO'
        callback_program = sy-repid
        value_org        = 'S'
      TABLES
        value_tab        = t_data
      EXCEPTIONS
        parameter_error  = 1
        no_values_found  = 2
        OTHERS           = 3.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      "BREAK-POINT.
    ENDIF.
  ENDMETHOD.
  METHOD value_help_01.
    DATA: BEGIN OF s_data,
            domvalue_l TYPE domvalue_l,
            ddtext     TYPE val_text,
          END OF s_data,
          t_data LIKE TABLE OF s_data.
    DATA: t_dd07v TYPE STANDARD TABLE OF dd07v,
          s_dd07v TYPE dd07v.

    REFRESH: t_data.

* Get the domain values
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'ZVISAOCONT'   " Give your domain here
      TABLES
        values_tab      = t_dd07v
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

* Prepare the data.
    LOOP AT t_dd07v INTO s_dd07v.
      MOVE-CORRESPONDING s_dd07v TO s_data.
      APPEND s_data TO t_data.
    ENDLOOP.

* F4
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        pvalkey          = ' '
        retfield         = 'DOMVALUE_L'
        dynpprog         = sy-repid
        dynpnr           = sy-dynnr
        dynprofield      = 'W_CONTRATO-VISAO'
        callback_program = sy-repid
        value_org        = 'S'
      TABLES
        value_tab        = t_data
      EXCEPTIONS
        parameter_error  = 1
        no_values_found  = 2
        OTHERS           = 3.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      "BREAK-POINT.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM seleciona_dados.
  TYPES: BEGIN OF ty_0045,
           objek     TYPE zsdt0066-nro_sol_ov,
           instrucao TYPE zsdt0066-instrucao,
         END OF ty_0045.
  DATA it_0045_aux TYPE TABLE OF ty_0045.
  DATA: vl_pedido    TYPE char255,
        lva_contrato TYPE zsdt0045-contrato,
        vl_qtd_total TYPE zsdt0143-qdt_embarcada.

  DATA: lr_contrato TYPE RANGE OF zsdt0045-contrato,
        lr_embarque TYPE RANGE OF erdat.

  SELECT *
    FROM zsdt0143
    INTO TABLE @DATA(t_table)
   WHERE safra IN @p_safra
     AND empresa IN @p_empre
     AND cliente IN @p_clien
     AND contrato IN @p_contr
     AND cancelado EQ @abap_false.

  CHECK t_table[] IS NOT INITIAL.
  "CS2023000851 Algodão - PT1 - Atualização da transação ZSDT0118 - Formato já executado na Amaggi e mais detalhado - #128361 - BG

  DATA(t_table_aux) =  t_table.

  DELETE ADJACENT DUPLICATES FROM t_table_aux COMPARING contrato empresa.

  LOOP AT t_table_aux INTO DATA(lwa_table_aux).
    CLEAR: wa_qtdembarcada.
    IF lwa_table_aux-contrato IS NOT INITIAL.

      SELECT nro_sol_ov
      FROM zsdt0051
      INTO TABLE @DATA(t_nr_sol)
      WHERE bstkd  = @lwa_table_aux-contrato
        AND vkorg  = @lwa_table_aux-empresa.

      IF sy-subrc IS INITIAL.
        SELECT vbeln FROM zsdt0053
          INTO TABLE @DATA(t_vbeln)
          FOR ALL ENTRIES IN @t_nr_sol
          WHERE nro_sol_ov = @t_nr_sol-nro_sol_ov
            AND vbeln NE @space.

        IF sy-subrc EQ 0.
          wa_qtdembarcada-contrato = lwa_table_aux-contrato.
          wa_qtdembarcada-empresa = lwa_table_aux-empresa.

          LOOP AT t_vbeln INTO DATA(s_vbeln).
            SELECT vbeln, vbelv, rfmng, vbtyp_n FROM vbfa
              INTO TABLE @DATA(lt_rfmng)
              WHERE vbelv = @s_vbeln-vbeln
               AND vbtyp_n IN ('M','N')
               AND vbtyp_v IN ('C').
            "pq
            IF sy-subrc EQ 0.
              LOOP AT lt_rfmng INTO DATA(ls_rfmng).
                IF ls_rfmng-vbtyp_n EQ 'N'.
                  wa_qtdembarcada-qtd_embarcada = wa_qtdembarcada-qtd_embarcada - ls_rfmng-rfmng.
                ELSE.
                  wa_qtdembarcada-qtd_embarcada = wa_qtdembarcada-qtd_embarcada + ls_rfmng-rfmng.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

      APPEND wa_qtdembarcada TO tg_qtdembarcada.

    ENDIF.
  ENDLOOP.

**  LOOP AT t_table INTO DATA(lwa_table).
**    MOVE lwa_table-contrato TO lva_contrato.
**
**    SELECT *
**      FROM zsdt0045
**     APPENDING TABLE @DATA(it_0045)
**      WHERE contrato = @lva_contrato.
**
**  ENDLOOP.

*  LOOP AT t_table ASSIGNING  FIELD-SYMBOL(<fs_table>).
*    CLEAR lr_embarque.
*    lr_embarque = VALUE #( sign = 'I' option = 'BT' ( low = <fs_table>-de_embarque high = <fs_table>-ate_embarque ) ).
*    IF <fs_table>-id_contrato IS NOT INITIAL.
*      SELECT nro_sol_ov
*      FROM zsdt0051
*      INTO TABLE @DATA(t_nr_sol)
*      WHERE bstkd  = @<fs_table>-contrato
*        AND vkorg  = @<fs_table>-empresa.
*
*      IF sy-subrc IS INITIAL.
*        SELECT vbeln FROM zsdt0053
*          INTO TABLE @DATA(t_vbeln)
*          FOR ALL ENTRIES IN @t_nr_sol
*          WHERE nro_sol_ov = @t_nr_sol-nro_sol_ov
*            AND vbeln NE @space.
*
*        IF sy-subrc EQ 0.
*          LOOP AT t_vbeln INTO DATA(s_vbeln).
*            SELECT vbeln, vbelv, rfmng, vbtyp_n FROM vbfa
*              INTO TABLE @DATA(lt_RFMNG)
*              WHERE vbelv = @s_vbeln-vbeln
*               AND vbtyp_n IN ('M','N')
*               AND vbtyp_v IN ('C')
*               AND erdat   IN @lr_embarque.
*
*            IF sy-subrc EQ 0.
*              LOOP AT lt_RFMNG INTO DATA(ls_rfmng).
*                IF ls_rfmng-vbtyp_n EQ 'N'.
*                  <fs_table>-qdt_embarcada = <fs_table>-qdt_embarcada - ls_rfmng-rfmng.
*                ELSE.
*                  <fs_table>-qdt_embarcada = <fs_table>-qdt_embarcada + ls_rfmng-rfmng.
*                ENDIF.
*              ENDLOOP.
*            ENDIF.
*          ENDLOOP.
*        ENDIF.
*
**        SELECT SUM( zmeng )
**        FROM zsdt0053
**        INTO @DATA(v_zmeng)
**        WHERE nro_sol_ov = @v_nr_sol.
**
**        IF sy-subrc IS INITIAL.
**          <fs_table>-qdt_embarcada = v_zmeng.
**        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

  lr_contrato = VALUE #( FOR ls_table IN t_table
                         ( sign = 'I'
                           option = 'EQ'
                           low = ls_table-contrato ) ).

**  LOOP AT t_table INTO DATA(lwa_table).
**    MOVE lwa_table-contrato TO lva_contrato.
**
**    SELECT *
**      FROM zsdt0045
**     APPENDING TABLE @DATA(it_0045)
**      WHERE contrato = @lva_contrato.
**
**  ENDLOOP.

  SELECT *
      FROM zsdt0045
     APPENDING TABLE @DATA(it_0045)
      WHERE contrato IN @lr_contrato.


  IF it_0045 IS NOT INITIAL.
*    DATA: lva_objek TYPE  zsdt0066-nro_sol_ov,
*          lr_objek  TYPE RANGE OF zsdt0066-nro_sol_ov.


    it_0045_aux = VALUE #( FOR ls_045 IN it_0045
                      ( objek = ls_045-objek
                        instrucao = ls_045-instrucao ) ).

*    lr_objek = VALUE #( FOR ls_045 IN it_0045
*                         ( sign = 'I'
*                           option = 'EQ'
*                           low = ls_045-objek ) ).

*    lt_objek = VALUE #( FOR ls_045 IN it_0045
*                         ( nro_sol_ov = ls_045-objek ) ).

**    LOOP AT it_0045 INTO DATA(lwa_0045).
**      MOVE lwa_0045-objek TO lva_objek.
**
**      SELECT *
**      FROM zsdt0066
**    APPENDING TABLE @DATA(it_0066)
**      WHERE instrucao  = @lwa_0045-instrucao
**        AND nro_sol_ov = @lva_objek.
**      CLEAR: lva_objek.
**    ENDLOOP.

    SELECT *
    FROM zsdt0066
    APPENDING TABLE @DATA(it_0066)
          FOR ALL ENTRIES IN @it_0045_aux
    WHERE instrucao  = @it_0045_aux-instrucao
      AND nro_sol_ov = @it_0045_aux-objek.


    "BUG - 79149 - CBRAND - Inicio
*    SELECT *
*      FROM zsdt0066
*      INTO TABLE @DATA(it_0066)
*      FOR ALL ENTRIES IN @it_0045
*      WHERE instrucao  = @it_0045-instrucao
*        AND nro_sol_ov = @it_0045-objek.
    "BUG - 79149 - CBRAND - Fim

    IF it_0066 IS NOT INITIAL.
      SELECT *
        FROM zsdt0001
        INTO TABLE @DATA(it_0001)
        FOR ALL ENTRIES IN @it_0066
        WHERE tp_movimento = 'S'
        AND vbeln  = @it_0066-vbeln
        AND nr_safra = @it_0066-charg "BUG - 79149 - CBRAND
        AND branch = @it_0066-werks.


      IF it_0001 IS NOT INITIAL.
        SELECT *
        FROM zsdt0001nt
        INTO TABLE @DATA(it_zsdt0001nt)
        FOR ALL ENTRIES IN @it_0001
        WHERE id_carga  = @it_0001-id_carga.


        LOOP AT it_0045 INTO DATA(wa_0045).

*-BUG 90598-16.09.2022-JT-inicio
*         READ TABLE it_0066 INTO DATA(wa_0066) WITH KEY instrucao =  wa_0045-instrucao.
          LOOP AT it_0066 INTO DATA(wa_0066) WHERE instrucao =  wa_0045-instrucao.
*-BUG 90598-16.09.2022-JT-fim

            READ TABLE it_0001 INTO DATA(wa_0001) WITH  KEY vbeln  = wa_0066-vbeln.

            CLEAR: vl_pedido.
            LOOP AT it_zsdt0001nt INTO DATA(wa_zsdt0001nt) WHERE id_carga  = wa_0001-id_carga.
              IF vl_pedido IS INITIAL.
                vl_pedido = |{ wa_zsdt0001nt-po_number  ALPHA = OUT }|.
              ELSE.
                vl_pedido = |{ vl_pedido }/{ wa_zsdt0001nt-po_number  ALPHA = OUT }|.
              ENDIF.
            ENDLOOP.

            IF vl_pedido IS NOT INITIAL.   "*-BUG 90598-16.09.2022-JT-inicio
              wa_pedido-contrato   = wa_0045-contrato.
              wa_pedido-pedido     = vl_pedido.
              COLLECT wa_pedido INTO it_pedido.  "*-BUG 90598-16.09.2022-JT-inicio
*             APPEND wa_pedido    TO it_pedido.
            ENDIF.
            CLEAR: wa_pedido.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

  SELECT *
    FROM zsdt0051
    INTO TABLE @DATA(it_0051)
    FOR ALL ENTRIES IN @t_table
    WHERE bstkd = @t_table-contrato.

*  SELECT * FROM MAKT
*    INTO TABLE @DATA(IT_MAKT)
*    FOR ALL ENTRIES IN @T_TABLE
*    WHERE MATNR EQ @T_TABLE-TIPO_PADRAO.

  SELECT * FROM mara
    INTO TABLE @DATA(it_mara)
   FOR ALL ENTRIES IN @t_table
   WHERE matnr EQ @t_table-tipo_padrao.

  SELECT * FROM lfa1
    INTO TABLE @DATA(it_lfa1)
    FOR ALL ENTRIES IN @t_table
    WHERE lifnr EQ @t_table-corretor.

  SELECT * FROM kna1
    INTO TABLE @DATA(it_kna1)
    FOR ALL ENTRIES IN @t_table
    WHERE kunnr EQ @t_table-cliente.

*-CS2023000189-04.04.2023-#108693-JT-inicio
  IF it_kna1[] IS NOT INITIAL.
    SELECT *
      FROM adrc
      INTO TABLE @DATA(it_adrc)
       FOR ALL ENTRIES IN @it_kna1
     WHERE addrnumber EQ @it_kna1-adrnr
       AND date_to    >= @sy-datum.
  ELSE.
    FREE: it_adrc.
  ENDIF.
*-CS2023000189-04.04.2023-#108693-JT-fim

  SELECT * FROM tvgrt
    INTO TABLE @DATA(it_tvgrt)
    FOR ALL ENTRIES IN @t_table
    WHERE vkgrp EQ @t_table-vendedor
      AND spras EQ @sy-langu.

  SELECT * FROM t001
    INTO TABLE @DATA(it_t001)
    FOR ALL ENTRIES IN @t_table
    WHERE bukrs EQ @t_table-empresa.


  SELECT * FROM kna1
    APPENDING TABLE @it_kna1
    FOR ALL ENTRIES IN @t_table
    WHERE kunnr EQ @t_table-cliente_final.

  SELECT * FROM zsdt0057
    INTO TABLE @DATA(it_zsdt0057)
      FOR ALL ENTRIES IN @t_table
    WHERE tp_venda EQ @t_table-tp_venda.

  PERFORM: saida TABLES
                 t_table
                 it_pedido
                 "IT_MAKT
                 it_mara
                 it_lfa1
                 it_kna1
                 it_adrc   "*-CS2023000189-04.04.2023-#108693-JT
                 it_tvgrt
                 it_t001
                 it_0051
                 it_zsdt0057
                 tg_qtdembarcada.

  consolidar=>selecao_projecao( ).

  SORT it_saida BY id_contrato.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  SAIDA
*&---------------------------------------------------------------------*
FORM saida TABLES t_table TYPE ty_zsdt0143
                  it_pedido LIKE it_pedido
                  "IT_MAKT STRUCTURE MAKT
                  it_mara  STRUCTURE mara
                  it_lfa1  STRUCTURE lfa1
                  it_kna1  STRUCTURE kna1
                  it_adrc  STRUCTURE adrc    "*-CS2023000189-04.04.2023-#108693-JT
                  it_tvgrt STRUCTURE tvgrt
                  it_t001  STRUCTURE t001
                  it_0051  STRUCTURE zsdt0051
                  it_zsdt0057 STRUCTURE zsdt0057
                  it_qtdembarcada STRUCTURE tg_qtdembarcada.

  DATA: vl_nro_sol_ov TYPE char255.
  DATA: vl_pedido TYPE char255.
  DATA: lva_visao_desc(15) TYPE c.

  DATA: lv_index_line TYPE sy-tabix.
  FIELD-SYMBOLS: <fs_table> TYPE ty_qtd_embarcada.
  DATA: vl_qtd_embarcada TYPE zsdt0143-qdt_embarcada.

  DATA: it_dd07v TYPE TABLE OF dd07v,
        wa_dd07v TYPE dd07v.


  FREE: it_saida, wa_relatorio, it_relatorio.

  DATA: total_preco TYPE zsded053,
        total_qtd   TYPE dzmeng.


  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'ZVISAOCONT'
    TABLES
      values_tab = it_dd07v.

  SORT t_table BY id_contrato contrato de_embarque.
  SORT it_qtdembarcada BY contrato.

  LOOP AT t_table INTO DATA(w_table).

    CLEAR: vl_nro_sol_ov, vl_qtd_embarcada.
    LOOP AT it_0051 INTO DATA(wa_0051) WHERE bstkd EQ w_table-contrato
                                         AND vkorg EQ w_table-empresa. " BUG #83141
      IF vl_nro_sol_ov IS INITIAL.
        vl_nro_sol_ov = |{ wa_0051-nro_sol_ov ALPHA = OUT }|.
      ELSE.
        vl_nro_sol_ov = |{ vl_nro_sol_ov }/{ wa_0051-nro_sol_ov  ALPHA = OUT }|.
      ENDIF.
    ENDLOOP.

*** US 77507 - CBRAND - Inicio
    READ TABLE it_pedido INTO wa_pedido WITH KEY contrato = w_table-contrato.
    IF sy-subrc = 0.
      vl_pedido = wa_pedido-pedido.
    ENDIF.
*** US 77507 - CBRAND - Fim

    TRY.
        DATA(_descr) = it_mara[ matnr = w_table-tipo_padrao ]-normt.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    "DATA(LINE) = STRLEN( _DESCR ).
    "LINE = LINE - 4.
    "IF LINE > 0.
    "_DESCR = _DESCR+LINE.
    "ENDIF.

*-CS2023000189-04.04.2023-#108693-JT-inicio
*   TRY.
*       DATA(d_cliente) = it_kna1[ kunnr = w_table-cliente ]-name1.
*     CATCH cx_sy_itab_line_not_found.
*   ENDTRY.
    TRY.
        DATA(l_adrnr)   = it_kna1[ kunnr = w_table-cliente ]-adrnr.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    TRY.
        DATA(d_cliente) = it_adrc[ addrnumber = l_adrnr ]-name1 &&
                          it_adrc[ addrnumber = l_adrnr ]-name2.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
*-CS2023000189-04.04.2023-#108693-JT-fim

    TRY.
        DATA(d_corretor) = it_lfa1[ lifnr = w_table-corretor ]-name1.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    TRY.
        DATA(d_vendedor) = it_tvgrt[ vkgrp = w_table-vendedor ]-bezei.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    TRY.
        DATA(d_empresa) = it_t001[ bukrs = w_table-empresa ]-butxt.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

*** BUG - 78952 - Inicio - CBRAND
    CLEAR: lva_visao_desc, wa_dd07v.
    READ TABLE it_dd07v INTO wa_dd07v WITH KEY domvalue_l = w_table-visao.

    lva_visao_desc = wa_dd07v-ddtext.
*** BUG - 78952 - Fim - CBRAND

    DATA(desc_cliente_final) = VALUE #( it_kna1[ kunnr = w_table-cliente_final ]-name1 OPTIONAL ).
    DATA(desc_tipo_venda)    = VALUE #( it_zsdt0057[ tp_venda = w_table-tp_venda ]-bezei OPTIONAL ).

*** Stefanini - IR244887 - 08/07/2025 - FINC - Início de Alteração
    DATA(lv_total_lines) = REDUCE i( INIT count = 0
                              FOR ls_totline IN t_table
                            WHERE ( contrato = w_table-contrato AND empresa = w_table-empresa )
                             NEXT count = count + 1 ).

    IF <fs_table> IS ASSIGNED.
      IF <fs_table>-contrato = w_table-contrato AND <fs_table>-empresa  = w_table-empresa.
        ADD 1 TO lv_index_line.
      ELSE.
        lv_index_line = 1.
      ENDIF.
    ELSE.
      lv_index_line = 1.
    ENDIF.
*** Stefanini - IR244887 - 08/07/2025 - FINC - Fim de Alteração

    "pq
    LOOP AT it_qtdembarcada ASSIGNING  <fs_table> WHERE contrato = w_table-contrato
                                                    AND empresa  = w_table-empresa.
*** Stefanini - IR244887 - 08/07/2025 - FINC - Início de Alteração
      IF <fs_table>-qtd_embarcada <= w_table-quatidade.
        vl_qtd_embarcada = <fs_table>-qtd_embarcada.
        <fs_table>-qtd_embarcada = 0.

      ELSE.

        IF lv_index_line = lv_total_lines.
          vl_qtd_embarcada = <fs_table>-qtd_embarcada.
          <fs_table>-qtd_embarcada = 0.
        ELSE.
          vl_qtd_embarcada = w_table-quatidade.
          <fs_table>-qtd_embarcada = <fs_table>-qtd_embarcada - w_table-quatidade.
        ENDIF.
      ENDIF.
*** Stefanini - IR244887 - 08/07/2025 - FINC - Fim de Alteração
    ENDLOOP.

    APPEND VALUE #( excluir          = icon_delete
                    edit             = icon_change
                    s_sol            = SWITCH #( w_table-sts_sol
                                                  WHEN 'A' THEN icon_initial
                                                  WHEN ' ' THEN icon_initial
                                                  WHEN 'L' THEN icon_release
                                                  WHEN 'D' THEN icon_delete
                                                  WHEN 'R' THEN icon_defect
                                                  WHEN 'P' THEN icon_led_yellow
                                                )
                    s_con                  = COND #( WHEN w_table-sts_con IS INITIAL THEN icon_initial ELSE icon_complete )
                    id_contrato            = w_table-id_contrato
                    contrato               = w_table-contrato
                    contrato_cliente       = w_table-contrato_cliente
                    safra                  = w_table-safra
                    tp_venda               = w_table-tp_venda
                    dt_venda               = w_table-dt_venda
                    cliente                = w_table-cliente
                    empresa                = w_table-empresa
                    vendedor               = w_table-vendedor
                    corretor               = w_table-corretor
                    comissao               = w_table-comissao
                    tipo_padrao            = w_table-tipo_padrao
                    porto                  = w_table-porto
                    quatidade              = w_table-quatidade
                    tolerancia             = w_table-tolerancia
                    de_embarque            = w_table-de_embarque
                    ate_embarque           = w_table-ate_embarque
                    preco                  = w_table-preco
                    preco_tons             = w_table-preco_tons
                    pctgem_ant             = w_table-pctgem_ant
*                    qdt_embarcada          = w_table-qdt_embarcada
                    qdt_embarcada          = vl_qtd_embarcada
                    sts_sol                = w_table-sts_sol
                    sts_con                = w_table-sts_con
                    a_usnam                = w_table-a_usnam
                    a_data_atual           = w_table-a_data_atual
                    a_hora_atual           = w_table-a_hora_atual
                    cancelado              = w_table-cancelado
                    c_usnam                = w_table-c_usnam
                    c_data_atual           = w_table-c_data_atual
                    c_hora_atual           = w_table-c_hora_atual
                    usnam                  = w_table-usnam
                    data_atual             = w_table-data_atual
                    hora_atual             = w_table-hora_atual
                    t_cotton               = w_table-t_cotton
                    t_usuario              = w_table-t_usuario
                    t_data                 = w_table-t_data
                    t_hora                 = w_table-t_hora
                    t_log                  = icon_protocol
                    desc_empresa           = d_empresa
                    desc_cliente           = d_cliente
                    desc_vendedor          = d_vendedor
                    desc_corretor          = d_corretor
                    desc_tipo_padrao       = _descr
                    nro_sol_ov             = vl_nro_sol_ov
                    pedido                 = vl_pedido "US 77507 - CBRAND
                    visao                  = w_table-visao "US 77507 - CBRAND
                    visao_desc             = lva_visao_desc
                    intercompany           = w_table-intercompany "US 77507 - CBRAND
                    acts                   = w_table-acts     "*-CS2023000189-04.04.2023-#108693-JT
                    id_contrato_referencia = w_table-id_contrato_referencia "US 77507 - CBRAND
                    waerk                  = w_table-waerk
                    cliente_final          = w_table-cliente_final
                    desc_cliente_final     = desc_cliente_final
                    desc_tp_venda          = desc_tipo_venda
                  ) TO it_saida.

    ADD w_table-preco TO total_preco.
    ADD w_table-quatidade TO total_qtd.

    wa_relatorio = VALUE #(
                    safra        = w_table-safra
                    desc_cliente = d_cliente
                    contrato     = w_table-contrato
                    tipo         = consolidar=>get_normt( w_table-tipo_padrao )
*                    PRECO        = W_TABLE-PRECO
                    jan          = COND #( WHEN w_table-de_embarque+4(2) EQ '01' THEN w_table-quatidade / 1000 ELSE 0 )
                    fev          = COND #( WHEN w_table-de_embarque+4(2) EQ '02' THEN w_table-quatidade / 1000 ELSE 0 )
                    mar          = COND #( WHEN w_table-de_embarque+4(2) EQ '03' THEN w_table-quatidade / 1000 ELSE 0 )
                    abr          = COND #( WHEN w_table-de_embarque+4(2) EQ '04' THEN w_table-quatidade / 1000 ELSE 0 )
                    mai          = COND #( WHEN w_table-de_embarque+4(2) EQ '05' THEN w_table-quatidade / 1000 ELSE 0 )
                    jun          = COND #( WHEN w_table-de_embarque+4(2) EQ '06' THEN w_table-quatidade / 1000 ELSE 0 )
                    jul          = COND #( WHEN w_table-de_embarque+4(2) EQ '07' THEN w_table-quatidade / 1000 ELSE 0 )
                    ago          = COND #( WHEN w_table-de_embarque+4(2) EQ '08' THEN w_table-quatidade / 1000 ELSE 0 )
                    set          = COND #( WHEN w_table-de_embarque+4(2) EQ '09' THEN w_table-quatidade / 1000 ELSE 0 )
                    out          = COND #( WHEN w_table-de_embarque+4(2) EQ '10' THEN w_table-quatidade / 1000 ELSE 0 )
                    nov          = COND #( WHEN w_table-de_embarque+4(2) EQ '11' THEN w_table-quatidade / 1000 ELSE 0 )
                    dev          = COND #( WHEN w_table-de_embarque+4(2) EQ '12' THEN w_table-quatidade / 1000 ELSE 0 )
                    qtd_tons     = w_table-quatidade / 1000
      ).

    COLLECT wa_relatorio INTO it_relatorio.

    CLEAR: d_empresa, d_cliente, d_vendedor, d_corretor, _descr.

  ENDLOOP.

  LOOP AT it_relatorio ASSIGNING FIELD-SYMBOL(<f_rel>).

    <f_rel>-preco = consolidar=>get_preco( <f_rel>-contrato ).

    <f_rel>-preco_lb = <f_rel>-preco * vlr * 15.
    <f_rel>-total    = <f_rel>-qtd_tons * <f_rel>-preco * vlr * 1000.

    ADD <f_rel>-jan TO wa_mes-jan.
    ADD <f_rel>-fev TO wa_mes-fev.
    ADD <f_rel>-mar TO wa_mes-mar.
    ADD <f_rel>-abr TO wa_mes-abr.
    ADD <f_rel>-mai TO wa_mes-mai.
    ADD <f_rel>-jun TO wa_mes-jun.
    ADD <f_rel>-jul TO wa_mes-jul.
    ADD <f_rel>-ago TO wa_mes-ago.
    ADD <f_rel>-set TO wa_mes-set.
    ADD <f_rel>-out TO wa_mes-out.
    ADD <f_rel>-nov TO wa_mes-nov.
    ADD <f_rel>-dev TO wa_mes-dev.

  ENDLOOP.

  consolidar=>consolida_dados( ).

  wa_novo-empresa = p_empre-low.
  wa_novo-safra = p_safra-low. "BUG - 79025 - CSB

  SELECT SINGLE butxt
    FROM t001
      INTO wa_desc-desc_empresa
      WHERE bukrs EQ wa_novo-empresa.

*  CASE ABAP_TRUE.
*    WHEN R_RELAT.
*      IF CONSOLIDAR=>QTD_PROJECAO( ) IS INITIAL.
*        MESSAGE 'Não foi encontrada projeção cadastrada!' TYPE 'S'.
*        EXIT.
*      ENDIF.
*      _PROJECAO = VALUE #(
*                           QTD         = CONV #( CONSOLIDAR=>QTD_PROJECAO( ) / 1000 )
*                           PRECO       = CONV #( TOTAL_PRECO / LINES( IT_RELATORIO ) * VLR * 1000 )
*                           PORCENTAGEM = CONV #( ( TOTAL_QTD / CONSOLIDAR=>QTD_PROJECAO( ) ) * 100 )
*                        ).
*
*      WRITE _PROJECAO-QTD TO _PROJECAO-QTD_C.
*      WRITE _PROJECAO-PRECO TO _PROJECAO-PRECO_C.
*      _PROJECAO-PORCENTAGEM_C = _PROJECAO-PORCENTAGEM.
*      _PROJECAO-PORCENTAGEM_C = |{ _PROJECAO-PORCENTAGEM_C ALPHA = OUT }|.
*
*      CONDENSE: _PROJECAO-QTD_C,
*                _PROJECAO-PRECO_C,
*                _PROJECAO-PORCENTAGEM_C
*       NO-GAPS.
*
*      _PROJECAO-PRECO_C = |${ _PROJECAO-PRECO_C }|.
*      _PROJECAO-PORCENTAGEM_C = |{ _PROJECAO-PORCENTAGEM_C }%|.
*
*  ENDCASE.

ENDFORM.                    " SAIDA

*&---------------------------------------------------------------------*
*&      Module  pbo  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo OUTPUT.

  IF ( cl_container IS INITIAL ).
    PERFORM: create_object.
  ENDIF.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TB0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
MODULE pai INPUT.

  DATA: l_error TYPE char1.

  CLEAR lv_acao.

  CASE sy-ucomm.
    WHEN: 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN: 'CANC'.
      LEAVE LIST-PROCESSING AND RETURN TO SCREEN 0.
    WHEN: 'EXIT'.
      LEAVE PROGRAM.
    WHEN: 'CREATE'.
      acao = abap_true.
      PERFORM: criar_safra.
*-CS2021000532-#84648-09.08.2022-JT-inicio
    WHEN: 'COPIAR'.
      PERFORM: copiar_safra.
*-CS2021000532-#84648-09.08.2022-JT-fim
    WHEN 'TCOTTON'.
*      CLEAR: it_sel_rows[].
*
*      CALL METHOD cl_grid->get_selected_rows
*        IMPORTING
*          et_index_rows = it_sel_rows.
*
*      IF it_sel_rows[] IS INITIAL.
*        MESSAGE 'Selecione uma linha!' TYPE 'S'.
*        EXIT.
*      ELSE.
*
*        READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
*
*        READ TABLE it_saida INTO wa_saida INDEX wa_sel_rows-index.
*        IF sy-subrc EQ 0.
*
**-CS2023000189-04.04.2023-#108693-JT-inicio
*          CLEAR l_error.
*
**-----------------------------
**-------- cadastra contratos no Trace
**-----------------------------
*          TRY .
*              zcl_trace_cotton=>zif_trace_cotton~get_instance(
*                 )->set_cadastra_contratos( EXPORTING i_id_contrato = wa_saida-id_contrato ).
*
*            CATCH zcx_integracao INTO DATA(ex_integra).
*              l_error = abap_true.
**             l_mesg  = ex_integra->msgv1 && '-' && ex_integra->msgv2 && '-' && ex_integra->msgv3 && '-' && ex_integra->msgv4.
*
*            CATCH zcx_error INTO DATA(ex_error).
*              l_error = abap_true.
**             l_mesg  = ex_error->msgv1   && '-' && ex_error->msgv2   && '-' && ex_error->msgv3   && '-' && ex_error->msgv4.
*          ENDTRY.
*
*          IF l_error = abap_false.
*            CALL METHOD cl_grid->refresh_table_display( is_stable = wa_stable ).
*            MESSAGE |Envio de informações ao sistema TraceCotton Finalizada. Verifique o Log!| TYPE 'S'. "BUG - 65565
*          ELSE.
*            MESSAGE |A tentativa de envio das informação ao sistema TraceCotton Falhou. Verifique o Log| TYPE 'E'. "BUG - 65565
*
*
*
*          ENDIF.
*
**          SELECT SINGLE * FROM  zsdt0143 INTO @DATA(wa_zsdt0143)
**            WHERE id_contrato EQ @wa_saida-id_contrato.
**
**          IF wa_saida-t_cotton IS INITIAL.
**
**            PERFORM f_preenche_dados_api USING wa_saida post.
**
**            PERFORM f_integracao_trace_cotton USING post wa_contratos
**                                                      CHANGING v_return.
**          ELSE.
**            PERFORM f_preenche_dados_api USING wa_saida put.
**
**            PERFORM f_integracao_trace_cotton USING put wa_contratos
**                                                      CHANGING v_return.
**          ENDIF.
**
**          CASE v_return.
**            WHEN 200.
**              wa_zsdt0143-t_cotton  = 'X'.
**              wa_zsdt0143-t_data    = sy-datum.
**              wa_zsdt0143-t_hora    = sy-uzeit.
**              wa_zsdt0143-t_usuario = sy-uname.
**              MODIFY zsdt0143 FROM wa_zsdt0143.
**              COMMIT WORK.
**
**              CLEAR: it_fcat[], it_saida[].
**              PERFORM: seleciona_dados.
**
**              CALL METHOD cl_grid->refresh_table_display( is_stable = wa_stable ).
**              MESSAGE |Envio de informações ao sistema TraceCotton Finalizada. Verifique o Log!| TYPE 'S'. "BUG - 65565
**            WHEN OTHERS.
**              MESSAGE |A tentativa de envio das informação ao sistema TraceCotton Falhou. Verifique o Log| TYPE 'E'. "BUG - 65565
**          ENDCASE.
*          " MESSAGE |Envio de informações ao sistema TraceCotton Finalizada. Verifique o Log!| TYPE 'S'.
*
*        ENDIF.
*-CS2023000189-04.04.2023-#108693-JT-inicio

*      ENDIF.
  ENDCASE.
ENDMODULE.                 " PAI  INPUT
*&---------------------------------------------------------------------*
*&      CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: handle_hotspot_click
        FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.                    "lcv_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcv_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING e_row_id e_column_id es_row_no.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "lcv_event_handler IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS tree_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_tree_event_receiver DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      handle_double_click FOR EVENT node_double_click OF cl_gui_alv_tree
        IMPORTING
          node_key,
      handle_expand_no_children FOR EVENT expand_nc OF cl_gui_alv_tree
        IMPORTING node_key,
      handle_item_double_click FOR EVENT item_double_click OF cl_gui_alv_tree
        IMPORTING fieldname node_key.

*    METHODS handle_left_click_run               " LEFT_CLICK_RUN
*     FOR EVENT left_click_run OF cl_gui_alv_grid.
  PRIVATE SECTION.
ENDCLASS.                    "tree_event_receiver DEFINITION

DATA: tree_event_receiver   TYPE REF TO lcl_tree_event_receiver.
*----------------------------------------------------------------------*
*       CLASS lcl_tree_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_tree_event_receiver IMPLEMENTATION.
* handle double_click
  METHOD handle_double_click.

    IF node_key GT 1.
      LOOP AT <fs_table> INTO <fs_line>.
        CLEAR: w_valor.
        PERFORM get_set_valores USING 'ITEM_KEY'
                                      'G'
                               CHANGING w_valor.
*        CONDENSE WG_VALOR NO-GAPS.

        IF w_valor EQ node_key.
          CLEAR node_key.
        ENDIF.
      ENDLOOP.
      IF node_key IS NOT INITIAL.
        PERFORM preco_frame_alv USING node_key.
      ENDIF.
    ENDIF.


  ENDMETHOD.                    "handle_double_click
* handle double_click
  METHOD handle_expand_no_children.
*    .
  ENDMETHOD.                    "handle_double_click
* handle double_click
  METHOD handle_item_double_click.
    IF node_key GT 1.
      LOOP AT <fs_table> INTO <fs_line>.
        CLEAR: w_valor.
        PERFORM get_set_valores USING 'ITEM_KEY'
                                      'G'
                               CHANGING w_valor.

        IF w_valor EQ node_key.
          CLEAR node_key.
        ENDIF.
      ENDLOOP.
      IF node_key IS NOT INITIAL.
        PERFORM preco_frame_alv USING node_key.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "handle_double_click
ENDCLASS.                    "lcl_tree_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM handle_hotspot_click  USING   i_row_id     TYPE lvc_s_row
                                   i_column_id  TYPE lvc_s_col
                                   is_row_no    TYPE lvc_s_roid.

  DATA: sidinf TYPE RANGE OF zde_id_interface.
  DATA: sidref TYPE RANGE OF zde_id_referencia.
  DATA: sdtreg TYPE RANGE OF zde_dt_registro.


  CASE i_column_id.
    WHEN: 'EDIT'.
      edit = 'X'.
      acao = abap_true.
      PERFORM: editar_safra   USING is_row_no-row_id.
    WHEN: 'EXCLUIR'.
      PERFORM: excluir_safra  USING is_row_no-row_id.
    WHEN: 'T_LOG'.

      READ TABLE it_saida INTO wa_saida INDEX i_row_id-index.
      IF sy-subrc EQ 0.

        sidinf = VALUE #( sign = 'I' option = 'EQ' ( low = '040' ) ).
        sidref = VALUE #( sign = 'I' option = 'EQ' ( low = wa_saida-id_contrato ) ).
        IF wa_saida-t_data IS NOT INITIAL.
          sdtreg =   VALUE #( sign = 'I' option = 'GE' ( low =  wa_saida-t_data ) ). .
        ENDIF.

        "*-CS2023000189-04.04.2023-#108693-JT-JAIME-inicio
        CALL FUNCTION 'ZSD_LOG_ENVIO_TRACE_COTTON'
          EXPORTING
            i_id_contrato  = wa_saida-id_contrato
            i_tipo_integra = 'CO'.

*       SUBMIT zintegracao  WITH sidinf IN sidinf
*                           WITH sidref IN sidref
*                           WITH sdtreg IN sdtreg AND RETURN.
        "*-CS2023000189-04.04.2023-#108693-JT-JAIME-fim
      ENDIF.

    WHEN: 'ACTS'.  "*-CS2023000189-04.04.2023-#108693-JT-JAIME
      READ TABLE it_saida INTO wa_saida INDEX i_row_id-index.
      IF sy-subrc EQ 0.
        PERFORM f_log_modif_acts  USING wa_saida-id_contrato.
      ENDIF.

  ENDCASE.
ENDFORM.                    " HANDLE_HOTSPOT_CLICK

FORM f_bdc_data USING p_program p_dynpro p_start p_fnam p_fval.
  CLEAR wa_dta.
  wa_dta-program   = p_program.
  wa_dta-dynpro    = p_dynpro.
  wa_dta-dynbegin  = p_start.
  wa_dta-fnam      = p_fnam.
  wa_dta-fval      = p_fval.
  APPEND wa_dta TO it_dta.
ENDFORM.

FORM f_call_transaction USING p_trans
                              p_mode
                              p_upd.

  REFRESH: it_msg, it_msgtext.

  CALL TRANSACTION p_trans USING it_dta
        MODE p_mode
        MESSAGES INTO it_msg
        UPDATE p_upd.

  IF it_msg[] IS NOT INITIAL.
    SELECT text
      FROM t100
      INTO TABLE it_msgtext
      FOR ALL ENTRIES IN it_msg
     WHERE arbgb  = it_msg-msgid AND
           msgnr  = it_msg-msgnr AND
           sprsl  = sy-langu.

    LOOP AT it_msgtext.
      TRANSLATE it_msgtext-texto USING '& '.
      CONDENSE it_msgtext-texto.
      MODIFY it_msgtext.
    ENDLOOP.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  EDITAR_SAFRA
*&---------------------------------------------------------------------*
FORM editar_safra  USING p_row_id.

  READ TABLE it_saida INTO wa_saida INDEX p_row_id.
  CLEAR: wa_novo, wa_desc.
  MOVE-CORRESPONDING wa_saida TO wa_novo.
  MOVE-CORRESPONDING wa_saida TO wa_desc.

  v_id_contr_ant = wa_novo-id_contrato_referencia.

  PERFORM f_atualiza_dados.

* PERFORM conversion USING 'IN' CHANGING wa_novo-tipo_padrao.
  PERFORM conversion18 USING 'IN' CHANGING wa_novo-tipo_padrao vmatnr18.
  wa_novo-tipo_padrao = vmatnr18.
  PERFORM conversion   USING 'IN' CHANGING wa_novo-cliente.
  PERFORM conversion   USING 'IN' CHANGING wa_novo-corretor.

  PERFORM: criar_safra.

  CLEAR: wa_desc, wa_novo.

ENDFORM.                    " EDITAR_SAFRA
*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_SAFRA
*&---------------------------------------------------------------------*
FORM excluir_safra  USING    p_row_id.

  DATA vl_answer TYPE char1.

  READ TABLE it_saida INTO wa_saida INDEX p_row_id.

  IF sy-subrc IS INITIAL.
    IF NOT wa_saida-contrato IS INITIAL.

      SELECT SINGLE nro_sol_ov
        FROM zsdt0051
        INTO @DATA(nro_sol_ov)
        WHERE bstkd  EQ @wa_saida-id_contrato
          AND status <> 'D'.  "*-CS2023000189-04.04.2023-#108693-JT

      IF NOT sy-subrc IS INITIAL.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question  = TEXT-009
          IMPORTING
            answer         = vl_answer
          EXCEPTIONS
            text_not_found = 1
            OTHERS         = 2.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.


        IF vl_answer EQ '1'.

*-CS2023000189-04.04.2023-#108693-JT-inicio
          CLEAR l_error.

*-----------------------------
*-------- exclui contratos no Trace
*-----------------------------
          TRY .
              zcl_trace_cotton=>zif_trace_cotton~get_instance(
                 )->set_exclui_contratos( EXPORTING i_id_contrato = wa_saida-id_contrato ).

            CATCH zcx_integracao INTO DATA(ex_integra).
              l_error = abap_true.
*             l_mesg  = ex_integra->msgv1 && '-' && ex_integra->msgv2 && '-' && ex_integra->msgv3 && '-' && ex_integra->msgv4.

            CATCH zcx_error INTO DATA(ex_error).
              l_error = abap_true.
*             l_mesg  = ex_error->msgv1   && '-' && ex_error->msgv2   && '-' && ex_error->msgv3   && '-' && ex_error->msgv4.
          ENDTRY.



          IF l_error = abap_false.
            CALL METHOD cl_grid->refresh_table_display( is_stable = wa_stable ).
            MESSAGE |Envio de informações ao sistema TraceCotton Finalizada. Verifique o Log!| TYPE 'S'. "BUG - 65565
          ELSE.
            MESSAGE |Erro envio Trace Cotton. Ação de Estorno foi Cancelada. Consulte Log| TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.

*          PERFORM f_preenche_dados_api USING wa_saida delete.
*
*          PERFORM f_integracao_trace_cotton USING delete wa_del_contratos
*                                            CHANGING v_return.
*
*          IF v_return EQ 200.
*            UPDATE zsdt0143 SET cancelado    = abap_true
*                           sts_con      = 'C'
*                           c_usnam      = sy-uname
*                           c_data_atual = sy-datum
*                           c_hora_atual = sy-uzeit
*                           t_cotton     = 'X'
*                           t_usuario    = sy-uname
*                           t_data       = sy-datum
*                           t_hora       = sy-uzeit
*                   WHERE id_contrato EQ wa_saida-id_contrato
*                   AND cancelado EQ abap_false.
*          ELSE.
*            MESSAGE |Erro envio Trace Cotton. Ação de Estorno foi Cancelada. Consulte Log| TYPE 'S' DISPLAY LIKE 'E'.
*          ENDIF.
*-CS2023000189-04.04.2023-#108693-JT-fim

        ELSE.
          MESSAGE |Ação de Estorno foi Cancelada!| TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      ELSE.
        MESSAGE |Contrato Vinculado a Solicitação { nro_sol_ov }!| TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ELSE.
      MESSAGE |Contrato em Branco. Verificar!| TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

  CLEAR: it_fcat[], it_saida[].
  PERFORM: seleciona_dados.

  CALL METHOD cl_grid->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  LEAVE TO SCREEN tela_01.
  CLEAR: wa_saida.

ENDFORM.                    " EXCLUIR_SAFRA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SAFRA
*&---------------------------------------------------------------------*
FORM update_safra .

  FREE: l_erro.

  IF ( edit EQ 'X' ).

*** US - 77507 - cbrand - inicio
    IF wa_novo-visao = 'E' AND wa_novo-intercompany = 'X' AND wa_novo-id_contrato_referencia IS INITIAL .
      MESSAGE |Obrigatório preenchimento "ID Contrato Referência"!| TYPE 'S' DISPLAY LIKE 'E'.
      l_erro = abap_true.
      EXIT.
    ENDIF.
*** US - 77507 - CBRAND - Fim

    IF wa_novo-id_contrato_referencia IS NOT INITIAL.
      SELECT *
        INTO @DATA(w_0143)
        FROM zsdt0143
          UP TO 1 ROWS
       WHERE id_contrato  = @wa_novo-id_contrato_referencia
         AND visao        = 'P'
         AND intercompany = 'X'.
      ENDSELECT.

      IF sy-subrc <> 0.
        MESSAGE 'Contrato Id Referência não pode ser utilizado!'  TYPE 'S' DISPLAY LIKE 'E'.
        l_erro = abap_true.
        EXIT.
      ENDIF.

      SELECT *
        INTO @DATA(w_0143x)
        FROM zsdt0143
          UP TO 1 ROWS
       WHERE id_contrato            <> @wa_novo-id_contrato
         AND id_contrato_referencia  = @wa_novo-id_contrato_referencia
         AND cancelado               = @abap_off.
      ENDSELECT.

      IF sy-subrc = 0.
        MESSAGE 'Contrato Id Referência ja está sendo utilizado por outro Contrato!'  TYPE 'S' DISPLAY LIKE 'E'.
        l_erro = abap_true.
        EXIT.
      ENDIF.

*-CS2023000189-04.04.2023-#108693-JT-inicio
      IF wa_novo-empresa = '0015'.
        SELECT id_contrato, empresa
          INTO @DATA(w_0143y)
          FROM zsdt0143
            UP TO 1 ROWS
         WHERE id_contrato = @wa_novo-id_contrato_referencia
           AND cancelado   = @abap_off.
        ENDSELECT.

        IF sy-subrc = 0 AND wa_novo-empresa <> w_0143y-empresa.
          MESSAGE 'Somente pode ser utilizado Contrato da mesma Empresa!'  TYPE 'S' DISPLAY LIKE 'E'.
          l_erro = abap_true.
          EXIT.
        ENDIF.
      ENDIF.
*-CS2023000189-04.04.2023-#108693-JT-fim

    ENDIF.

    IF wa_novo-tipo_padrao IS NOT INITIAL.
      SELECT SINGLE matnr
        FROM mara
        INTO @DATA(l_matnr)
        WHERE matnr EQ @wa_novo-tipo_padrao.
      IF sy-subrc <> 0.
        MESSAGE 'Tipo Padrão não localizado!'  TYPE 'S' DISPLAY LIKE 'E'.
        l_erro = abap_true.
        EXIT.
      ENDIF.
    ENDIF.

    IF wa_novo-empresa  IS INITIAL OR
       wa_novo-contrato IS INITIAL.
      l_erro = abap_true.
      MESSAGE |Obrigatório o Preenchimento de 'Empresa', 'Contrato'!| TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.

      SELECT SINGLE * FROM  zsdt0143 INTO @DATA(w_modif)
       WHERE id_contrato EQ @wa_novo-id_contrato.

      UPDATE zsdt0143 SET  id_contrato          = wa_novo-id_contrato
                         contrato               = wa_novo-contrato
                         contrato_cliente       = wa_novo-contrato_cliente
                         safra                  = wa_novo-safra
                         tp_venda               = wa_novo-tp_venda
                         dt_venda               = wa_novo-dt_venda
                         cliente                = wa_novo-cliente
                         empresa                = wa_novo-empresa
                         vendedor               = wa_novo-vendedor
                         corretor               = wa_novo-corretor
                         comissao               = wa_novo-comissao
                         tipo_padrao            = wa_novo-tipo_padrao
                         porto                  = wa_novo-porto
                         quatidade              = wa_novo-quatidade
                         tolerancia             = wa_novo-tolerancia
                         de_embarque            = wa_novo-de_embarque
                         ate_embarque           = wa_novo-ate_embarque
                         preco                  = wa_novo-preco
                         preco_tons             = wa_novo-preco_tons
                         pctgem_ant             = wa_novo-pctgem_ant
                         qdt_embarcada          = wa_novo-qdt_embarcada
                         sts_sol                = wa_novo-sts_sol
                         sts_con                = wa_novo-sts_con
                         a_usnam                = sy-uname
                         a_data_atual           = sy-datum
                         a_hora_atual           = sy-uzeit
*** US - 77507 - cbrand
                         visao                  = wa_novo-visao
                         intercompany           = wa_novo-intercompany
                         acts                   = wa_novo-acts  "*-CS2023000189-04.04.2023-#108693-JT
                         id_contrato_referencia = wa_novo-id_contrato_referencia
*** US - 77507 - cbrand


                         WHERE id_contrato      = wa_novo-id_contrato.

*-CS2023000189-04.04.2023-#108693-JT-inicio
*-Ajusta ACTS dos contratos dependentes
      SELECT *
        FROM zsdt0143
        INTO TABLE @DATA(t_0143a)
       WHERE id_contrato_referencia = @wa_novo-id_contrato.

      LOOP AT t_0143a    INTO DATA(w_0143a).
        w_0143a-acts        = wa_novo-acts.
        MODIFY zsdt0143  FROM w_0143a.
      ENDLOOP.

      IF w_modif-acts <> wa_novo-acts.
        PERFORM f_gravalog_acts USING wa_novo-id_contrato
                                      w_modif-acts
                                      wa_novo-acts.
      ENDIF.
*-CS2023000189-04.04.2023-#108693-JT-fim

      IF w_modif-cliente       <> wa_novo-cliente      OR w_modif-safra       <> wa_novo-safra OR
         w_modif-contrato      <> wa_novo-contrato     OR w_modif-de_embarque <> wa_novo-de_embarque OR
         w_modif-ate_embarque  <> wa_novo-ate_embarque OR w_modif-quatidade   <> wa_novo-quatidade OR
         w_modif-acts          <> wa_novo-acts         OR "*-CS2023000189-04.04.2023-#108693-JT
         w_modif-tipo_padrao   <> wa_novo-tipo_padrao AND wa_novo-visao = 'P'. " US - 77507 - CBRAND




*-CS2023000189-04.04.2023-#108693-JT-inicio
        CLEAR l_error.


*-----------------------------
*-------- cadastra contratos no Trace
*-----------------------------
        TRY .
            zcl_trace_cotton=>zif_trace_cotton~get_instance(
               )->set_cadastra_contratos( EXPORTING i_id_contrato = wa_novo-id_contrato ).

          CATCH zcx_integracao INTO DATA(ex_integra).
            l_error = abap_true.
*           l_mesg  = ex_integra->msgv1 && '-' && ex_integra->msgv2 && '-' && ex_integra->msgv3 && '-' && ex_integra->msgv4.

          CATCH zcx_error INTO DATA(ex_error).
            l_error = abap_true.
*           l_mesg  = ex_error->msgv1   && '-' && ex_error->msgv2   && '-' && ex_error->msgv3   && '-' && ex_error->msgv4.
        ENDTRY.




        IF l_error = abap_false.
          MESSAGE |Contrato modificado e enviado ao Trace Cotton com Sucesso!| TYPE 'S'.

        ELSE.
          MESSAGE |Contrato modificado e salvo com sucesso. Erro ao enviar ao sistema Trace Cotton. Verificar Log!| TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

*        IF w_modif-t_cotton IS NOT INITIAL.
*          PERFORM f_preenche_dados_api USING wa_novo put.
*
*          PERFORM f_integracao_trace_cotton USING put wa_contratos  CHANGING v_return.
*        ELSE.
*          PERFORM f_preenche_dados_api USING wa_novo post.
*
*          PERFORM f_integracao_trace_cotton USING post wa_contratos  CHANGING v_return.
*        ENDIF.
*
*
*        IF v_return EQ 200.
*          wa_novo-t_cotton  = 'X'.
*          wa_novo-t_data    = sy-datum.
*          wa_novo-t_hora    = sy-uzeit.
*          wa_novo-t_usuario = sy-uname.
*          MODIFY zsdt0143 FROM wa_novo.
*          COMMIT WORK.
*
*          MESSAGE |Contrato modificado e enviado ao Trace Cotton com Sucesso!| TYPE 'S'.
*
*        ELSE.
*          MESSAGE |Contrato modificado e salvo com sucesso. Erro ao enviar ao sistema Trace Cotton. Verificar Log!| TYPE 'S' DISPLAY LIKE 'E'.
*        ENDIF.
*-CS2023000189-04.04.2023-#108693-JT-fim

      ELSE.
        MESSAGE |Contrato modificado com Sucesso!| TYPE 'S' .
      ENDIF.

      CLEAR:it_fcat[], it_saida[].
      PERFORM: seleciona_dados.

      CALL METHOD cl_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

      CLEAR: edit.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.
ENDFORM.                    " UPDATE_SAFRA

*-CS2023000189-04.04.2023-#108693-JT-inicio
*****************************************************
* gravar log
*****************************************************
FORM f_gravalog_acts USING p_id
                           p_acts_old
                           p_acts_new.

  DATA: l_seq  TYPE timestampl,
        w_0326 TYPE zsdt0326.

  DO.
    GET TIME STAMP FIELD l_seq.

    SELECT SINGLE *
      FROM zsdt0326
      INTO w_0326
     WHERE id_contrato = p_id
       AND field       = 'ACTS'
       AND sequencia   = l_seq.

    CHECK sy-subrc <> 0.

    w_0326-mandt        = sy-mandt.
    w_0326-id_contrato  = p_id.
    w_0326-field        = 'ACTS'.
    w_0326-sequencia    = l_seq.
    w_0326-value_old    = p_acts_old.
    w_0326-value_new    = p_acts_new.
    w_0326-us_data      = sy-datum.
    w_0326-us_hora      = sy-uzeit.
    w_0326-us_user      = sy-uname.
    MODIFY zsdt0326  FROM w_0326.

    EXIT.
  ENDDO.

ENDFORM.
*-CS2023000189-04.04.2023-#108693-JT-fim

*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT
*&---------------------------------------------------------------------*
FORM create_object .
  DATA: gr_event_handler TYPE REF TO lcl_event_handler.

  IF ( cl_container IS INITIAL ).

    CREATE OBJECT cl_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.


    CREATE OBJECT cl_grid
      EXPORTING
        i_parent = cl_container.

    PERFORM: fcat USING abap_false.

  ENDIF.


  CREATE OBJECT gr_event_handler.
  SET HANDLER gr_event_handler->handle_hotspot_click FOR cl_grid.

*  DATA: IT_EXCLUDE_FCODE TYPE UI_FUNCTIONS WITH HEADER LINE.
*
*  IT_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL.
*  APPEND IT_EXCLUDE_FCODE.

  CALL METHOD cl_grid->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      is_variant                    = wa_variant
*     IT_TOOLBAR_EXCLUDING          = IT_EXCLUDE_FCODE[]
      i_save                        = 'A'
    CHANGING
      it_outtab                     = it_saida
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.


ENDFORM.                    " CREATE_OBJECT
*&---------------------------------------------------------------------*
*&      Form  FCAT
*&---------------------------------------------------------------------*
FORM fcat USING p_dir.
  FREE it_fcat.

  IF p_dir IS INITIAL.
    PERFORM monta_catalog USING:
      'EXCLUIR'          'Excluir'  'Excluir' 'Exc.'        '04' '' 'X'  '' 'C' '' '',
      'EDIT'             'Editar'   'Editar'  'Edt'        '04' '' 'X'  '' 'C' '' ''.
  ELSE.
    PERFORM monta_catalog USING:
    'S_SOL'            'Status da Solicitação' 'Sts Solic.' 'Sol' '02' '' ''  '' 'C' '' '',
    'S_CON'            'Status do Contrato'    'Sts Contr.' 'Cont' '02' '' ''  '' 'C' '' ''.
  ENDIF.

  PERFORM monta_catalog USING:
  'ID_CONTRATO'             'Id Contrato'            'Id Cont'                  'IdCont'                 '' 'X' ''  '' ''  '' '',    "'10'
  'SAFRA'                   'Safra'                  'Safra'                    'Safra'                  '' ''  ''  '' ''  '' '',    "'05'
  'CONTRATO'                'Contrato'               'Contrat'                  'Cont.'                  '' ''  ''  '' ''  '' '',    "'10'
  'CONTRATO_CLIENTE'        'Contrato Cliente'       'Contrat'                  'Contr.Cli.'             '' ''  ''  '' ''  '' '',    "'10'
  'ACTS'                    'Acts'                   'Acts'                     'Acts'                   '' ''  ''  '' ''  '' '',    "'10' "*-CS2023000189-04.04.2023-#108693-JT
  'DT_VENDA'                'Data Venda'             'Data Ve'                  'Dt.Ven.'                '' ''  ''  '' ''  '' '',    "'10'
  'CLIENTE'                 'Cliente'                'Cliente'                  'Clien.'                 '' ''  ''  '' ''  '' '',    "'10'
  'DESC_CLIENTE'            'Desc Cliente'           'Desc Cl'                  'D.Clie.'                '' ''  ''  '' ''  '' '',    "'10'
  'EMPRESA'                 'Empresa'                'Empresa'                  'Empre.'                 '' ''  ''  '' ''  '' '',    "'10'
  'DESC_EMPRESA'            'Desc Empresa'           'Desc Em'                  'D.Empre.'               '' ''  ''  '' ''  '' '',    "'10'
  'VENDEDOR'                'Equipe de Vendas'       'Equipe '                  'Eqp.Vend.'              '' ''  ''  '' ''  '' '',    "'03'
  'DESC_VENDEDOR'           'Desc Vendedor'          'Desc Ve'                  'D.Vend'                 '' ''  ''  '' ''  '' '',    "'10'
  'CORRETOR'                'Corretor'               'Correto'                  'Cor.'                   '' ''  ''  '' ''  '' '',    "'10'
  'DESC_CORRETOR'           'Desc Corretor'          'Desc Co'                  'D.Cor'                  '' ''  ''  '' ''  '' '',    "'10'
  'COMISSAO'                'Comissão %'             'Comissã'                  'Coms. %'                '' ''  ''  '' ''  '' '',    "'10'
  'TIPO_PADRAO'             'Tipo Padão'             'Tipo Pa'                  'Tp.Pd'                  '18' 'X' ''  '' ''  '' '',    "'10'
  'DESC_TIPO_PADRAO'        'Desc Tipo'              'Desc Ti'                  'D.Tp'                   '' ''  ''  '' ''  '' '',    "'05'
  'PORTO'                   'Porto'                  'Porto'                    'Prto'                   '' ''  ''  '' ''  '' '',    "'10'
  'QUATIDADE'               'Quantidade/KG'          'Quantid'                  'Qtd/KG'                 '' ''  ''  '' ''  '' '',    "'16'
  'TOLERANCIA'              'Tolerância %'           'Tolerân'                  'Tlr %'                  '' ''  ''  '' ''  '' '',    "'10'
  'DE_EMBARQUE'             'De'                     'De'                       'De'                     '' ''  ''  '' ''  '' '',    "'10'
  'ATE_EMBARQUE'            'Até'                    'Até'                      'Até'                    '' ''  ''  '' ''  '' '',    "'10'
  'QDT_EMBARCADA'           'Qtd Embarcada'          'Qtd Emb'                  'Qtd Emb/KG'             '' ''  ''  '' ''  '' '',    "'16'
  'PRECO'                   'Preço LB/USD'           'Preço L'                  'PrLB/USD'               '' ''  ''  '' ''  '' '',    "'10'
  'PRECO_TONS'              'Preço Tonelada'         'Preço T'                  'PrTN'                   '' ''  ''  '' ''  '' '',    "'10'
  'PCTGEM_ANT'              '% Antecipação'          '% Antec'                  '% Ant.'                 '' ''  ''  '' ''  '' '',    "'10'
  'NRO_SOL_OV'              'Solicitações'           'Nro.Sol'                  'N. Sol.'                '' ''  ''  '' ''  '' '',
  'PEDIDO'                  'Pedido'                 'Pedido'                   'Pedido'                 '' ''  ''  '' ''  '' '',     "'10'
  'VISAO_DESC'              'Visão'                  'Visão'                    'Visão'                  '' ''  ''  '' ''  '' '',    "'10'
  'INTERCOMPANY'            'Intercompany?'          'Intercompany?'            'Intercompany?'          '' ''  ''  '' ''  '' '',    "'10'
  'ID_CONTRATO_REFERENCIA'  'Id Contrato Referência' 'Id Contrato Referência'   'Id Contrato Referência' '' ''  ''  '' ''  '' ''.    "'10'

*-BUG 69276 - JT - 23.11.2021 - inicio
* IF p_empre-low <> '0001'.
  IF l_envia_trace = abap_true.
*-BUG 69276 - JT - 23.11.2021 - fim
    PERFORM monta_catalog USING:
    'T_COTTON'      'Envio T.Cotton'      'Env.TCotton' 'Ev TC'     '14' ''  ''   '' ''   '' '',    "'10'
    'T_DATA'        'Data Envio'          'Dt Envio'    'DT'        '12' ''  ''   '' ''   '' '',    "'05'
    'T_HORA'        'Hora Envio'          'Hr Envio'    'Hr'        '12' ''  ''   '' ''   '' '',    "'10'
    'T_USUARIO'     'Usuário Envio'       'Us Envio'    'Us'        '12' ''  ''   '' ''   '' '',    "'10'
    'T_LOG'         'Log Envio'           'Log Envio'   'Log'       '10' ''  'X'  '' 'C'  '' ''.    "'10'
  ENDIF.

ENDFORM.                    " FCAT
*&---------------------------------------------------------------------*
*&      Form  MONTA_CATALOG
*&---------------------------------------------------------------------*
FORM monta_catalog  USING  VALUE(p_fieldname)
                           VALUE(p_descl)
                           VALUE(p_desch)
                           VALUE(p_descs)
                           VALUE(p_tam)
                           VALUE(p_no_zero)
                           VALUE(p_hotspot)
                           VALUE(p_cor)
                           VALUE(p_just)
                           VALUE(p_sum)
                           VALUE(p_out).
  CASE p_fieldname.
    WHEN  'CONTRATO'.
      APPEND VALUE #(
               fieldname = p_fieldname
               tabname   = COND #( WHEN r_relat NE abap_true THEN 'ZSDT0143' ELSE 'IT_RELATORIO' )
               scrtext_l = p_descl
               scrtext_m = p_desch
               scrtext_s = p_descs
               outputlen = p_tam
               no_zero   = p_no_zero
               hotspot   = p_hotspot
               emphasize = p_cor
               just      = p_just
               do_sum    = p_sum
               no_out    = p_out
               datatype  = 'CHAR'
               inttype   = 'C'
               intlen    = '000035'
               lowercase = 'X'
      ) TO it_fcat.

    WHEN  'CONTRATO_CLIENTE'.
      APPEND VALUE #(
               fieldname = p_fieldname
               tabname   = COND #( WHEN r_relat NE abap_true THEN 'ZSDT0143' ELSE 'IT_RELATORIO' )
               scrtext_l = p_descl
               scrtext_m = p_desch
               scrtext_s = p_descs
               outputlen = p_tam
               no_zero   = p_no_zero
               hotspot   = p_hotspot
               emphasize = p_cor
               just      = p_just
               do_sum    = p_sum
               no_out    = p_out
               datatype  = 'CHAR'
               inttype   = 'C'
               intlen    = '000035'
               lowercase = ' '
      ) TO it_fcat.

    WHEN OTHERS.
      IF p_fieldname = 'ACTS'.  "*-CS2023000189-04.04.2023-#108693-JT
        APPEND VALUE #(
                 fieldname = p_fieldname
                 tabname   = COND #( WHEN r_relat NE abap_true THEN 'ZSDT0143' ELSE 'IT_RELATORIO' )
                 scrtext_l = p_descl
                 scrtext_m = p_desch
                 scrtext_s = p_descs
                 outputlen = p_tam
                 no_zero   = p_no_zero
                 hotspot   = abap_true  "p_hotspot
                 emphasize = p_cor
                 just      = p_just
                 do_sum    = p_sum
                 no_out    = p_out
                 checkbox  = abap_true
        ) TO it_fcat.

      ELSE.
        APPEND VALUE #(
                 fieldname = p_fieldname
                 tabname   = COND #( WHEN r_relat NE abap_true THEN 'ZSDT0143' ELSE 'IT_RELATORIO' )
                 scrtext_l = p_descl
                 scrtext_m = p_desch
                 scrtext_s = p_descs
                 outputlen = p_tam
                 no_zero   = p_no_zero
                 hotspot   = p_hotspot
                 emphasize = p_cor
                 just      = p_just
                 do_sum    = p_sum
                 no_out    = p_out
        ) TO it_fcat.
      ENDIF.
  ENDCASE.

ENDFORM.                    " MONTA_CATALOG

*&---------------------------------------------------------------------*
*&      Form  CRIAR_SAFRA
*&---------------------------------------------------------------------*
FORM copiar_safra .

  CLEAR: it_sel_rows[].

  CALL METHOD cl_grid->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  IF it_sel_rows[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Selecione uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF lines( it_sel_rows[] ) > 1.
    MESSAGE s024(sd) WITH 'Selecione apenas uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  edit = abap_off.
  acao = abap_true.

  READ TABLE it_sel_rows   INTO wa_sel_rows INDEX 1.

  PERFORM: editar_safra   USING wa_sel_rows-index.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CRIAR_SAFRA
*&---------------------------------------------------------------------*
FORM criar_safra .
  "CALL SCREEN TELA_02 STARTING AT 15 10 ENDING AT 65 27.
  IF sy-ucomm = 'CREATE'.
    CLEAR:  wa_novo, wa_desc-desc_cliente,
            wa_desc-desc_empresa,
            wa_desc-desc_cliente,
            wa_desc-desc_vendedor,
            wa_desc-desc_corretor,
            wa_desc-desc_tipo_padrao,
            wa_desc-desc_tp_venda.
    wa_novo-visao   = 'E'.
    wa_novo-safra   = p_safra-low.
  ELSEIF sy-ucomm = 'COPIAR'.
    CLEAR: wa_novo-t_cotton.
  ENDIF.

  IF wa_novo-visao IS INITIAL.
    wa_novo-visao = 'E'.
  ENDIF.
  wa_novo-empresa = p_empre-low.

  CALL SCREEN tela_02 STARTING AT 15 10 ENDING AT 80 32. "28
  "CALL SCREEN tela_02 STARTING AT 15 10 ENDING AT 80 27.
ENDFORM.                    " CRIAR_SAFRA
*&---------------------------------------------------------------------*
*&      Module  PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0200 OUTPUT.
  SET PF-STATUS 'PF0200'.
  SET TITLEBAR  'TB0200'.
ENDMODULE.                 " PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0200  INPUT
*&---------------------------------------------------------------------*

MODULE pai_0200 INPUT.

  CASE sy-ucomm.
    WHEN: 'BACK' OR 'EXIT' OR 'CANC' OR 'CLOSE'.
      CLEAR: wa_novo, edit.
      LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
    WHEN: 'OK'.
      IF ( edit EQ 'X' ).
        PERFORM: update_safra.
      ELSE.
        PERFORM: inserir_safra.
      ENDIF.
      IF l_erro IS INITIAL.
        CLEAR wa_novo.
      ENDIF.
    WHEN 'BPRT'.
      consolidar=>get_porto( ).
    WHEN 'CHCK'.
      "PERFORM trata_fields.
  ENDCASE.

ENDMODULE.                 " PAI_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  INSERIR_SAFRA
*&---------------------------------------------------------------------*
FORM inserir_safra .

  DATA: seq TYPE zsdt0143-id_contrato.

  FREE: l_erro.

  IF wa_novo-id_contrato_referencia IS NOT INITIAL.
    SELECT *
      INTO @DATA(w_0143)
      FROM zsdt0143
        UP TO 1 ROWS
     WHERE id_contrato  = @wa_novo-id_contrato_referencia
       AND visao        = 'P'
       AND intercompany = 'X'.
    ENDSELECT.

    IF sy-subrc <> 0.
      MESSAGE 'Contrato Id Referência não pode ser utilizado!'  TYPE 'S' DISPLAY LIKE 'E'.
      l_erro = abap_true.
      EXIT.
    ENDIF.

    SELECT *
      INTO @DATA(w_0143x)
      FROM zsdt0143
        UP TO 1 ROWS
     WHERE id_contrato            <> @wa_novo-id_contrato
       AND id_contrato_referencia  = @wa_novo-id_contrato_referencia
       AND cancelado               = @abap_off.
    ENDSELECT.

    IF sy-subrc = 0.
      MESSAGE 'Contrato Id Referência ja está sendo utilizado por outro Contrato!'  TYPE 'S' DISPLAY LIKE 'E'.
      l_erro = abap_true.
      EXIT.
    ENDIF.

*-CS2023000189-04.04.2023-#108693-JT-inicio
    IF wa_novo-empresa = '0015'.
      SELECT id_contrato, empresa
        INTO @DATA(w_0143y)
        FROM zsdt0143
          UP TO 1 ROWS
       WHERE id_contrato = @wa_novo-id_contrato_referencia
         AND cancelado   = @abap_off.
      ENDSELECT.

      IF sy-subrc = 0 AND wa_novo-empresa <> w_0143y-empresa.
        MESSAGE 'Somente pode ser utilizado Contrato da mesma Empresa!'  TYPE 'S' DISPLAY LIKE 'E'.
        l_erro = abap_true.
        EXIT.
      ENDIF.
    ENDIF.
*-CS2023000189-04.04.2023-#108693-JT-fim

  ENDIF.

  IF wa_novo-tipo_padrao IS NOT INITIAL.
    "CS2023000851 Algodão - PT1 - Atualização da transação ZSDT0118 - Formato já executado na Amaggi e mais detalhado --128361 - BG
    CLEAR vmatnr18.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = wa_novo-tipo_padrao
      IMPORTING
        output       = vmatnr18
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    SELECT SINGLE matnr
      FROM mara
      INTO @DATA(l_matnr)
      WHERE matnr EQ @vmatnr18.
    IF sy-subrc <> 0.
      MESSAGE 'Tipo Padrão não localizado!'  TYPE 'S' DISPLAY LIKE 'E'.
      l_erro = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

  IF wa_novo-safra IS INITIAL.
    MESSAGE |O Campo Safra é Obrigatorio!| TYPE 'S' DISPLAY LIKE 'E'.
    l_erro = abap_true.
    EXIT.
  ENDIF.

  IF wa_novo-porto IS INITIAL.
    MESSAGE |O Campo Porto é Obrigatorio!| TYPE 'S' DISPLAY LIKE 'E'.
    l_erro = abap_true.
    EXIT.
  ENDIF.

  IF wa_novo-dt_venda IS INITIAL.
    MESSAGE |O Campo Data de Venda é Obrigatorio!| TYPE 'S' DISPLAY LIKE 'E'.
    l_erro = abap_true.
    EXIT.
  ENDIF.

  IF wa_novo-de_embarque IS INITIAL.
    MESSAGE |O Campo Embarque é Obrigatorio!| TYPE 'S' DISPLAY LIKE 'E'.
    l_erro = abap_true.
    EXIT.
  ENDIF.

*** US - 77507 - CBRAND - Inicio
  IF wa_novo-visao = 'E' AND wa_novo-intercompany = 'X' AND wa_novo-id_contrato_referencia IS INITIAL . .
    MESSAGE |Obrigatório preenchimento "ID Contrato Referência"!| TYPE 'S' DISPLAY LIKE 'E'.
    l_erro = abap_true.
    EXIT.
  ENDIF.
*** US - 77507 - CBRAND - Fim

  CONDENSE wa_novo-empresa NO-GAPS.

  IF wa_novo-empresa IS INITIAL OR wa_novo-contrato IS INITIAL.
    MESSAGE |Obrigatório o Preenchimento de 'Empresa', 'Contrato'!| TYPE 'S' DISPLAY LIKE 'E'.
    l_erro = abap_true.
  ELSEIF wa_novo-empresa EQ '*'.
    MESSAGE |Entre com uma Empresa Valida!| TYPE 'S' DISPLAY LIKE 'E'.
    l_erro = abap_true.
  ELSE.

    IF wa_novo IS NOT INITIAL.

      SELECT COUNT( * )
        FROM zsdt0143
        INTO seq.

      ADD 1 TO seq.

      wa_novo-id_contrato   =   seq.

      PERFORM conversion USING 'IN' CHANGING wa_novo-id_contrato.
*     PERFORM conversion USING 'IN' CHANGING wa_novo-tipo_padrao.
      PERFORM conversion18 USING 'IN' CHANGING wa_novo-tipo_padrao vmatnr18.
      wa_novo-tipo_padrao = vmatnr18.
      PERFORM conversion USING 'IN' CHANGING wa_novo-corretor.
      PERFORM conversion USING 'IN' CHANGING wa_novo-cliente.

      wa_novo-usnam         =   sy-uname.
      wa_novo-data_atual    =   sy-datum.
      wa_novo-hora_atual    =   sy-uzeit.

      INSERT INTO zsdt0143 VALUES wa_novo.

      COMMIT WORK AND WAIT.

*-CS2023000189-04.04.2023-#108693-JT-inicio
      CLEAR l_error.

*-----------------------------
*---- cadastra contratos no Trace
*-----------------------------
      TRY .
          zcl_trace_cotton=>zif_trace_cotton~get_instance(
             )->set_cadastra_contratos( EXPORTING i_id_contrato = wa_novo-id_contrato ).



        CATCH zcx_integracao INTO DATA(ex_integra).
          l_error = abap_true.
*         l_mesg  = ex_integra->msgv1 && '-' && ex_integra->msgv2 && '-' && ex_integra->msgv3 && '-' && ex_integra->msgv4.

        CATCH zcx_error INTO DATA(ex_error).
          l_error = abap_true.
*         l_mesg  = ex_error->msgv1   && '-' && ex_error->msgv2   && '-' && ex_error->msgv3   && '-' && ex_error->msgv4.
      ENDTRY.

      IF l_error = abap_false.
        MESSAGE |Contrato Criado e enviado ao Sistema Trace Cotton com Sucesso!| TYPE 'S'.
      ELSE.
        MESSAGE |Contrato Criado e salvo com sucesso. Erro ao enviar ao sistema Trace Cotton. Verificar Log!| TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

*      PERFORM f_preenche_dados_api USING wa_novo post.
*
*      PERFORM f_integracao_trace_cotton USING post wa_contratos
*                                                CHANGING v_return.
*
*      IF v_return EQ 200.
*        wa_novo-t_cotton  = 'X'.
*        wa_novo-t_data    = sy-datum.
*        wa_novo-t_hora    = sy-uzeit.
*        wa_novo-t_usuario = sy-uname.
*
*        MODIFY zsdt0143 FROM wa_novo.
*        COMMIT WORK.
*
*        MESSAGE |Contrato Criado e enviado ao Sistema Trace Cotton com Sucesso!| TYPE 'S'.
*
*      ELSE.
*        MESSAGE |Contrato Criado e salvo com sucesso. Erro ao enviar ao sistema Trace Cotton. Verificar Log!| TYPE 'S' DISPLAY LIKE 'E'.
*      ENDIF.
*-CS2023000189-04.04.2023-#108693-JT-fim


      CLEAR: it_fcat[], it_saida[].
      PERFORM: seleciona_dados.

      CALL METHOD cl_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.

ENDFORM.                    " INSERIR_SAFRA
*&---------------------------------------------------------------------*
*&      Form  CONVERSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0965   text
*      <--P_SEQ  text
*----------------------------------------------------------------------*
FORM conversion  USING p_in_ou
                 CHANGING p_conversao.

  DATA conversao TYPE c LENGTH 28.

  CASE p_in_ou.
    WHEN 'IN'.
      conversao = 'CONVERSION_EXIT_ALPHA_INPUT'.
    WHEN 'OU'.
      conversao = 'CONVERSION_EXIT_ALPHA_OUTPUT'.
  ENDCASE.

  CALL FUNCTION conversao
    EXPORTING
      input  = p_conversao
    IMPORTING
      output = p_conversao.

ENDFORM.

FORM conversion18    USING p_in_ou
                  CHANGING p_conversao
                           p_conversao2 TYPE matnr18.

  DATA conversao TYPE c LENGTH 28.

  CASE p_in_ou.
    WHEN 'IN'.
      conversao = 'CONVERSION_EXIT_ALPHA_INPUT'.
    WHEN 'OU'.
      conversao = 'CONVERSION_EXIT_ALPHA_OUTPUT'.
  ENDCASE.

  CALL FUNCTION conversao
    EXPORTING
      input  = p_conversao
    IMPORTING
      output = p_conversao2.

ENDFORM.

FORM f_atualiza_dados.

  CLEAR: wa_desc-desc_corretor, wa_desc-desc_cliente, wa_desc-desc_vendedor, wa_desc-desc_empresa.

* PERFORM conversion USING 'IN' CHANGING wa_novo-tipo_padrao.
  PERFORM conversion18 USING 'IN' CHANGING wa_novo-tipo_padrao vmatnr18.
  wa_novo-tipo_padrao = vmatnr18.
  PERFORM conversion USING 'IN' CHANGING wa_novo-corretor.
  PERFORM conversion USING 'IN' CHANGING wa_novo-cliente.
  PERFORM conversion USING 'IN' CHANGING wa_novo-empresa.
  PERFORM conversion USING 'IN' CHANGING wa_novo-vendedor.

  SELECT SINGLE maktx FROM makt
    INTO @DATA(_desc)
    WHERE matnr EQ @wa_novo-tipo_padrao.

  IF sy-subrc = 0.
    DATA(line) = strlen( _desc ).
    line = line - 4.
    IF line > 0.
      wa_desc-desc_tipo_padrao = _desc+line.
    ENDIF.
  ELSE.
    CLEAR: wa_desc-desc_tipo_padrao.
  ENDIF.

  SELECT SINGLE name1 FROM lfa1
    INTO wa_desc-desc_corretor
    WHERE lifnr EQ wa_novo-corretor.

*-CS2023000189-04.04.2023-#108693-JT-inicio
*  SELECT SINGLE name1 FROM kna1
*    INTO wa_desc-desc_cliente
*    WHERE kunnr EQ wa_novo-cliente.

  SELECT SINGLE adrnr FROM kna1
    INTO @DATA(l_adrnr)
    WHERE kunnr EQ @wa_novo-cliente.

  SELECT SINGLE *
    FROM adrc
    INTO @DATA(w_adrc)
   WHERE addrnumber EQ @l_adrnr
     AND date_to    >= @sy-datum.

  wa_desc-desc_cliente = w_adrc-name1 && w_adrc-name2.
*-CS2023000189-04.04.2023-#108693-JT-fim

  SELECT SINGLE bezei
    FROM tvgrt
    INTO wa_desc-desc_vendedor
    WHERE vkgrp EQ wa_novo-vendedor
      AND spras EQ sy-langu.

  SELECT SINGLE butxt
    FROM t001
    INTO wa_desc-desc_empresa
    WHERE bukrs EQ wa_novo-empresa.

ENDFORM.

*&---------------------------------------------------------------------*
*&  COPIAR CONTRATO
*&---------------------------------------------------------------------*
FORM copiar_contrato.

  CLEAR: it_sel_rows[].

  CALL METHOD grid->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  IF it_sel_rows[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Selecione uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF lines( it_sel_rows[] ) > 1.
    MESSAGE s024(sd) WITH 'Selecione apenas uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE it_sel_rows   INTO wa_sel_rows INDEX 1.

  CLEAR at_id.

  at_id   = wa_sel_rows-index.
  lv_acao = 'C'.
  l_copia = abap_true.

  consolidar=>get_dados( 'C' ).

  CALL SCREEN 0510.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ATUALIZA_DADOS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atualiza_dados INPUT.

  PERFORM f_atualiza_dados.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.

*  DATA: tl_function TYPE ui_functions.

  SET PF-STATUS 'PF300'.
  SET TITLEBAR 'TI300'.

  it_sort = VALUE #(
                    ( spos = '1' fieldname = 'ORDEM' )
                    ( spos = '2' fieldname = 'SAFRA' )
                    ( spos = '3' fieldname = 'DESC_CLIENTE' )
                   ).

  FREE it_fcat.

  it_fcat = VALUE #(
                      ( col_pos = 01 fieldname = 'SAFRA'        scrtext_l = 'Safra'    )
                      ( col_pos = 02 fieldname = 'DESC_CLIENTE' scrtext_l = 'Cliente'  )
                      ( col_pos = 03 fieldname = 'CONTRATO'     scrtext_l = 'Contrato' )
                      ( col_pos = 04 fieldname = 'TIPO'         scrtext_l = 'Tipo'     )
                      ( col_pos = 05 fieldname = 'PRECO'        scrtext_l = 'UC$/LB'   )
                      ( col_pos = 06 fieldname = 'PRECO_LB'     scrtext_l = 'U$/@'     )
                      ( col_pos = 07 fieldname = 'JAN'          scrtext_l = 'Janeiro'   no_out = COND #( WHEN wa_mes-jan IS NOT INITIAL THEN abap_false ELSE abap_true ) )
                      ( col_pos = 08 fieldname = 'FEV'          scrtext_l = 'Fevereiro' no_out = COND #( WHEN wa_mes-fev IS NOT INITIAL THEN abap_false ELSE abap_true ) )
                      ( col_pos = 09 fieldname = 'MAR'          scrtext_l = 'Março'     no_out = COND #( WHEN wa_mes-mar IS NOT INITIAL THEN abap_false ELSE abap_true ) )
                      ( col_pos = 10 fieldname = 'ABR'          scrtext_l = 'Abril'     no_out = COND #( WHEN wa_mes-abr IS NOT INITIAL THEN abap_false ELSE abap_true ) )
                      ( col_pos = 11 fieldname = 'MAI'          scrtext_l = 'Maio'      no_out = COND #( WHEN wa_mes-mai IS NOT INITIAL THEN abap_false ELSE abap_true ) )
                      ( col_pos = 12 fieldname = 'JUN'          scrtext_l = 'Junho'     no_out = COND #( WHEN wa_mes-jun IS NOT INITIAL THEN abap_false ELSE abap_true ) )
                      ( col_pos = 13 fieldname = 'JUL'          scrtext_l = 'Julho'     no_out = COND #( WHEN wa_mes-jul IS NOT INITIAL THEN abap_false ELSE abap_true ) )
                      ( col_pos = 14 fieldname = 'AGO'          scrtext_l = 'Agosto'    no_out = COND #( WHEN wa_mes-ago IS NOT INITIAL THEN abap_false ELSE abap_true ) )
                      ( col_pos = 15 fieldname = 'SET'          scrtext_l = 'Setembro'  no_out = COND #( WHEN wa_mes-set IS NOT INITIAL THEN abap_false ELSE abap_true ) )
                      ( col_pos = 16 fieldname = 'OUT'          scrtext_l = 'Outubro'   no_out = COND #( WHEN wa_mes-out IS NOT INITIAL THEN abap_false ELSE abap_true ) )
                      ( col_pos = 17 fieldname = 'NOV'          scrtext_l = 'Novembro'  no_out = COND #( WHEN wa_mes-nov IS NOT INITIAL THEN abap_false ELSE abap_true ) )
                      ( col_pos = 18 fieldname = 'DEV'          scrtext_l = 'Dezembro'  no_out = COND #( WHEN wa_mes-dev IS NOT INITIAL THEN abap_false ELSE abap_true ) )
                      ( col_pos = 19 fieldname = 'QTD_TONS'     scrtext_l = 'Qtd Tons'  )
                      ( col_pos = 20 fieldname = 'TOTAL'        scrtext_l = 'U$ Total'  )
                      ( col_pos = 20 fieldname = 'ORDEM'        scrtext_l = 'Ordem'     no_out = abap_true )
                   ).

  IF ( _cont IS INITIAL ).

    CREATE OBJECT _cont
      EXPORTING
        container_name              = 'CC_'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT _grid
      EXPORTING
        i_parent = _cont.

  ENDIF.

  wa_layout-cwidth_opt = abap_true.
  wa_layout-info_fname = 'COLOR'.
  wa_layout-stylefname = 'ESTILO'.

  tl_function = VALUE #( ( cl_gui_alv_grid=>mc_fc_excl_all ) ).

  CALL METHOD _grid->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding          = tl_function
      is_layout                     = wa_layout
    CHANGING
      it_outtab                     = it_relatorio
      it_fieldcatalog               = it_fcat
      it_sort                       = it_sort
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'GERAEXCEL'.
      consolidar=>gera_rel( ).
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0400 OUTPUT.
  SET PF-STATUS 'PF400'.
  SET TITLEBAR 'TI400'.

  FREE it_fcat.

  it_fcat = VALUE #(
                      ( col_pos = 01 fieldname = 'SAFRA'      scrtext_l = 'Safra'    )
                      ( col_pos = 02 fieldname = 'EMPRESA'    scrtext_l = 'Empresa'  )
                      ( col_pos = 03 fieldname = 'FAZENDA'    scrtext_l = 'Fazenda'  )
                      ( col_pos = 04 fieldname = 'QUANTIDADE' scrtext_l = 'Projeção' outputlen = 15 )
                   ).

  IF ( p_cont IS INITIAL ).

    CREATE OBJECT p_cont
      EXPORTING
        container_name              = 'CC_1'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT p_grid
      EXPORTING
        i_parent = p_cont.

  ENDIF.

  CALL METHOD p_grid->set_table_for_first_display
    CHANGING
      it_outtab                     = it_projecao
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0400 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'FADD'.
      consolidar=>add_projecao( wa_projecao ).
    WHEN 'FDEL'.
      consolidar=>del_projecao( ).
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_PROJECAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_projecao INPUT.

  IF consolidar=>get_empresa( wa_projecao-empresa ) IS INITIAL.
    CLEAR: wa_projecao-empresa, desc_empresa.
  ELSE.
    desc_empresa = consolidar=>get_empresa( wa_projecao-empresa ).
  ENDIF.

  IF consolidar=>get_centro( wa_projecao-fazenda ) IS INITIAL.
    CLEAR: wa_projecao-fazenda, desc_centro.
  ELSE.
    desc_centro = consolidar=>get_centro( wa_projecao-fazenda ).
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_PORTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_porto INPUT.
*
*  DATA T_RETURN TYPE TABLE OF DDSHRETVAL.
*
*  TYPES: BEGIN OF TY_PORTO,
*           SEQ      TYPE  SY-TABIX,
*           DS_PORTO TYPE ZDS_PORTO,
*         END OF TY_PORTO.
*
*  DATA T_PORTO TYPE TABLE OF TY_PORTO.
*
*  SELECT DS_PORTO
*  FROM ZNOM_TRANSPORTE
*  INTO CORRESPONDING FIELDS OF TABLE T_PORTO.
*
*  DELETE T_PORTO WHERE DS_PORTO IS INITIAL.
*
*  SORT T_PORTO BY DS_PORTO.
*  DELETE ADJACENT DUPLICATES FROM T_PORTO COMPARING DS_PORTO.
*
*  LOOP AT T_PORTO ASSIGNING FIELD-SYMBOL(<L_POR>).
*    <L_POR>-SEQ = SY-TABIX.
*  ENDLOOP.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      RETFIELD        = 'DS_PORTO'
*      DYNPPROG        = SY-REPID
*      DYNPNR          = SY-DYNNR
*      DYNPROFIELD     = 'WA_NOVO-PORTO'
*      VALUE_ORG       = 'S'
*    TABLES
*      VALUE_TAB       = T_PORTO
*      RETURN_TAB      = T_RETURN
*    EXCEPTIONS
*      PARAMETER_ERROR = 1
*      NO_VALUES_FOUND = 2
*      OTHERS          = 3.
*
*  IF SY-SUBRC IS INITIAL.
*    TRY .
*        WA_NOVO-PORTO = T_RETURN[ 1 ]-FIELDVAL.
*      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*    ENDTRY.
*  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo0500 OUTPUT.

  SET TITLEBAR 'TITLE0500'.
  SET PF-STATUS 'STATUS0500'.

  consolidar=>creat_doc( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai0500 INPUT.

  CLEAR lv_acao.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'REFRESH'.
      consolidar=>get_dados( ).
*-CS2021000532-#84648-09.08.2022-JT-inicio
    WHEN 'COPIAR'.
      PERFORM copiar_contrato.
*-CS2021000532-#84648-09.08.2022-JT-fim

      "CS2023000851 Algodão - PT1 - Atualização da transação ZSDT0118 - Formato já executado na Amaggi e mais detalhado --128361 - BG
    WHEN 'ZSDT0062'.
      PERFORM chama_zsdt0062.
    WHEN 'NEW'.
      CLEAR: w_contrato, at_id, l_copia.
      state = 'N'.
      consolidar=>get_dados( ).
      w_contrato-empresa = p_empre-low.
      w_contrato-safra = p_safra-low. "BUG - 79025 - CSB

      CALL SCREEN 0510.

    WHEN 'TCOTTON'.
      PERFORM envia_trace.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  REFRESH_PRECO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE refresh_preco INPUT.
  consolidar=>get_estrutura_preco( ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PBO_0510  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0510 OUTPUT.

  consolidar=>adm_botoes( ).

  consolidar=>verifica_erros( ).



  SET TITLEBAR 'TITLE0510'.
  SET PF-STATUS 'STATUS0510' EXCLUDING fcode.

*  LOOP AT t_fieldcatalog ASSIGNING FIELD-SYMBOL(<fcat>).
*    CASE <fcat>-fieldname.
*      WHEN 'POSNR1' OR
*           'VALDT' OR
*           'VALDT_HEDGE'.
*        <fcat>-no_out = abap_true.
*    ENDCASE.
*  ENDLOOP.
*
*  IF cc_2 IS NOT INITIAL.
*    CALL METHOD grid1->free.
*    CALL METHOD cc_2->free.
*    CLEAR: cc_2, grid1.
*  ENDIF.
*
*  CREATE OBJECT cc_2
*    EXPORTING
*      container_name              = 'CC'
*    EXCEPTIONS
*      cntl_error                  = 1
*      cntl_system_error           = 2
*      create_error                = 3
*      lifetime_error              = 4
*      lifetime_dynpro_dynpro_link = 5
*      OTHERS                      = 6.
*
*  CREATE OBJECT grid1
*    EXPORTING
*      i_parent = cc_2.
*
*  _layout =
*  VALUE #(
*           zebra      = abap_true
*           no_rowmark = abap_true
*           stylefname = 'STYLE'
*           info_fname = 'LINE_COLOR'
*           sel_mode = 'A'
*         ).
*
*  it_exclud = VALUE #( ( cl_gui_alv_grid=>mc_fc_excl_all ) ).
*
*  SET HANDLER: consolidar=>dt_changed_f_preco FOR grid1.
*
*  CALL METHOD grid1->set_table_for_first_display
*    EXPORTING
*      is_layout            = _layout
*      it_toolbar_excluding = it_exclud
*    CHANGING
*      it_outtab            = <fs_table>
*      it_fieldcatalog      = t_fieldcatalog.
*
*  CALL METHOD grid1->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*  CALL METHOD grid1->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_0510  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0510 INPUT.
  DATA: l_errors TYPE char1.

  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR state.
*      PERFORM redefine_objetos USING '2'.
      g_tab_strip-pressed_tab = c_tab_strip-tab1.
      tab_strip-activetab = space.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      IF state EQ 'N'.
        CLEAR w_contrato.

        FREE <fs_table>.
      ELSE.
        CLEAR state.
        consolidar=>get_dados( ).
      ENDIF.
    WHEN 'EDITAR'.
      IF w_contrato-sts_con NE 'E'.
        state = 'E'.
        edit = 'X'.
      ENDIF.
    WHEN 'SAVE'.
      consolidar=>save_dados( lv_acao ).  "*-CS2021000532-#84648-09.08.2022-JT
      IF t_msg[] IS INITIAL.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'ENCERRAR'.
      consolidar=>encerrar( ).
      consolidar=>get_dados( ).
    WHEN 'BPRT'.
      consolidar=>get_porto( ).
    WHEN 'DELETAR'.
      state = 'D'.
      consolidar=>deletar( ).
      consolidar=>get_dados( ).
      LEAVE TO SCREEN 0.
    WHEN 'MSG'.
      consolidar=>verifica_erros( ).
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SCREEN_0510  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_0510 OUTPUT.

  btn_enc = COND #( WHEN w_contrato-sts_con EQ 'E' THEN '@Q3@ Abrir' ELSE '@DF@ Encerrar' ).

*  CHECK ACAO IS NOT INITIAL.

  LOOP AT SCREEN.

    IF acao IS NOT INITIAL.
      CASE screen-name.
        WHEN 'W_CONTRATO-SAFRA' OR
             'W_CONTRATO-CONTRATO' OR
             'W_CONTRATO-TP_VENDA' OR
             'W_CONTRATO-DT_VENDA' OR
             'W_CONTRATO-CLIENTE' OR
             'W_CONTRATO-VENDEDOR' OR
             'W_CONTRATO-TIPO_PADRAO' OR
             'W_CONTRATO-CONTRATO_CLIENTE' OR
             'W_CONTRATO-COMISSAO' OR
             'W_CONTRATO-PRECO' OR
             'W_CONTRATO-CORRETOR' OR
             'W_CONTRATO-TOLERANCIA' OR
             'W_CONTRATO-PRECO_TONS' OR
             'W_CONTRATO-QUATIDADE' OR
             'W_CONTRATO-PCTGEM_ANT' OR
             'W_CONTRATO-DE_EMBARQUE' OR
             'W_CONTRATO-ATE_EMBARQUE' OR
             'W_CONTRATO-VISAO' OR
             'W_CONTRATO-INTERCOMPANY' OR
             'W_CONTRATO-ID_CONTRATO_REFERENCIA'.
          screen-input = 1.
          MODIFY SCREEN.

          "*-CS2023000189-04.04.2023-#108693-JT-inicio
        WHEN 'W_CONTRATO-ACTS'.
          IF w_contrato-id_contrato_referencia IS NOT INITIAL.
            SELECT SINGLE acts
                     INTO @DATA(l_acts)
                     FROM zsdt0143
                    WHERE "empresa     = @w_contrato-empresa and
                      id_contrato = @w_contrato-id_contrato_referencia.
            IF sy-subrc = 0.
              w_contrato-acts = l_acts.
            ENDIF.
            screen-input = 0.
          ELSE.
            screen-input = 1.
          ENDIF.
          MODIFY SCREEN.
          "*-CS2023000189-04.04.2023-#108693-JT-fim

      ENDCASE.
    ENDIF.

    IF screen-name = 'W_CONTRATO-VISAO'.
      screen-required = 1.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name EQ 'BTN_PORTO'.
      CASE state.
        WHEN 'N' OR 'E'.
          screen-input = 1.
        WHEN OTHERS.
          screen-input = 0.
      ENDCASE.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  IF w_contrato-visao IS INITIAL.
    w_contrato-visao = 'E'.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STYLE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE style OUTPUT.

  CHECK grid1 IS NOT INITIAL.

  acao = COND #( WHEN state EQ 'N' OR state EQ 'E' THEN 'X' ELSE '' ).

  DATA w_tipo_calc TYPE zsdt0059-tipo_calc.

  CALL METHOD grid1->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = t_fieldcatalog.

  LOOP AT t_fieldcatalog ASSIGNING FIELD-SYMBOL(<f_fieldcatalog>).

    IF acao IS NOT INITIAL .
      IF line_exists( t_fields[ field = <f_fieldcatalog>-fieldname ] ).
        <f_fieldcatalog>-edit = abap_true.
      ENDIF.
      CASE <f_fieldcatalog>-fieldname.
        WHEN 'WAERS' OR 'CBOT' OR 'MONAT' OR 'VALDT'.
          <f_fieldcatalog>-edit = abap_true.
      ENDCASE.
    ELSE.
      <f_fieldcatalog>-edit = space.
    ENDIF.

  ENDLOOP.

  CALL METHOD grid1->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = t_fieldcatalog.

  LOOP AT <fs_table> ASSIGNING <fs_line>.

    FREE style.
    CLEAR: w_valor, w_tipo_calc.

    ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs_line> TO <fs_campo>.
    <fs_campo> = style.

    consolidar=>get_valores(
      EXPORTING
        fieldname = 'TIPO_CALC'
      CHANGING
        vl_any    = w_tipo_calc ).

    w_style-fieldname = 'BEZEI'.
    CASE w_tipo_calc.
      WHEN 'C' OR 'R'.
        w_style-style  = alv_style_font_bold.
        w_style-style2 = alv_style2_no_border_left.
      WHEN OTHERS.
        w_style-style = alv_style_font_bold_no.
    ENDCASE.
    INSERT w_style INTO TABLE style.

    consolidar=>get_valores(
      EXPORTING
        fieldname = 'BEZEI'
      CHANGING
        vl_any    = w_valor ).

    w_style-fieldname = 'MONAT'.
    IF w_valor EQ 'PREMIO' OR w_valor EQ 'SPREAD'.
      w_style-style = COND #( WHEN acao IS INITIAL
                                THEN cl_gui_alv_grid=>mc_style_disabled
                                ELSE cl_gui_alv_grid=>mc_style_enabled ).
    ELSE.
      w_style-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.
    INSERT w_style INTO TABLE style .

    LOOP AT t_fields INTO DATA(_fields).
      CLEAR w_valor.

      w_style-fieldname = _fields-field.

      consolidar=>get_valores(
        EXPORTING
          fieldname = 'COD_FP'
        CHANGING
          vl_any    = w_valor ).

      READ TABLE t_preco INTO w_preco
        WITH KEY field = _fields-field
                 cod_fp = w_valor.

      IF w_tipo_calc NE 'C'
     AND w_tipo_calc NE 'F'
     AND w_tipo_calc NE 'R'
     AND sy-subrc IS INITIAL.
        w_style-style = COND #( WHEN acao IS INITIAL
                                        THEN cl_gui_alv_grid=>mc_style_disabled
                                        ELSE cl_gui_alv_grid=>mc_style_enabled ) + alv_style_font_bold_no .
      ELSE.
        IF w_tipo_calc EQ 'C'
        OR w_tipo_calc EQ 'R'.
          w_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_font_bold.
        ELSEIF w_tipo_calc EQ 'F'.
          w_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_font_bold_no.
        ELSE.
          w_style-style = COND #( WHEN acao IS INITIAL
                          THEN cl_gui_alv_grid=>mc_style_disabled
                          ELSE cl_gui_alv_grid=>mc_style_enabled ) + alv_style_font_bold_no .
        ENDIF.
      ENDIF.
      INSERT w_style INTO TABLE style.
    ENDLOOP.

    w_style-fieldname = 'WAERS'.
    CASE w_tipo_calc.
      WHEN 'C' OR 'R'.
        w_style-style = cl_gui_alv_grid=>mc_style_disabled.
      WHEN OTHERS.
        w_style-style = COND #( WHEN acao IS INITIAL
                                        THEN cl_gui_alv_grid=>mc_style_disabled
                                        ELSE cl_gui_alv_grid=>mc_style_enabled ).
    ENDCASE.
    INSERT w_style INTO TABLE style .

    w_style-fieldname = 'VALDT'.
    w_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_style INTO TABLE style .

    w_style-fieldname = 'CBOT'.
    CASE w_tipo_calc.
      WHEN 'C' OR 'R'.
        w_style-style = cl_gui_alv_grid=>mc_style_disabled.
      WHEN OTHERS.

        consolidar=>get_valores(
          EXPORTING
            fieldname = 'OCBOT'
          CHANGING
            vl_any    = w_valor ).

        IF w_valor EQ abap_true.
          w_style-style = COND #( WHEN acao IS INITIAL
                                          THEN cl_gui_alv_grid=>mc_style_disabled
                                          ELSE cl_gui_alv_grid=>mc_style_enabled ).
        ELSE.
          w_style-style = cl_gui_alv_grid=>mc_style_disabled.
        ENDIF.

    ENDCASE.

    INSERT w_style INTO TABLE style .

    ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs_line> TO <fs_campo>.

    <fs_campo> = style.
    CASE w_tipo_calc.
      WHEN 'C' OR 'R'.
        w_valor = 'C305'.

        consolidar=>set_valores(
          EXPORTING
            fieldname = 'LINE_COLOR'
          CHANGING
            vl_any    = w_valor ).

      WHEN OTHERS.
        CLEAR w_valor.

        consolidar=>set_valores(
          EXPORTING
            fieldname = 'LINE_COLOR'
          CHANGING
            vl_any    = w_valor ).
    ENDCASE.

  ENDLOOP.

  CALL METHOD grid1->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ATUALIZA_DADOS_CONTRATO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atualiza_dados_contrato INPUT.
  "CS2023000851 Algodão - PT1 - Atualização da transação ZSDT0118 - Formato já executado na Amaggi e mais detalhado --128361 - BG

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = w_contrato-tipo_padrao
    IMPORTING
      output       = vmatnr18
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.

*  w_contrato-tipo_padrao = |{ w_contrato-tipo_padrao ALPHA = IN }|.
  w_contrato-corretor      = |{ w_contrato-corretor       ALPHA = IN }|.
  w_contrato-cliente       = |{ w_contrato-cliente        ALPHA = IN }|.
  w_contrato-cliente_final = |{ w_contrato-cliente_final  ALPHA = IN }|.
  w_contrato-empresa       = |{ w_contrato-empresa        ALPHA = IN }|.
  w_contrato-vendedor      = |{ w_contrato-vendedor       ALPHA = IN }|.
  w_contrato-tp_venda      = |{ w_contrato-tp_venda       ALPHA = IN }|.

  SELECT SINGLE maktx FROM makt
    INTO @DATA(_desc)
    WHERE matnr EQ @vmatnr18.

  DATA(line) = strlen( _desc ).
  line = line - 4.
  IF line > 0.
    w_contrato-desc_tipo_padrao = _desc+line.
  ENDIF.

  SELECT SINGLE name1 FROM lfa1
    INTO w_contrato-desc_corretor
    WHERE lifnr EQ w_contrato-corretor.

*-CS2023000189-04.04.2023-#108693-JT-inicio
*  SELECT SINGLE name1 FROM kna1
*    INTO w_contrato-desc_cliente
*    WHERE kunnr EQ w_contrato-cliente.

  SELECT SINGLE adrnr FROM kna1
    INTO @DATA(l_adrnr)
    WHERE kunnr EQ @wa_novo-cliente.

  SELECT SINGLE *
    FROM adrc
    INTO @DATA(w_adrc)
   WHERE addrnumber EQ @l_adrnr
     AND date_to    >= @sy-datum.

  IF sy-subrc EQ 0.
    wa_desc-desc_cliente = w_adrc-name1 && w_adrc-name2.
  ELSE.
    SELECT SINGLE name1 FROM kna1
      INTO w_contrato-desc_cliente
      WHERE kunnr EQ w_contrato-cliente.
  ENDIF.
*-CS2023000189-04.04.2023-#108693-JT-fim

  SELECT SINGLE bezei
    FROM tvgrt
    INTO w_contrato-desc_vendedor
    WHERE vkgrp EQ w_contrato-vendedor
      AND spras EQ sy-langu.

  SELECT SINGLE butxt
    FROM t001
    INTO w_contrato-desc_empresa
    WHERE bukrs EQ w_contrato-empresa.

  SELECT SINGLE name1 FROM kna1
    INTO w_contrato-desc_cliente_final
    WHERE kunnr EQ w_contrato-cliente_final.

  SELECT SINGLE bezei FROM zsdt0057
    INTO w_contrato-desc_tp_venda
    WHERE tp_venda EQ w_contrato-tp_venda.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_VENDA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_venda INPUT.

  TYPES: BEGIN OF ty_0057,
           tp_venda TYPE zsded012,
           bezei    TYPE bezei30,
           auart    TYPE auart,
         END OF ty_0057.

  DATA: it_0057   TYPE TABLE OF ty_0057,
        it_return TYPE TABLE OF ddshretval,
        wa_return LIKE LINE OF it_return.

  FREE it_0057.

  SELECT tp_venda bezei auart
    FROM zsdt0057
     INTO TABLE it_0057
     WHERE status EQ abap_true
       AND param_espec IN ('A', 'Z', 'X').

  CHECK it_0057 IS NOT INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'TP_VENDA'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'W_CONTRATO-TP_VENDA'
      value_org   = 'S'
    TABLES
      value_tab   = it_0057.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_VENDEDOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_vendedor INPUT.

  TYPES: BEGIN OF tl_tvgrt,
           vkgrp TYPE tvgrt-vkgrp,
           bezei TYPE tvgrt-bezei,
         END OF tl_tvgrt.

  DATA: t_tab   TYPE TABLE OF ddshretval,
        t_dselc TYPE TABLE OF dselc,
        t_tvgrt TYPE TABLE OF tl_tvgrt.

  REFRESH: t_tvgrt, t_tab, t_dselc.

  SELECT vkgrp bezei
    FROM tvgrt
    INTO TABLE t_tvgrt
     WHERE spras EQ sy-langu.

  CHECK t_tvgrt IS NOT INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'VKGRP'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'W_CONTRATO-VENDEDOR'
      value_org   = 'S'
    TABLES
      value_tab   = t_tvgrt.

ENDMODULE.

FORM f_integracao_trace_cotton USING s_metodo
                                     s_table TYPE any  CHANGING s_return.

  DATA: "id_referencia   TYPE char11,
    id_referencia   TYPE string,
    w_dados         TYPE ty_contratos,
    w_del_contratos TYPE ty_del_contratos.

  CLEAR: v_url, wa_zauth_webservice.

  MOVE-CORRESPONDING s_table TO w_dados.

* Novo Desenvolvimento - BUG - 65565 - Inicio
  CALL METHOD /ui2/cl_json=>serialize
    EXPORTING
      data   = s_table
    RECEIVING
      r_json = e_json.

  IF s_metodo EQ 'DELETE'.
    MOVE-CORRESPONDING s_table TO w_del_contratos.
    id_referencia = w_del_contratos-referenciaintegracao.
  ELSE.
    id_referencia = w_dados-referenciaintegracaocontrato.
  ENDIF.

  CLEAR: s_return.
  TRY .
      zcl_integracao_tcot_contratos=>zif_integracao_tcot_contratos~get_instance(
        )->set_int_contratos( i_json = e_json i_metodo = s_metodo i_id_referencia = id_referencia   ).
      s_return      = 200.
    CATCH zcx_integracao INTO DATA(ex_integra).
      ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    CATCH zcx_error INTO DATA(ex_error).    "  "
      ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.

* Novo Desenvolvimento - BUG - 65565 - Fim

* Códifo Original - BUG - 65565 - Inicio
*  SELECT SINGLE * FROM zauth_webservice INTO wa_zauth_webservice
*    WHERE service EQ 'TRACE_COTTON_CONTRATOS'.
*
*  CASE s_metodo.
*    WHEN 'POST'.
*      CONCATENATE wa_zauth_webservice-url 'incluirContrato'  INTO v_url.
*    WHEN 'PUT'.
*      CONCATENATE wa_zauth_webservice-url 'atualizarContrato'  INTO v_url.
*    WHEN 'DELETE'.
*      CONCATENATE wa_zauth_webservice-url 'excluirContrato'  INTO v_url.
*      MOVE-CORRESPONDING s_table TO w_del_contratos.
*  ENDCASE.
*
*
*  cl_http_client=>create_by_url(
*      EXPORTING url   =  |{ v_url }|
*      IMPORTING client = e_http
*      EXCEPTIONS argument_not_found = 1
*                 plugin_not_active  = 2
*                 internal_error     = 3 ).
*
*
*  CALL METHOD e_http->request->set_header_field
*    EXPORTING
*      name  = '~request_method'
*      value = s_metodo.
*
*  CALL METHOD e_http->request->set_header_field
*    EXPORTING
*      name  = '~server_protocol'
*      value = 'HTTP/1.1'.
*
*  CALL METHOD e_http->request->set_header_field
*    EXPORTING
*      name  = 'Content-Type'
*      value = 'application/json; charset=UTF-8'.
*
*
*  ob_web_token->zif_webservice~autentica_api_ad = abap_true.
*
*  CALL METHOD /ui2/cl_json=>serialize
*    EXPORTING
*      data   = s_table
*    RECEIVING
*      r_json = e_json.
*
*  ob_web_token->zif_webservice~consultar(
*  EXPORTING
*    i_http                     = e_http
*    i_xml                      = e_json
*    i_not_content_length       = abap_true
*  IMPORTING
*    e_reason                   = DATA(e_reason)
*  RECEIVING
*    e_resultado                = r_json
*  EXCEPTIONS
*    http_communication_failure = 1
*    http_invalid_state         = 2
*    http_processing_failed     = 3
*    http_invalid_timeout       = 4
*    OTHERS                     = 5 ).
*
*  IF e_reason EQ 'OK'.
*    s_return      = 200.
*  ELSE.
*    /ui2/cl_json=>deserialize( EXPORTING json = r_json CHANGING data = wa_return_api ).
*    s_return = wa_return_api-status.
*  ENDIF.
*
*  IF s_metodo EQ 'DELETE'.
*    id_referencia = w_del_contratos-referenciaintegracao.
*  ELSE.
*    id_referencia = w_dados-referenciaintegracaocontrato.
*  ENDIF.
*
*  PERFORM f_inclui_integracao USING id_referencia s_metodo e_json v_url s_return r_json. "CHANGING p_id_integracao.
* Códifo Original - BUG - 65565 - Fim
ENDFORM.


FORM f_preenche_dados_api USING v_dados  TYPE any
                                v_metodo.

  DATA: vl_safra(2) TYPE c.
  DATA s_dados TYPE ty_saida.

  CLEAR: vl_safra, wa_contratos, wa_del_contratos.

  MOVE-CORRESPONDING v_dados TO s_dados.

  CASE v_metodo.
    WHEN 'POST' OR 'PUT'.

      wa_contratos-referenciaintegracaocontrato = s_dados-id_contrato.
      wa_contratos-referenciaintegracaoempresa  = s_dados-empresa.
      wa_contratos-referenciaintegracaocliente  = s_dados-cliente.
      wa_contratos-contrato                     = s_dados-contrato.
      wa_contratos-codigoacts                   = s_dados-acts.  "*-CS2023000189-04.04.2023-#108693-JT

      SELECT SINGLE * FROM kna1 INTO @DATA(wa_kna1)
        WHERE kunnr EQ @s_dados-cliente.

      IF sy-subrc EQ 0.
        wa_contratos-nomecliente = wa_kna1-name1.

        IF wa_kna1-stcd1 IS NOT INITIAL.
          wa_contratos-cnpj = wa_kna1-stcd1.
        ELSE.
          wa_contratos-cnpj = wa_kna1-stcd2.
        ENDIF.
      ENDIF.

      SELECT SINGLE * FROM mara INTO @DATA(wa_mara)
        WHERE matnr EQ @s_dados-tipo_padrao.

      IF sy-subrc EQ 0.
        wa_contratos-classificacao = wa_mara-normt.
      ENDIF.

      vl_safra = s_dados-safra+2(2) - 1.
      CONCATENATE vl_safra '/' s_dados-safra+2(2) INTO  wa_contratos-safra.
      CONCATENATE s_dados-de_embarque+6(2)  '/' s_dados-de_embarque+4(2)  '/'  s_dados-de_embarque(4) INTO  wa_contratos-datainicial.
      CONCATENATE s_dados-ate_embarque+6(2) '/' s_dados-ate_embarque+4(2) '/'  s_dados-ate_embarque(4) INTO  wa_contratos-datafinal.
      wa_contratos-quantidade = s_dados-quatidade / 1000.

    WHEN 'DELETE'.
      wa_del_contratos-contrato             = s_dados-contrato.
      wa_del_contratos-referenciaintegracao = s_dados-id_contrato.
*     vl_safra = s_dados-safra(2) - 1.
*     CONCATENATE vl_safra '/' s_dados-safra(2) INTO  wa_del_contratos-safra.
      vl_safra = s_dados-safra+2(2) - 1.
      CONCATENATE vl_safra '/' s_dados-safra+2(2) INTO  wa_del_contratos-safra.
  ENDCASE.

ENDFORM.


FORM f_inclui_integracao USING p_id_referencia p_metodo p_json p_url p_return p_data_retorno. "CHANGING p_id_integracao.

  DATA: l_id     TYPE zde_id_integracao.

  CLEAR: w_integracao,
         w_integracao_log.


*--------------------------------------
* sequencia id integracao
*--------------------------------------
  zcl_integracao=>zif_integracao~get_instance( )->get_new_id_integracao(
      IMPORTING e_id_integracao = l_id ).

*--------------------------------------
* cria header
*--------------------------------------
  CLEAR w_integracao.

  w_integracao-mandt                = sy-mandt.
  w_integracao-id_integracao        = l_id.
  w_integracao-id_interface         = '040'.
  w_integracao-id_referencia        = p_id_referencia.
  w_integracao-dt_registro          = sy-datum.
  w_integracao-hr_registro          = sy-uzeit.
  w_integracao-us_registro          = sy-uname.
  w_integracao-ck_integrado         = abap_true.
  w_integracao-dt_integrado         = sy-datum.
  w_integracao-hr_integrado         = sy-uzeit.
  w_integracao-us_integrado         = sy-uname.
  w_integracao-ds_url               = p_url.
  w_integracao-ds_body              = p_json.
  w_integracao-ds_metodo            = p_metodo.
  w_integracao-ds_content_type      = 'application/json; charset=UTF-8'.
  w_integracao-ds_formato           = 'JSON'.
  w_integracao-ds_funcao_processa   = '/login'.

  CONCATENATE  '[#{#"name":"~request_uri",#"value":"/login"#}#,#{#"name":"~request_method",#"value":"POST"#}#,#'
               '{#"name":"~server_protocol",#"value":"HTTP/1.1"#}#,#{#"name":"content-type",#"value":"application/json; charset=UTF-8"#}#,'
               '#"name":"content-length",#"value":"67 "#}##]#'
         INTO w_integracao-ds_header.

  w_integracao-nm_code              = p_return.
  w_integracao-ds_data_retorno      = p_data_retorno.
  w_integracao-tp_integracao        = '0'.
  w_integracao-tp_sincronia         = '1'.
  w_integracao-ck_retornou          = abap_true.
  w_integracao-ck_processado        = abap_true.
  w_integracao-dt_processado        = sy-datum.
  w_integracao-hr_processado        = sy-uzeit.
  w_integracao-us_processado        = sy-uname.
  MODIFY zintegracao             FROM w_integracao.
  COMMIT WORK.

*--------------------------------------
* cria header LOG
*--------------------------------------
  CLEAR w_integracao_log.

  w_integracao_log-mandt            = sy-mandt.
  w_integracao_log-id_integracao    = l_id.
  w_integracao_log-dt_registro      = sy-datum.
  w_integracao_log-hr_registro      = sy-uzeit.
  w_integracao_log-us_registro      = sy-uname.
  w_integracao_log-nm_code          = p_return.
  w_integracao_log-ds_data_retorno  = COND #( WHEN p_return EQ '200' THEN 'Sucesso' ELSE p_data_retorno ) .
  w_integracao_log-dt_resposta      = sy-datum.
  w_integracao_log-hr_resposta      = sy-uzeit.
  w_integracao_log-us_resposta      = sy-uname.
  MODIFY zintegracao_log         FROM w_integracao_log.
  COMMIT WORK.


  CALL FUNCTION 'ZDENQUEUE_INTEGRACAO'
    EXPORTING
      id_integracao = l_id.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  CREATE_DROPDOWN_BOX  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_dropdown_box INPUT.
  dynpro_utilities=>value_help( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_ID_CONT_REF  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_id_cont_ref INPUT.

*ID Contrato Produtor / Contrato / Data da venda / Cliente / Tipo Padrão.


  DATA: t_id_cont_ref TYPE TABLE OF zsdt0143.

  CLEAR: t_id_cont_ref.
  SELECT *
    FROM  zsdt0143
    INTO TABLE t_id_cont_ref
     WHERE visao = 'P'
     AND intercompany = 'X'.

  CLEAR: it_field_tab.
  PERFORM get_info_field USING: 'ZSDT0143' 'ID_CONTRATO' wa_dfies.
  wa_dfies-position  = 1.
  APPEND wa_dfies TO it_field_tab.

  PERFORM get_info_field USING: 'ZSDT0143' 'CONTRATO' wa_dfies.
  wa_dfies-position  = 2.
  APPEND wa_dfies TO it_field_tab.

  PERFORM get_info_field USING: 'ZSDT0143' 'DT_VENDA' wa_dfies.
  wa_dfies-position  = 3.
  APPEND wa_dfies TO it_field_tab.

  PERFORM get_info_field USING: 'ZSDT0143' 'CLIENTE' wa_dfies.
  wa_dfies-position  = 4.
  APPEND wa_dfies TO it_field_tab.

  PERFORM get_info_field USING: 'ZSDT0143' 'TIPO_PADRAO' wa_dfies.
  wa_dfies-position  = 5.
  APPEND wa_dfies TO it_field_tab.

*-CS2023000189-04.04.2023-#108693-JT-inicio
  PERFORM get_info_field USING: 'ZSDT0143' 'EMPRESA' wa_dfies.
  wa_dfies-position  = 6.
  APPEND wa_dfies TO it_field_tab.
*-CS2023000189-04.04.2023-#108693-JT-fim


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ID_CONTRATO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'WA_NOVO-ID_CONTRATO_REFERENCIA'
      value_org       = 'S'
    TABLES
      value_tab       = t_id_cont_ref
      field_tab       = it_field_tab
      return_tab      = it_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc IS INITIAL.
    TRY .
        wa_novo-id_contrato_referencia = it_return[ 1 ]-fieldval.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDIF.

  IF wa_novo-id_contrato_referencia IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_novo-id_contrato_referencia
      IMPORTING
        output = wa_novo-id_contrato_referencia.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_novo-id_contrato_referencia
      IMPORTING
        output = gva_id_contrato_referencia.

    SELECT *
    FROM zsdt0143 INTO TABLE @DATA(lit_zsdt0143)
       WHERE visao = 'P'
    AND intercompany = 'X'
      AND id_contrato = @gva_id_contrato_referencia.

    READ TABLE lit_zsdt0143 INTO DATA(lwa_zsdt0143) INDEX 1.
    IF sy-subrc = 0.
* preencher automaticamente
      wa_novo-safra      = lwa_zsdt0143-safra.
      wa_novo-contrato   = lwa_zsdt0143-contrato.
      wa_novo-dt_venda   = lwa_zsdt0143-dt_venda.
      wa_novo-quatidade  = lwa_zsdt0143-quatidade.
*     wa_novo-empresa    = lwa_zsdt0143-empresa.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
        EXPORTING
          input  = lwa_zsdt0143-tipo_padrao
        IMPORTING
          output = vmatnr18. "lwa_zsdt0143-tipo_padrao.
      lwa_zsdt0143-tipo_padrao = vmatnr18.

*      SELECT SINGLE matnr
*        INTO @DATA(lva_matnr)
*        FROM  mara
*        WHERE bismt = @lwa_zsdt0143-tipo_padrao.

      SELECT *
        INTO @DATA(w_cabn)
        FROM cabn
          UP TO 1 ROWS
        WHERE atnam = 'COD_ALGODAO_SIMILAR'.
      ENDSELECT.
      IF sy-subrc <> 0.
        CLEAR w_cabn.
      ENDIF.

      SELECT *
        INTO @DATA(w_ausp)
        FROM ausp
          UP TO 1 ROWS
        WHERE atinn = @w_cabn-atinn
          AND klart = '001'
          AND atwrt = @lwa_zsdt0143-tipo_padrao.
      ENDSELECT.

      IF sy-subrc = 0 AND lwa_zsdt0143-tipo_padrao IS NOT INITIAL.
        wa_novo-tipo_padrao = w_ausp-objek. "lva_matnr.
      ELSE.
        CLEAR: wa_novo-tipo_padrao.
      ENDIF.
    ELSE.
*      CLEAR: wa_novo-safra, wa_novo-contrato, wa_novo-dt_venda,
*             wa_novo-quatidade, wa_novo-tipo_padrao.
    ENDIF.
  ELSE.
*    CLEAR: wa_novo-safra, wa_novo-contrato, wa_novo-dt_venda,
*           wa_novo-quatidade, wa_novo-tipo_padrao.
  ENDIF.

  PERFORM f_atualiza_dados.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE trata_fields OUTPUT.

  IF wa_novo-visao = 'P'.
*** Verificar grupo de contas.
    IF wa_novo-cliente IS NOT INITIAL.
      SELECT SINGLE ktokd
        INTO @DATA(lva_ktokd)
        FROM  kna1
        WHERE kunnr = @wa_novo-cliente.

      IF lva_ktokd = 'ZCIC'.
        wa_novo-intercompany = 'X'.
      ELSE.
        wa_novo-intercompany = ''.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDIF.

*-CS2023000189-04.04.2023-#108693-JT-inicio
  LOOP AT SCREEN.
    IF screen-name = 'WA_NOVO-ACTS'.
      IF wa_novo-id_contrato_referencia IS NOT INITIAL.
        SELECT SINGLE acts
                 INTO @DATA(l_acts2)
                 FROM zsdt0143
                WHERE "empresa     = @wa_novo-empresa and
                      id_contrato = @wa_novo-id_contrato_referencia.
        IF sy-subrc = 0.
          wa_novo-acts = l_acts2.
        ENDIF.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
*-CS2023000189-04.04.2023-#108693-JT-fim

  LOOP AT SCREEN.
    IF acao IS INITIAL.
      screen-input = 0.
      MODIFY SCREEN.
    ELSE.
      IF screen-name EQ 'WA_NOVO-VISAO'.
        screen-required = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ 'WA_NOVO-ID_CONTRATO_REFERENCIA'.
        IF wa_novo-visao = 'P'.
          screen-input     = 0.
          CLEAR: wa_novo-id_contrato_referencia.
          MODIFY SCREEN.
        ELSE.
          IF wa_novo-intercompany = 'X' AND wa_novo-visao = 'E'.
            screen-input     = 1.
            MODIFY SCREEN.
          ELSE.
            IF wa_novo-intercompany = '' AND wa_novo-visao = 'E'.
              screen-input     = 0.
              screen-required = 0.
              CLEAR: wa_novo-id_contrato_referencia.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF screen-name EQ 'WA_NOVO-INTERCOMPANY'.
        IF lva_ktokd = 'ZCIC' AND wa_novo-visao = 'P'.
          screen-input     = 0.
          MODIFY SCREEN.
        ELSE.
          IF wa_novo-visao = 'E'." OR lva_ktokd <> 'ZCIC'.
            screen-input     = 1.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDMODULE.

MODULE trata_fields_input INPUT.

  IF wa_novo-id_contrato_referencia <> v_id_contr_ant.
    IF wa_novo-id_contrato_referencia IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_novo-id_contrato_referencia
        IMPORTING
          output = wa_novo-id_contrato_referencia.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_novo-id_contrato_referencia
        IMPORTING
          output = gva_id_contrato_referencia.

      SELECT *
        FROM zsdt0143
        INTO TABLE @DATA(t_zsdt0143)
       WHERE visao        = 'P'
         AND intercompany = 'X'
         AND id_contrato  = @gva_id_contrato_referencia.

      READ TABLE t_zsdt0143 INTO DATA(w_zsdt0143) INDEX 1.

      IF sy-subrc = 0.
        wa_novo-safra      = w_zsdt0143-safra.
        wa_novo-contrato   = w_zsdt0143-contrato.
        wa_novo-dt_venda   = w_zsdt0143-dt_venda.
        wa_novo-quatidade  = w_zsdt0143-quatidade.
*     wa_novo-empresa    = w_zsdt0143-empresa.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input  = w_zsdt0143-tipo_padrao
          IMPORTING
            output = vmatnr18. "w_zsdt0143-tipo_padrao.
        w_zsdt0143-tipo_padrao = vmatnr18.

*      SELECT SINGLE matnr
*        INTO @DATA(l_matnr)
*        FROM  mara
*        WHERE bismt = @w_zsdt0143-tipo_padrao.

        SELECT *
          INTO w_cabn
          FROM cabn
            UP TO 1 ROWS
          WHERE atnam = 'COD_ALGODAO_SIMILAR'.
        ENDSELECT.
        IF sy-subrc <> 0.
          CLEAR w_cabn.
        ENDIF.

        SELECT *
          INTO w_ausp
          FROM ausp
            UP TO 1 ROWS
          WHERE atinn = w_cabn-atinn
            AND klart = '001'
            AND atwrt = w_zsdt0143-tipo_padrao.
        ENDSELECT.

        IF sy-subrc = 0 AND w_zsdt0143-tipo_padrao IS NOT INITIAL.
          wa_novo-tipo_padrao = w_ausp-objek.  "l_matnr.
        ELSE.
          CLEAR: wa_novo-tipo_padrao.
        ENDIF.
      ELSE.
*      CLEAR: wa_novo-safra, wa_novo-contrato, wa_novo-dt_venda,
*             wa_novo-quatidade, wa_novo-tipo_padrao.
      ENDIF.
    ELSE.
*    CLEAR: wa_novo-safra, wa_novo-contrato, wa_novo-dt_venda,
*           wa_novo-quatidade, wa_novo-tipo_padrao.
    ENDIF.
  ENDIF.

  PERFORM f_atualiza_dados.

  v_id_contr_ant = wa_novo-id_contrato_referencia.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200_exit INPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS_01  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE trata_fields_01 OUTPUT.
  CLEAR: lva_ktokd.
  IF w_contrato-visao = 'P'.
*** Verificar grupo de contas.
    IF w_contrato-cliente IS NOT INITIAL.
      SELECT SINGLE ktokd
        INTO lva_ktokd
        FROM  kna1
        WHERE kunnr = w_contrato-cliente.

      IF lva_ktokd = 'ZCIC'.
        w_contrato-intercompany = 'X'.
      ELSE.
        w_contrato-intercompany = ''.
      ENDIF.
*     MODIFY SCREEN.
    ENDIF.
  ENDIF.

  LOOP AT SCREEN.
    IF acao IS INITIAL.
      screen-input = 0.
      MODIFY SCREEN.
    ELSE.
      IF screen-name EQ 'W_CONTRATO-ID_CONTRATO_REFERENCIA'.
        IF w_contrato-visao = 'P'.
          screen-input     = 0.
          CLEAR: w_contrato-id_contrato_referencia.
          MODIFY SCREEN.
        ELSE.
          IF w_contrato-intercompany = 'X' AND w_contrato-visao = 'E'.
            screen-input     = 1.
            MODIFY SCREEN.
          ELSE.
            IF w_contrato-intercompany = '' AND w_contrato-visao = 'E'.
              screen-input    = 0.
              screen-required = 0.
              CLEAR: w_contrato-id_contrato_referencia.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF screen-name EQ 'W_CONTRATO-CLIENTE_FINAL'.
        IF w_contrato-visao EQ 'P' AND w_contrato-intercompany EQ abap_true.
          screen-input = 1.
        ELSE.
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name EQ 'W_CONTRATO-INTERCOMPANY'.
        IF lva_ktokd = 'ZCIC' AND w_contrato-visao = 'P'.
          screen-input     = 0.
          MODIFY SCREEN.
        ELSE.
          IF w_contrato-visao = 'E'." OR lva_ktokd <> 'ZCIC'.
            screen-input     = 1.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_ID_CONT_REF_01  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_id_cont_ref_01 INPUT.
*ID Contrato Produtor / Contrato / Data da venda / Cliente / Tipo Padrão.

  CLEAR: it_return, "lva_matnr,
         wa_dfies, it_field_tab.

  DATA: t_id_cont_ref_f TYPE TABLE OF zsdt0143.


  SELECT *
    FROM  zsdt0143
    INTO TABLE t_id_cont_ref_f
     WHERE visao = 'P'
     AND intercompany = 'X'.


  CLEAR: it_field_tab.
  PERFORM get_info_field USING: 'ZSDT0143' 'ID_CONTRATO' wa_dfies.
  wa_dfies-position  = 1.
  APPEND wa_dfies TO it_field_tab.

  PERFORM get_info_field USING: 'ZSDT0143' 'CONTRATO' wa_dfies.
  wa_dfies-position  = 2.
  APPEND wa_dfies TO it_field_tab.

  PERFORM get_info_field USING: 'ZSDT0143' 'DT_VENDA' wa_dfies.
  wa_dfies-position  = 3.
  APPEND wa_dfies TO it_field_tab.

  PERFORM get_info_field USING: 'ZSDT0143' 'CLIENTE' wa_dfies.
  wa_dfies-position  = 4.
  APPEND wa_dfies TO it_field_tab.

  PERFORM get_info_field USING: 'ZSDT0143' 'TIPO_PADRAO' wa_dfies.
  wa_dfies-position  = 5.
  APPEND wa_dfies TO it_field_tab.

*-CS2023000189-04.04.2023-#108693-JT-inicio
  PERFORM get_info_field USING: 'ZSDT0143' 'EMPRESA' wa_dfies.
  wa_dfies-position  = 6.
  APPEND wa_dfies TO it_field_tab.
*-CS2023000189-04.04.2023-#108693-JT-inicio

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ID_CONTRATO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'W_CONTRATO-ID_CONTRATO_REFERENCIA'
      value_org       = 'S'
    TABLES
      value_tab       = t_id_cont_ref_f
      return_tab      = it_return
      field_tab       = it_field_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc IS INITIAL.
    TRY .
        w_contrato-id_contrato_referencia = it_return[ 1 ]-fieldval.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDIF.

  IF w_contrato-id_contrato_referencia IS NOT INITIAL.
    CLEAR: lit_zsdt0143, gva_id_contrato_referencia.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_contrato-id_contrato_referencia
      IMPORTING
        output = gva_id_contrato_referencia.

    SELECT *
    FROM zsdt0143 INTO TABLE lit_zsdt0143
       WHERE visao = 'P'
    AND intercompany = 'X'
      AND id_contrato = gva_id_contrato_referencia.

* preencher automaticamente
    CLEAR: lwa_zsdt0143.
    READ TABLE lit_zsdt0143 INTO lwa_zsdt0143 INDEX 1.

    IF sy-subrc = 0.
      w_contrato-safra     = lwa_zsdt0143-safra.
      w_contrato-contrato  = lwa_zsdt0143-contrato.
      w_contrato-dt_venda  = lwa_zsdt0143-dt_venda.
      w_contrato-quatidade = lwa_zsdt0143-quatidade.

*      SELECT SINGLE matnr
*        INTO lva_matnr
*        FROM  mara
*        WHERE bismt = lwa_zsdt0143-tipo_padrao.

      SELECT *
        INTO w_cabn
        FROM cabn
          UP TO 1 ROWS
        WHERE atnam = 'COD_ALGODAO_SIMILAR'.
      ENDSELECT.
      IF sy-subrc <> 0.
        CLEAR w_cabn.
      ENDIF.

      SELECT *
        INTO w_ausp
        FROM ausp
          UP TO 1 ROWS
        WHERE atinn = w_cabn-atinn
          AND klart = '001'
          AND atwrt = lwa_zsdt0143-tipo_padrao.
      ENDSELECT.

      IF sy-subrc = 0 AND lwa_zsdt0143-tipo_padrao IS NOT INITIAL..
        w_contrato-tipo_padrao = w_ausp-objek. "lva_matnr.
      ELSE.
        CLEAR: w_contrato-tipo_padrao.
      ENDIF.
    ELSE.
      CLEAR: w_contrato-safra, w_contrato-contrato,
             w_contrato-dt_venda,  w_contrato-quatidade,
             w_contrato-tipo_padrao.
    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CREATE_DROPDOWN_BOX_01  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_dropdown_box_01 INPUT.
  dynpro_utilities=>value_help_01( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  GET_INFO_FIELD
*&---------------------------------------------------------------------*
FORM get_info_field USING p_tabname   TYPE ddobjname
                          p_fieldname TYPE dfies-fieldname
                          p_info      TYPE dfies.

  DATA: dfies_tab TYPE STANDARD TABLE OF dfies.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname   = p_tabname
      fieldname = p_fieldname
    TABLES
      dfies_tab = dfies_tab.

  READ TABLE dfies_tab INTO p_info INDEX 1.

  CASE p_fieldname.

    WHEN 'ID_CONTRATO'.
      p_info-reptext   = 'Id. Contrato'.
      p_info-scrtext_s = 'Id. Contrato'.
      p_info-scrtext_m = 'Id. Contrato'.
      p_info-scrtext_l = 'Id. Contrato'.

    WHEN 'CONTRATO'.
      p_info-reptext   = 'Contrato'.
      p_info-scrtext_s = 'Contrato'.
      p_info-scrtext_m = 'Contrato'.
      p_info-scrtext_l = 'Contrato'.

    WHEN 'DT_VENDA'.
      p_info-reptext   = 'Dt.Venda'.
      p_info-scrtext_s = 'Dt.Venda'.
      p_info-scrtext_m = 'Dt.Venda'.
      p_info-scrtext_l = 'Dt.Venda'.

    WHEN 'CLIENTE'.
      p_info-reptext   = 'Cliente'.
      p_info-scrtext_s = 'Cliente'.
      p_info-scrtext_m = 'Cliente'.
      p_info-scrtext_l = 'Cliente'.


    WHEN 'TIPO_PADRAO'.
      p_info-reptext   = 'Tipo Padrão'.
      p_info-scrtext_s = 'Tipo Padrão'.
      p_info-scrtext_m = 'Tipo Padrão'.
      p_info-scrtext_l = 'Tipo Padrão'.

  ENDCASE.
ENDFORM.

**********************************************
* log modificacoes ACTS
**********************************************
FORM f_log_modif_acts  USING p_id.

  DATA : ls_variant   TYPE disvariant,
         t_0326       TYPE TABLE OF zsdt0326,
         w_0326       TYPE TABLE OF zsdt0326,
         l_grid_title TYPE lvc_title.

  FREE: it_fieldcat_comp.

  SELECT *
    INTO TABLE t_0326
    FROM zsdt0326
   WHERE id_contrato = p_id
     AND field       = 'ACTS'.

  IF t_0326[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Não houveram modificações no ACTS.' DISPLAY LIKE 'S'.
    EXIT.
  ENDIF.

  PERFORM preenche_cat_comp USING :
   '01' 'ZSDT0143'  'ID_CONTRATO' 'T_0326' 'ID_CONTRATO'            'Id Contrato'            '12'     ''    ''     ''    '' '',
   '02' ''          ''            'T_0326' 'FIELD'                  'Campo'                  '05'     ''    ''     ''    '' '',
   '03' ''          ''            'T_0326' 'VALUE_OLD'              'Valor Antigo'           '13'     ''    ''     ''    '' 'X',
   '04' ''          ''            'T_0326' 'VALUE_NEW'              'Valor Atual'            '13'     ''    ''     ''    '' 'X',
   '05' ''          ''            'T_0326' 'US_DATA'                'Data'                   '13'     ''    ''     ''    '' '',
   '06' ''          ''            'T_0326' 'US_HORA'                'Hora'                   '13'     ''    ''     ''    '' '',
   '07' ''          ''            'T_0326' 'US_USER'                'Usuário'                '25'     ''    ''     ''    '' ''.

* ls_variant-report = 'ZIMP60' && 'REAL'.
  l_grid_title = 'Log de Modificações'.

  SORT t_0326 BY us_data DESCENDING
                 us_hora DESCENDING.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = sy-repid
      it_fieldcat           = it_fieldcat_comp[]
*     it_sort               = t_sort[]
*     i_callback_user_command = 'USER_COMMAND_COMPRO'
      i_grid_title          = l_grid_title
      i_save                = 'X'
*     is_variant            = ls_variant
      i_screen_start_column = 35
      i_screen_start_line   = 08
      i_screen_end_column   = 140
      i_screen_end_line     = 18
    TABLES
      t_outtab              = t_0326.

ENDFORM.

FORM preenche_cat_comp USING  VALUE(p_col_pos)       TYPE i
                              VALUE(p_ref_tabname)   LIKE dd02d-tabname
                              VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                              VALUE(p_tabname)       LIKE dd02d-tabname
                              VALUE(p_field)         LIKE dd03d-fieldname
                              VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                              VALUE(p_outputlen)
                              VALUE(p_edit)
                              VALUE(p_do_sum)
                              VALUE(p_just)
                              VALUE(p_hotspot)
                              VALUE(p_checkbox).

  CLEAR: wa_estrutura.

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
  wa_estrutura-do_sum        = p_do_sum.
  wa_estrutura-just          = p_just.
  wa_estrutura-hotspot       = p_hotspot.
  wa_estrutura-checkbox      = p_checkbox.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  wa_estrutura-outputlen = p_outputlen.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO it_fieldcat_comp.

ENDFORM.
"CS2023000851 Algodão - PT1 - Atualização da transação ZSDT0118 - Formato já executado na Amaggi e mais detalhado --128361 - BG
*&---------------------------------------------------------------------*
*& Form CHAMA_ZSDT0062
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM chama_zsdt0062 .

  DATA: wl_0060 TYPE zsdt0060,
        v_data  TYPE char10.
  CLEAR: it_sel_rows[].

  CALL METHOD grid->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  IF it_sel_rows[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Selecione uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF lines( it_sel_rows[] ) > 1.
    MESSAGE s024(sd) WITH 'Selecione apenas uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE it_sel_rows   INTO wa_sel_rows INDEX 1.

  READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX wa_sel_rows-index.
  "READ TABLE t_alv_saida ASSIGNING <saida> INDEX wa_sel_rows-index.

  DATA wl_mode TYPE c.
  wl_mode = 'E'.
  CONCATENATE sy-datum+6(2)'.' sy-datum+4(2) '.' sy-datum(4) INTO v_data.
  SELECT SINGLE *
    FROM zsdt0060
    INTO wl_0060
      WHERE usnam EQ sy-uname
       AND  programa EQ 'ZSDR0022'.



  PERFORM f_bdc_data USING:
  'ZSDR0022'  '0050'  'X' ''                         ' ',
  ''          ''      ''  'BDC_OKCODE'               '=ADD',
  ''          ''      ''  'BDC_CURSOR'               'WG_HEADER-NRO_SOL_OV',
  ''          ''      ''  'BDC_SUBSCR'               'ZSDR0022                                0052SUB01',
  ''          ''      ''  'BDC_SUBSCR'               'ZSDR0022                                0101TAB_STRIP_SCA',
  'ZSDR0022'  '0050'  'X' ''                         ' ',
  "''          ''      ''  'BDC_OKCODE'               '=SEARCH',
  ''          ''      ''  'BDC_CURSOR'               'WG_HEADER-TP_VENDA',
  ''          ''      ''  'WG_HEADER-DATA_VENDA'     v_data,
  ''          ''      ''  'WG_HEADER-TP_VENDA'       <fs_saida>-tp_venda ,
  ''          ''      ''  'WG_HEADER-VKORG'          <fs_saida>-empresa,
  ''          ''      ''  'WG_HEADER-RISCO_SACADO'   'N',
  ''          ''      ''  'WG_HEADER-VTWEG'          wl_0060-vtweg,
  ''          ''      ''  'WG_HEADER-SPART'          wl_0060-spart,
  ''          ''      ''  'WG_HEADER-VKBUR'          wl_0060-vkbur,
  ''          ''      ''  'WG_HEADER-VKGRP'          wl_0060-vkgrp,
  ''          ''      ''  'WG_HEADER-TX_MULTA'       '2,00',
  ''          ''      ''  'WG_HEADER-TX_JUROS'       '1,00',
  ''          ''      ''  'BDC_SUBSCR'               'ZSDR0022                                0052SUB01',
  ''          ''      ''  'BDC_SUBSCR'               'ZSDR0022                                0101TAB_STRIP_SCA',
  'ZSDR0022'  '0050'  'X' ''                         ' ',
  ''          ''      ''  'BDC_OKCODE'               '=SEARCH',
  "''          ''      ''  'BDC_CURSOR'               'WG_HEADER-ID_CONTRATO',
  ''          ''      ''  'WG_HEADER-DATA_VENDA'     v_data,
  ''          ''      ''  'WG_HEADER-TP_VENDA'       <fs_saida>-tp_venda  ,
  ''          ''      ''  'WG_HEADER-ID_CONTRATO'    <fs_saida>-id_contrato,
  ''          ''      ''  'WG_HEADER-VKORG'          <fs_saida>-empresa,
  ''          ''      ''  'WG_HEADER-RISCO_SACADO'   'N',
  ''          ''      ''  'WG_HEADER-VTWEG'          wl_0060-vtweg,
  ''          ''      ''  'WG_HEADER-SPART'          wl_0060-spart,
  ''          ''      ''  'WG_HEADER-VKBUR'          wl_0060-vkbur,
  ''          ''      ''  'WG_HEADER-VKGRP'          wl_0060-vkgrp,
  ''          ''      ''  'WG_HEADER-TX_MULTA'       '2,00',
  ''          ''      ''  'WG_HEADER-TX_JUROS'       '1,00',
  ''          ''      ''  'BDC_SUBSCR'               'ZSDR0022                                0052SUB01',
  ''          ''      ''  'BDC_SUBSCR'               'ZSDR0022                                0101TAB_STRIP_SCA'.
*'ZSDR0022'  '0050'  'X' ''              ' ',
*''          ''      ''  'BDC_OKCODE'  '=ADD',
*''          ''      ''  'BDC_CURSOR'  'WG_HEADER-NRO_SOL_OV',
*''          ''      ''  'BDC_SUBSCR'  'ZSDR0022                                0052SUB01',
*''          ''      ''  'BDC_SUBSCR'  'ZSDR0022                                0101TAB_STRIP_SCA',
*'ZSDR0022'  '0050'  'X' ''              ' ',
*"''          ''      ''  'BDC_OKCODE'  '=SEARCH',
*''          ''      ''  'BDC_CURSOR'  'WG_HEADER-TP_VENDA',
*''          ''      ''  'WG_HEADER-DATA_VENDA'  V_DATA,
*''          ''      ''  'WG_HEADER-TP_VENDA'  <FS_SAIDA>-TP_VENDA  ,
*''          ''      ''  'WG_HEADER-ID_CONTRATO'  <FS_SAIDA>-ID_CONTRATO,
*''          ''      ''  'wg_header-vkbur' wl_0060-vkbur,
*''          ''      ''  'wg_header-vkgrp' wl_0060-vkgrp,
*''          ''      ''  'wg_header-vtweg' wl_0060-vtweg,
*''          ''      ''  'wg_header-spart' wl_0060-spart,
*''          ''      ''  'wg_header-waerk' wl_0060-waerk ,
*''          ''      ''  'wg_header-vkaus' wl_0060-vkaus ,
*''          ''      ''  'wg_header-inco1' wl_0060-inco1 ,
*''          ''      ''  'wg_header-inco2' wl_0060-inco2 ,
*''          ''      ''  'WG_HEADER-VKORG'  <FS_SAIDA>-EMPRESA,
*''          ''      ''  'WG_HEADER-RISCO_SACADO'  'N',
*''          ''      ''  'WG_HEADER-TX_MULTA'  '2,00',
*''          ''      ''  'WG_HEADER-TX_JUROS'  '1,00',
*''          ''      ''  'BDC_SUBSCR'  'ZSDR0022                                0052SUB01',
*''          ''      ''  'BDC_SUBSCR'  'ZSDR0022'  .

  CALL TRANSACTION 'ZSDT0062' USING it_dta
        MODE wl_mode
        MESSAGES INTO it_msg
        UPDATE 'S'.

*LOOP AT it_msg INTO DATA(W_MSG).
*
*  IF W_MSG-MSGV3 EQ ''.
*
*    ENDIF.
*
*  ENDLOOP.
  PERFORM seleciona_dados.

  IF grid IS NOT INITIAL.
    CALL METHOD grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ENVIA_TRACE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM envia_trace .
  CLEAR: it_sel_rows[].

  CALL METHOD grid->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  IF it_sel_rows[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Selecione uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ELSE.
    IF l_envia_trace IS INITIAL.
      MESSAGE s024(sd) WITH 'Empresa não parametrizada para enviar dados ao Trace Cotton' DISPLAY LIKE 'E'.
      EXIT.
    ELSE.

      READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

      READ TABLE it_saida INTO wa_saida INDEX wa_sel_rows-index.
      IF sy-subrc EQ 0.

*-CS2023000189-04.04.2023-#108693-JT-inicio
        CLEAR l_error.

*-----------------------------
*-------- cadastra contratos no Trace
*-----------------------------
        TRY .
            zcl_trace_cotton=>zif_trace_cotton~get_instance(
               )->set_cadastra_contratos( EXPORTING i_id_contrato = wa_saida-id_contrato ).

          CATCH zcx_integracao INTO DATA(ex_integra).
            l_error = abap_true.

          CATCH zcx_error INTO DATA(ex_error).
            l_error = abap_true.
        ENDTRY.

        IF l_error = abap_false.
          "CALL METHOD cl_grid->refresh_table_display( is_stable = wa_stable ).
          MESSAGE |Envio de informações ao sistema TraceCotton Finalizada. Verifique o Log!| TYPE 'S'. "BUG - 65565
        ELSE.
          MESSAGE |A tentativa de envio das informação ao sistema TraceCotton Falhou. Verifique o Log| TYPE 'E'. "BUG - 65565

        ENDIF.

      ENDIF.
*-CS2023000189-04.04.2023-#108693-JT-inicio

      IF grid IS NOT INITIAL.
        CALL METHOD grid->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      ENDIF.

    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module TAB_STRIP_ACTIVE_TAB_SET OUTPUT
*&---------------------------------------------------------------------*
MODULE tab_strip_active_tab_set OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.


  CASE v_param_espec.
    WHEN 'Z' OR 'M'.
      g_tab_strip-subscreen = '0522'.
    WHEN OTHERS.
      g_tab_strip-subscreen = '0521'.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TAB_STRIP_ACTIVE_TAB_GET  INPUT
*&---------------------------------------------------------------------*
MODULE tab_strip_active_tab_get INPUT.
  ok_code = sy-ucomm.

  CASE ok_code.
    WHEN c_tab_strip-tab1.
      g_tab_strip-pressed_tab = c_tab_strip-tab1.
    WHEN c_tab_strip-tab2.
      g_tab_strip-pressed_tab = c_tab_strip-tab2.
    WHEN c_tab_strip-tab3.
      g_tab_strip-pressed_tab = c_tab_strip-tab3.
    WHEN c_tab_strip-tab4.
      g_tab_strip-pressed_tab = c_tab_strip-tab4.
    WHEN c_tab_strip-tab5.
      g_tab_strip-pressed_tab = c_tab_strip-tab5.
    WHEN c_tab_strip-tab6.
      g_tab_strip-pressed_tab = c_tab_strip-tab6.
    WHEN c_tab_strip-tab7.
      g_tab_strip-pressed_tab = c_tab_strip-tab7.
    WHEN c_tab_strip-tab8.
      g_tab_strip-pressed_tab = c_tab_strip-tab8.
    WHEN c_tab_strip-tab9.
      g_tab_strip-pressed_tab = c_tab_strip-tab9.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CRIA_OBJETOS OUTPUT
*&---------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.

*
*  DATA: wl_repid    TYPE sy-repid,
*        wl_function LIKE tl_function WITH HEADER LINE,
*        lt_f4       TYPE lvc_t_f4 WITH HEADER LINE.
*
*  DATA: l_hierarchy_header TYPE treev_hhdr,
*        ls_variant         TYPE disvariant.
*
*  DATA: lt_events TYPE cntl_simple_events,
*        l_event   TYPE cntl_simple_event.
*
*  IF v_param_espec NE 'M' AND
*     v_param_espec NE 'Z'.
*  IF container_s IS INITIAL.
*    CLEAR: wa_layout.
*    wa_layout-zebra      = c_x.
*    wa_layout-no_rowmark = c_x.
**    WA_STABLE-ROW        = C_X.
*    wa_layout-stylefname = 'STYLE'.
*    wa_layout-info_fname = 'LINE_COLOR'.
*
*    CREATE OBJECT container_s
*      EXPORTING
*        container_name = 'CC_PRECO'.
*
*    CREATE OBJECT grid_s
*      EXPORTING
*        i_parent = container_s.
*
*    PERFORM cl_gui_alv_grid USING 'DINAM'. "Exclui os Botão da ALV
*
*    REFRESH: t_fieldcatalog.
*    CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
*      EXPORTING
*        i_tp_venda      = w_contrato-tp_venda
*      IMPORTING
*        e_table         = t_new_table
*      TABLES
*        te_fieldcatalog = t_fieldcatalog.
*
** Cria uma field-symbol como tabela interna
*    ASSIGN t_new_table->* TO <fs_table>.
*    CREATE DATA t_new_line LIKE LINE OF <fs_table>.
*
** Cria uma field-symbol como work area
*    ASSIGN t_new_line->* TO <fs_line>.
**    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
**      EXPORTING
***       I_BUFFER_ACTIVE        =
**        i_structure_name       = 'ZSDT0059'
***       I_CLIENT_NEVER_DISPLAY = 'X'
***       I_BYPASSING_BUFFER     =
**        i_internal_tabname     = 'TG_0059'
**      CHANGING
**        ct_fieldcat            = t_fieldcatalog
**      EXCEPTIONS
**        inconsistent_interface = 1
**        program_error          = 2
**        OTHERS                 = 3.
**    IF sy-subrc <> 0.
*** Implement suitable error handling here
**    ENDIF.
*
*    CALL METHOD grid_s->set_table_for_first_display
*      EXPORTING
*        it_toolbar_excluding = tl_function
*        is_layout            = wa_layout
**       i_save               = 'X'
*      CHANGING
*        it_fieldcatalog      = t_fieldcatalog[]
*        it_outtab            = <fs_table>.
*
*    CALL METHOD grid_s->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*    CALL METHOD grid_s->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
**    SET HANDLER: lcl_event_handler=>on_double_click FOR grid_s,
**                 lcl_event_handler=>on_data_changed_finished_preco FOR grid_s,
**                 lcl_event_handler=>on_data_changed_preco FOR grid_s.
*  ELSE.
*    CALL METHOD grid_s->refresh_table_display
*      EXPORTING
*        is_stable = wa_stable.
*  ENDIF.
*
*
*ELSE.
*  IF container_f IS INITIAL.
*    CREATE OBJECT tree_event_receiver.
*
*    CREATE OBJECT container_f
*      EXPORTING
*        container_name = 'CC_FRAME'.
*
*    CREATE OBJECT splitter
*      EXPORTING
*        parent  = container_f
*        rows    = 1
*        columns = 2.
*
*    CALL METHOD splitter->set_column_sash
*      EXPORTING
*        id    = 1
*        type  = splitter->type_movable
*        value = splitter->false.
*
*    CALL METHOD splitter->get_container
*      EXPORTING
*        row       = 1
*        column    = 1
*      RECEIVING
*        container = container_spliter1.
*
**    posiciona spliter na altura x
*    CALL METHOD splitter->set_column_width
*      EXPORTING
*        id    = 1
*        width = 100.
*
**    PERFORM MONTAR_LAYOUT USING 'PRECO_FRAME'.
*    CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
*      EXPORTING
*        i_tp_venda      = w_contrato-tp_venda
*      IMPORTING
*        e_table         = t_new_table
*      TABLES
*        te_fieldcatalog = t_fieldcatalog.
*
** Cria uma field-symbol como tabela interna
*    ASSIGN t_new_table->* TO <fs_table_frame>.
*    LOOP AT t_fieldcatalog INTO w_fieldcatalog.
*      IF w_fieldcatalog-fieldname EQ 'BEZEI'.
*        w_fieldcatalog-no_out = c_x.
*      ENDIF.
*      w_fieldcatalog-tabname = '<FS_TABLE_FRAME>'.
*      MODIFY t_fieldcatalog FROM w_fieldcatalog.
*    ENDLOOP.
*
** create tree control
*    CREATE OBJECT tree1
*      EXPORTING
*        parent                      = container_spliter1
*        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_multiple
*        item_selection              = 'X'
*        no_html_header              = 'X'
*        no_toolbar                  = ''
*      EXCEPTIONS
*        cntl_error                  = 1
*        cntl_system_error           = 2
*        create_error                = 3
*        lifetime_error              = 4
*        illegal_node_selection_mode = 5
*        failed                      = 6
*        illegal_column_name         = 7.
*
*    IF sy-subrc <> 0.
*      MESSAGE x208(00) WITH 'ERROR'.                        "#EC NOTEXT
*    ENDIF.
*
*    PERFORM build_hierarchy_header CHANGING l_hierarchy_header.
*
*    ls_variant-report = sy-repid.
*
** create emty tree-control
*    CALL METHOD tree1->set_table_for_first_display
*      EXPORTING
*        is_hierarchy_header = l_hierarchy_header
*        i_save              = 'A'
*        is_variant          = ls_variant
*      CHANGING
*        it_outtab           = <FS_TABLE_FRAME>
*        it_fieldcatalog     = t_fieldcatalog.
*
*    CLEAR l_event.
*    l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
*    l_event-appl_event = 'X'.
*    APPEND l_event TO lt_events.
*
** define the events which will be passed to the backend
*    CLEAR l_event.
*    l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
*    l_event-appl_event = 'X'.
*    APPEND l_event TO lt_events.
*    CLEAR l_event.
*    l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
*    l_event-appl_event = 'X'.
*    APPEND l_event TO lt_events.
*    CLEAR l_event.
*    l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
*    l_event-appl_event = 'X'.
*    APPEND l_event TO lt_events.
** register events
*    CALL METHOD tree1->set_registered_events
*      EXPORTING
*        events                    = lt_events
*      EXCEPTIONS
*        cntl_error                = 1
*        cntl_system_error         = 2
*        illegal_event_combination = 3.
*
** set handler for tree1
*    SET HANDLER: tree_event_receiver->handle_double_click       FOR tree1,
*                 tree_event_receiver->handle_item_double_click  FOR tree1,
*                 tree_event_receiver->handle_expand_no_children FOR tree1.
** calculate totals
*    CALL METHOD tree1->update_calculations.
*  ELSE.
** calculate totals
*    CALL METHOD tree1->update_calculations.
** this method must be called to send the data to the frontend
*    CALL METHOD tree1->frontend_update.
*  ENDIF.
*  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form cl_gui_alv_grid
*&---------------------------------------------------------------------*
FORM cl_gui_alv_grid  USING p_dir.
  DATA wl_function LIKE LINE OF tl_function.

  FREE: tl_function.
  CLEAR wl_function.

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

  CASE p_dir.
    WHEN 'INSTRUCAO' OR 'FORM_LOTE' OR 'PEDIDO'.
    WHEN OTHERS.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
      APPEND wl_function TO tl_function.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*       build hierarchy-header-information
*----------------------------------------------------------------------*
FORM build_hierarchy_header CHANGING
                               p_hierarchy_header TYPE treev_hhdr.

  p_hierarchy_header-heading = TEXT-t05."'Item Fixação'.
  p_hierarchy_header-tooltip = TEXT-t05."'Item Fixação'.
  p_hierarchy_header-width = 50.
  p_hierarchy_header-width_pix = ''.

ENDFORM.                               " build_hierarchy_header

*&---------------------------------------------------------------------*
*&      Form  GET_SET_VALORES
*&---------------------------------------------------------------------*
FORM get_set_valores  USING   p_campo
                              p_get_set
                     CHANGING p_valor.


  ASSIGN COMPONENT p_campo  OF STRUCTURE <fs_line> TO <fs_campo>.

  IF p_get_set EQ 'S'.
    MOVE p_valor TO <fs_campo>.
  ELSEIF p_get_set EQ 'G'.
    MOVE <fs_campo> TO p_valor.
  ENDIF.

ENDFORM.                    " GET_SET_VALORES
*&---------------------------------------------------------------------*
*&      Form  PRECO_FRAME_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_KEY  text
*----------------------------------------------------------------------*
FORM preco_frame_alv  USING    p_node_key.

  DATA: tl_filter    TYPE lvc_t_filt,
        wl_filter    TYPE lvc_s_filt,
        tl_children  TYPE lvc_t_nkey WITH HEADER LINE,
        wl_sort1(30) VALUE 'NIVEL',
        wl_sort2(30) VALUE 'POSNR'.

  FIELD-SYMBOLS: <fs_line_frame> TYPE any.
  UNASSIGN <fs_line_frame>.
  CREATE DATA t_new_line LIKE LINE OF <fs_table_frame>.

  ASSIGN t_new_line->* TO <fs_line_frame>.

  CALL METHOD tree1->get_children
    EXPORTING
      i_node_key  = p_node_key
    IMPORTING
      et_children = tl_children[].

  READ TABLE tl_children INTO tl_children INDEX 1.

  CALL METHOD tree1->get_outtab_line
    EXPORTING
      i_node_key    = tl_children
    IMPORTING
      e_outtab_line = <fs_line_frame>.

*  READ TABLE tl_children INTO wl_children INDEX 1.
*
*  READ TABLE tg_preco_frame INTO tg_preco_frame INDEX wl_children-table_line.

  CLEAR: w_valor.
*  PERFORM GET_SET_VALORES USING 'POSNR'
*                                'G'
*                        CHANGING WG_VALOR.
  ASSIGN COMPONENT 'POSNR' OF STRUCTURE <fs_line_frame> TO  <fs_campo>.
  w_valor = <fs_campo>.
  CONDENSE w_valor NO-GAPS.

  REFRESH: tl_filter.
  wl_filter-fieldname = 'POSNR'."c_dmbtr.
  wl_filter-sign      = 'I'. "c_i.
  wl_filter-option    = 'EQ'. "c_ne.
  wl_filter-low       = w_valor. "TG_PRECO_FRAME-POSNR.

  APPEND wl_filter TO tl_filter.

  wl_filter-fieldname = 'INVISIBLE'."c_dmbtr.
  wl_filter-sign      = 'I'. "c_i.
  wl_filter-option    = 'EQ'. "c_ne.
  wl_filter-low       = space. "TG_PRECO_FRAME-POSNR.

  APPEND wl_filter TO tl_filter.

  " 21.02.2024 - RAMON ----------------------->
  wl_filter-fieldname = 'REDIST'."c_dmbtr.
  wl_filter-sign      = 'I'. "c_i.
  wl_filter-option    = 'EQ'. "c_ne.
  wl_filter-low       = space.

  APPEND wl_filter TO tl_filter.
  " 21.02.2024 - RAMON -----------------------<

  IF container_spliter2 IS INITIAL.
    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = c_x.
*    WA_STABLE-ROW        = C_X.

    CALL METHOD splitter_f->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = container_spliter2.

*    posiciona spliter na altura x
    CALL METHOD splitter_f->set_column_width
      EXPORTING
        id    = 1
        width = 30.

    CALL METHOD splitter_f->set_column_sash
      EXPORTING
        id    = 1
        type  = splitter_f->type_movable
        value = splitter_f->true.

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
    wl_function = cl_gui_alv_grid=>mc_mb_filter.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_delete_filter.
    APPEND wl_function TO tl_function.

    CREATE OBJECT grid_f
      EXPORTING
        i_parent = container_spliter2.

*    CREATE OBJECT obg_toolbar
*      EXPORTING
*        io_alv_grid = grid_f.

** Register event handler
*    SET HANDLER obg_toolbar->on_toolbar_p_fram FOR grid8.
*    SET HANDLER obg_toolbar->handle_user_command_p_fram FOR grid8.

*    PERFORM MONTAR_LAYOUT USING 'PRECO_FRAME2'.
    REFRESH: t_fieldcatalog.
    CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
      EXPORTING
        i_tp_venda      = v_tp_venda
*      IMPORTING
*       e_table         = t_new_table
      TABLES
        te_fieldcatalog = t_fieldcatalog.

*    SORT  <FS_TABLE> BY (WL_SORT1) ASCENDING (WL_SORT2) ASCENDING.
    wa_layout-stylefname = 'STYLE'.
    wa_layout-info_fname =  'LINE_COLOR'.

    """ tabela de preço 16.02.2024 # aba preço #aba preco "alv aba preco
    CALL METHOD grid_f->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = <fs_table>.

    CALL METHOD grid_f->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid_f->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid_f->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].



*    SET HANDLER:
*              lcl_event_handler=>on_double_click FOR grid_f,
**              lcl_event_handler=>on_hotspot_click for grid1,
*              lcl_event_handler=>on_data_changed_finished_preco FOR grid_f,
*              lcl_event_handler=>on_data_changed_preco FOR grid_f.
**              lcl_event_handler=>on_onf4 FOR grid1.
  ELSE.
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
    wl_function = cl_gui_alv_grid=>mc_mb_filter.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_delete_filter.
    APPEND wl_function TO tl_function.

*    posiciona spliter na altura x
    CALL METHOD splitter_f->set_column_width
      EXPORTING
        id    = 1
        width = 30.
*    CALL METHOD GRID8->SET_FILTER_CRITERIA
*      EXPORTING
*        IT_FILTER = TL_FILTER.

    REFRESH: t_fieldcatalog.
    CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
      EXPORTING
        i_tp_venda      = v_tp_venda
*      IMPORTING
*       e_table         = t_new_table
      TABLES
        te_fieldcatalog = t_fieldcatalog.

    CALL METHOD grid_f->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = <fs_table>.

    CALL METHOD grid_f->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDFORM.                    " PRECO_FRAME_ALV
*&---------------------------------------------------------------------*
*& Module MODIFY_SCREEN OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE modify_screen OUTPUT.
  DATA lv_count TYPE i.

  CLEAR lv_count.
*  w_contrato-nro_sol_ov = '1234567890'.
  SPLIT w_contrato-nro_sol_ov AT '/' INTO TABLE DATA(t_nro_sol).
  LOOP AT SCREEN.
    IF screen-group1 = 'TAB'.
      ADD 1 TO lv_count.
      READ TABLE t_nro_sol INTO DATA(ls_nro_sol) INDEX lv_count.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT |TAB_STRIP_TAB{ lv_count }| OF STRUCTURE tabs TO FIELD-SYMBOL(<fs_value>).
        <fs_value> = ls_nro_sol.
      ELSE.
        screen-invisible = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form get_frame
*&---------------------------------------------------------------------*
FORM get_frame .
* Preco
  IF tree1 IS NOT INITIAL.
    CALL METHOD tree1->delete_all_nodes.
  ENDIF.
  REFRESH: t_fieldcatalog.
  CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
    EXPORTING
      i_tp_venda      = v_tp_venda
    IMPORTING
      e_table         = t_new_table
    TABLES
      te_fieldcatalog = t_fieldcatalog
      te_fields       = t_fields.

* Cria uma field-symbol como tabela interna
  ASSIGN t_new_table->* TO <fs_table>.
  CREATE DATA t_new_line LIKE LINE OF <fs_table>.
  CREATE DATA t_new_table LIKE <fs_table>.
* Cria uma field-symbol como tabela interna
  ASSIGN t_new_table->* TO <fs_table_frame>.

* Cria uma field-symbol como work area
  ASSIGN t_new_line->* TO <fs_line>.
  ASSIGN t_new_line->* TO <fs_line_aux>.

  SORT tg_0059 BY posnr ASCENDING nivel ASCENDING cod_fp ASCENDING .

  " 21.02.2024 - RAMON -->
  APPEND LINES OF tg_0059_2 TO tg_0059.
  " 21.02.2024 - RAMON --<

  LOOP AT tg_0059.

*    MOVE-CORRESPONDING: TL_0059 TO TG_PRECO_N.
    MOVE-CORRESPONDING: tg_0059 TO <fs_line>.

    PERFORM get_set_valores USING 'REDIST'
                                  'S'
                         CHANGING tg_0059-redist.


    PERFORM get_set_valores USING 'TIPO_CALC'
                                  'S'
                         CHANGING tg_0059-tipo_calc.

    PERFORM get_set_valores USING 'C_DECIMAIS'
                                  'S'
                         CHANGING tg_0059-c_decimais.

    PERFORM get_set_valores USING 'PRECO_ITEM'
                                  'S'
                         CHANGING tg_0059-preco.

    PERFORM get_set_valores USING 'WAERS'
                                  'S'
                         CHANGING tg_0059-waers.

    PERFORM get_set_valores USING 'OCBOT'
                                  'S'
                         CHANGING tg_0059-ocbot.

    CONDENSE tg_0059-formula2 NO-GAPS.

    IF tg_0059-c_decimais EQ 2.
      p_2 = tg_0059-formula2.
      WRITE p_2 TO t_preco-formula.
    ELSEIF tg_0059-c_decimais EQ 4.
      p_4 = tg_0059-formula2.
      WRITE p_4 TO t_preco-formula.
    ELSEIF tg_0059-c_decimais EQ 5.
      p_5 = tg_0059-formula2.
      WRITE p_5 TO t_preco-formula.
    ENDIF.
    CONDENSE t_preco-formula NO-GAPS.
*    MOVE: TL_0059-FORMULA2 TO TG_PRECO-FORMULA2.
*    MOVE: TL_0059-FORMULA  TO TG_PRECO_N-FORMULA.

    PERFORM get_set_valores USING tg_0059-field
                                  'S'
                         CHANGING t_preco-formula.

*    APPEND TG_PRECO_N.
    CLEAR:w_tabix.
    LOOP AT <fs_table> ASSIGNING <fs_line_aux>.

      CLEAR: w_valor, w_tabix.
      PERFORM get_set_valores USING  'COD_FP'
                                     'G'
                            CHANGING w_valor.

      CLEAR w_valor_aux.

      ASSIGN COMPONENT 'COD_FP'  OF STRUCTURE <fs_line_aux> TO <fs_campo>.

      w_valor_aux = <fs_campo>.

      IF w_valor EQ w_valor_aux.

        CLEAR w_valor_aux.

        ASSIGN COMPONENT 'POSNR'  OF STRUCTURE <fs_line_aux> TO <fs_campo>.

        w_valor_aux = <fs_campo>.

        IF tg_0059-posnr EQ w_valor_aux AND tg_0059-redist = abap_false. "<-- ramon 21.02.2024

          ASSIGN COMPONENT tg_0059-field  OF STRUCTURE <fs_line_aux> TO <fs_campo>.

          <fs_campo> = t_preco-formula.

          w_tabix = sy-tabix.

        ENDIF.

      ENDIF.

      " 16.02.2024 - RAMON -->
      IF lv_redist = abap_true.

        IF tg_0059-redist = abap_true.

          ASSIGN COMPONENT 'BEZEI' OF STRUCTURE <fs_line_aux> TO <fs_campo>.

          IF <fs_campo> = tg_0059-bezei.

            ASSIGN COMPONENT 'REDIST' OF STRUCTURE <fs_line_aux> TO <fs_campo>.
            <fs_campo> = tg_0059-redist.

          ENDIF.

        ENDIF.

      ENDIF.

      " 16.02.2024 - RAMON --<

*      IF wg_header-param_espec EQ 'Z'. "RJF-Ini
*        ASSIGN COMPONENT 'BEZEI'  OF STRUCTURE <fs_line_aux> TO <fs_campo>.
*        ASSIGN COMPONENT 'PRECO'  OF STRUCTURE <fs_line_aux> TO <fs_campo2>.
*        wl_valor     = <fs_campo>.
*        wl_valor_aux = <fs_campo2>.
*        IF wl_valor EQ 'QUANTIDADE À FIXAR' AND wl_valor_aux IS NOT INITIAL.
*          <fs_campo2> = abap_false.
*        ENDIF.
*      ENDIF. "RJF-Fim

    ENDLOOP.
    IF w_tabix IS INITIAL.

      " 22.02.2023 - RAMON -->
      PERFORM get_set_valores USING  'NIVEL' 'G' CHANGING w_valor.
      gv_ultimo_nvl = w_valor.
      " 22.02.2023 - RAMON --<


      APPEND <fs_line> TO <fs_table>.
    ELSEIF w_tabix IS NOT INITIAL.
*        MODIFY <FS_TABLE> FROM <FS_LINE> INDEX WL_TABIX.
    ENDIF.
    CLEAR: t_preco, tg_0059, <fs_line>.
  ENDLOOP.

  LOOP AT tg_0059 INTO tg_0059.
    MOVE-CORRESPONDING: tg_0059 TO t_preco.
    IF tg_0059-tipo_calc EQ 'C'
    OR tg_0059-tipo_calc EQ 'R'.
      MOVE: tg_0059-formula TO t_preco-formula.
    ELSE.
      IF tg_0059-c_decimais EQ 2.
        p_2 = tg_0059-formula2.
        WRITE p_2 TO t_preco-formula.
      ELSEIF tg_0059-c_decimais EQ 4.
        p_4 = tg_0059-formula2.
        WRITE p_4 TO t_preco-formula.
      ELSEIF tg_0059-c_decimais EQ 5.
        p_5 = tg_0059-formula2.
        WRITE p_5 TO t_preco-formula.
      ENDIF.
      CONDENSE t_preco-formula NO-GAPS.
    ENDIF.

    " 21.02.2024 - RAMON -->
    IF tg_0059-redist = abap_true.
      t_preco-invisible = abap_true.
    ENDIF.
    " 21.02.2024 - RAMON --<

    APPEND t_preco.
  ENDLOOP.
  READ TABLE t_preco  INDEX 1.
  DELETE t_preco WHERE posnr NE t_preco-posnr.

*  IF tree1 IS NOT INITIAL.
*    PERFORM redefine_objetos USING '2'.
*  ENDIF.
  IF v_param_espec EQ 'M' OR v_param_espec EQ 'Z'.
    PERFORM create_hierarchy.
  ENDIF.
  tg_0059_old[] = tg_0059[].
ENDFORM.

*&---------------------------------------------------------------------*
*& Form redefine_objetos
*&---------------------------------------------------------------------*
FORM redefine_objetos USING p_alv.
  DATA: lt_events TYPE cntl_simple_events,
        l_event   TYPE cntl_simple_event.

  CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
    EXPORTING
      i_tp_venda      = v_tp_venda
*      IMPORTING
*     e_table         = t_new_table
    TABLES
      te_fieldcatalog = t_fieldcatalog.

  IF p_alv EQ '1'.

    wa_layout-stylefname = 'STYLE'.
    wa_layout-info_fname =  'LINE_COLOR'.

    CALL METHOD grid_s->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = <fs_table>.

  ELSEIF p_alv EQ '2'.
*     create Hierarchy-header
    DATA l_hierarchy_header TYPE treev_hhdr.
    DATA: ls_variant TYPE disvariant.

    LOOP AT t_fieldcatalog INTO w_fieldcatalog.
      IF w_fieldcatalog-fieldname EQ 'BEZEI'.
        w_fieldcatalog-no_out = c_x.
      ENDIF.
      w_fieldcatalog-tabname = '<FS_TABLE_FRAME>'.
      MODIFY t_fieldcatalog FROM w_fieldcatalog.
    ENDLOOP.


    PERFORM build_hierarchy_header CHANGING l_hierarchy_header.
    ls_variant-report = sy-repid.

    CALL METHOD tree1->free.
    FREE: tree1.
* create tree control
    CREATE OBJECT tree1
      EXPORTING
        parent                      = container_spliter1
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_multiple
        item_selection              = 'X'
        no_html_header              = 'X'
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
      MESSAGE x208(00) WITH 'ERROR'.                        "#EC NOTEXT
    ENDIF.

* create emty tree-control
    CALL METHOD tree1->set_table_for_first_display
      EXPORTING
        is_hierarchy_header = l_hierarchy_header
*       it_list_commentary  = lt_list_commentary
*       i_logo              = l_logo
*       i_background_id     = 'ALV_BACKGROUND'
        i_save              = 'A'
        is_variant          = ls_variant
      CHANGING
        it_outtab           = <fs_table_frame> "TG_PRECO_FRAME[] "table must be emty !!
        it_fieldcatalog     = t_fieldcatalog.

    CLEAR l_event.
    l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
    l_event-appl_event = 'X'.
    APPEND l_event TO lt_events.

* define the events which will be passed to the backend
    CLEAR l_event.
    l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
    l_event-appl_event = 'X'.
    APPEND l_event TO lt_events.
    CLEAR l_event.
    l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
    l_event-appl_event = 'X'.
    APPEND l_event TO lt_events.
    CLEAR l_event.
*    l_event-eventid = cl_gui_column_tree=>eventid_header_click.
*    APPEND l_event TO lt_events.
*    CLEAR l_event.
    l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
    l_event-appl_event = 'X'.
    APPEND l_event TO lt_events.
* register events
    CALL METHOD tree1->set_registered_events
      EXPORTING
        events                    = lt_events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.

* set handler for tree1
    SET HANDLER tree_event_receiver->handle_double_click       FOR tree1.
    SET HANDLER tree_event_receiver->handle_item_double_click  FOR tree1.
    SET HANDLER tree_event_receiver->handle_expand_no_children FOR tree1.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module INIT OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE init OUTPUT.
  DATA wa_ucomm_solic LIKE LINE OF t_ucomm_solic.
  CLEAR: v_tp_venda, v_param_espec, wa_ucomm_solic.
  REFRESH: t_solic, t_ucomm_solic, tg_0059, tg_0059_2.

  IF g_tab_strip-pressed_tab NE tab_strip-activetab.
    IF container_f IS NOT INITIAL AND grid_f IS NOT INITIAL.
      IF grid_f IS NOT INITIAL.
        grid_f->free( ).
        CLEAR grid_f.
      ENDIF.
      IF container_spliter2 IS NOT INITIAL.
        container_spliter2->free( ).
        CLEAR container_spliter2.
      ENDIF.
      container_f->free( ).
      CLEAR: container_f.
    ENDIF.
  ENDIF.

  IF g_tab_strip-pressed_tab IS INITIAL.
    g_tab_strip-pressed_tab = tabs-tab_strip_tab1.
  ENDIF.

  SPLIT w_contrato-nro_sol_ov AT '/' INTO TABLE t_solic.
  LOOP AT t_solic INTO DATA(wa_solic).
    APPEND INITIAL LINE TO t_ucomm_solic ASSIGNING FIELD-SYMBOL(<fs_ucomm_solic>).
    ASSIGN COMPONENT |TAB{ sy-tabix }| OF STRUCTURE c_tab_strip TO <fs_value>.
    <fs_ucomm_solic>-ucomm  = <fs_value>.
    <fs_ucomm_solic>-nr_solic = |{ wa_solic ALPHA = IN }|.
  ENDLOOP.


  READ TABLE t_ucomm_solic INTO wa_ucomm_solic WITH KEY ucomm = g_tab_strip-pressed_tab.

  SELECT SINGLE tp_venda param_espec FROM zsdt0051
    INTO ( v_tp_venda, v_param_espec )
    WHERE nro_sol_ov = wa_ucomm_solic-nr_solic.

  SELECT * FROM zsdt0059
    INTO TABLE tg_0059
      WHERE nro_sol_ov EQ wa_ucomm_solic-nr_solic.

  SELECT * FROM zsdt0059_2
    INTO TABLE tg_0059_2
    WHERE nro_sol_ov EQ wa_ucomm_solic-nr_solic.

  tab_strip-activetab = g_tab_strip-pressed_tab.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form create_hierarchy
*&---------------------------------------------------------------------*
FORM create_hierarchy .

* add data to tree
  DATA: l_posnr_key   TYPE lvc_nkey,
        l_header_key  TYPE lvc_nkey,
        l_last_key    TYPE lvc_nkey,
        wl_posnr      TYPE zsdt0059-posnr,
        wl_fix        TYPE sy-tabix,
        wl_texto      TYPE lvc_value,
        wl_fix_aux(4),
        wl_sort1(30)  VALUE 'POSNR',
        wl_sort2(30)  VALUE 'NIVEL'.


*CALL METHOD tree1->delete_all_nodes.
  REFRESH: <fs_table_frame>.
  SORT: t_preco BY posnr ASCENDING nivel ASCENDING .
  SORT: <fs_table> BY (wl_sort1) ASCENDING (wl_sort2) ASCENDING.

  LOOP AT <fs_table> INTO <fs_line>.
    IF sy-tabix EQ 1.
      PERFORM add_header_line USING    <fs_line>
                                       ''
                              CHANGING l_header_key.
    ENDIF.
    CLEAR: w_valor.
    PERFORM get_set_valores USING 'INVISIBLE'
                                  'G'
                          CHANGING w_valor.
    CONDENSE w_valor NO-GAPS.
    IF w_valor IS INITIAL.
      CLEAR: w_valor.
      PERFORM get_set_valores USING 'POSNR'
                                    'G'
                            CHANGING w_valor.
      CONDENSE w_valor NO-GAPS.
*    ON CHANGE OF tg_preco-posnr.

      IF wl_posnr IS INITIAL
      OR wl_posnr NE  w_valor. "TG_PRECO-POSNR.
        ADD 1 TO wl_fix.
        wl_fix_aux = wl_fix.
        SHIFT wl_fix_aux LEFT DELETING LEADING '0'.
        CONDENSE wl_fix_aux NO-GAPS.

        CONCATENATE TEXT-t05 wl_fix_aux '-' w_valor INTO wl_texto SEPARATED BY space.
        PERFORM add_posnr_line USING    <fs_line>
                                        l_header_key
                                        wl_texto
                                CHANGING l_posnr_key.
      ENDIF.
      PERFORM add_complete_line USING  <fs_line>
                                       l_posnr_key
                              CHANGING l_last_key.

      CLEAR: wl_posnr.
      PERFORM get_set_valores USING 'POSNR'
                                    'G'
                            CHANGING wl_posnr.
      CONDENSE wl_posnr NO-GAPS.
*    WL_POSNR = TG_PRECO-POSNR.
      PERFORM get_set_valores USING 'ITEM_KEY'
                                        'S'
                           CHANGING l_last_key.
      t_preco_f-item_key = l_last_key.
*    MODIFY TG_PRECO .
      MODIFY <fs_table> FROM <fs_line>.
    ENDIF.
  ENDLOOP.

  CALL METHOD tree1->expand_node
    EXPORTING
      i_node_key       = l_header_key
      i_expand_subtree = space.
* calculate totals
  CALL METHOD tree1->update_calculations.

* this method must be called to send the data to the frontend
  CALL METHOD tree1->frontend_update.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_complete_line
*&---------------------------------------------------------------------*
FORM add_complete_line USING   ps_preco "TYPE TY_PRECO
                               p_relat_key TYPE lvc_nkey
                     CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi,
        ls_preco       TYPE ty_preco_n.
*  ls_item_layout-fieldname = 'FORMULA2'.
  ls_item_layout-class   = cl_gui_column_tree=>item_class_text.
*  ls_item_layout-editable = 'X'.
*  append ls_item_layout to lt_item_layout.

*  clear ls_item_layout.
*  ls_item_layout-fieldname = 'PLANETYPE'.
*  ls_item_layout-alignment = cl_gui_column_tree=>align_right.
*  append ls_item_layout to lt_item_layout.
  MOVE-CORRESPONDING: ps_preco TO ls_preco.
  l_node_text =  ls_preco-bezei.

  DATA: ls_node TYPE lvc_s_layn.
  IF ls_preco-tipo_calc EQ 'C'
  OR ls_preco-tipo_calc EQ 'R'.
    ls_node-n_image   = icon_sum. "SPACE.
  ELSEIF ls_preco-tipo_calc EQ 'F'.
    ls_node-n_image   = icon_attachment.
  ELSE.
    ls_node-n_image   = icon_set_state.
  ENDIF.
  ls_node-exp_image = space.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      is_outtab_line   = ps_preco
      i_node_text      = l_node_text
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.
ENDFORM.                               " add_complete_line
*&---------------------------------------------------------------------*
*& Form add_header_line
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM add_header_line USING     ps_preco "TYPE TY_PRECO
                               p_relat_key TYPE lvc_nkey
                     CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
*        LS_PRECO_aux TYPE TY_PRECO_N,
        ls_preco    TYPE ty_preco.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.
  ls_item_layout-t_image = icon_te_costs_assign. "'@3P@'.
  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
  ls_item_layout-style   =
                        cl_gui_column_tree=>style_intensifd_critical.
  APPEND ls_item_layout TO lt_item_layout.

* add node
  l_node_text =  TEXT-t05."'Item Fixação'.

  DATA: ls_node TYPE lvc_s_layn.
  ls_node-n_image   = space.
  ls_node-exp_image = space.
*  MOVE-CORRESPONDING: PS_PRECO to LS_PRECO_Aux.
*  LS_PRECO-monat = LS_PRECO_Aux-monat.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_preco
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                               " add_header_line
*&---------------------------------------------------------------------*
*& Form add_posnr_line
*&---------------------------------------------------------------------*
FORM add_posnr_line USING     ps_preco "TYPE TY_PRECO
                               p_relat_key TYPE lvc_nkey
                               p_text TYPE lvc_value
                     CHANGING  p_node_key TYPE lvc_nkey.

  FIELD-SYMBOLS: <fs_preco> TYPE any.
  UNASSIGN <fs_preco>.
  CREATE DATA t_new_line LIKE LINE OF <fs_table>.

* Cria uma field-symbol como work area
  ASSIGN t_new_line->* TO <fs_preco>.

  DATA: l_node_text TYPE lvc_value,
        ls_preco    TYPE ty_preco,
        wl_monat    TYPE ty_preco-monat.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.
  ls_item_layout-t_image = icon_val_quantity_structure." '@3P@'.
  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
  ls_item_layout-style   =
                        cl_gui_column_tree=>style_intensified.
  APPEND ls_item_layout TO lt_item_layout.

* add node
  l_node_text = p_text." ps_preco-posnr.

  DATA: ls_node TYPE lvc_s_layn.
  ls_node-n_image   = space.
  ls_node-exp_image = space.

*  ASSIGN <FS_LINE> TO <FS_PRECO>.
*
*
  MOVE-CORRESPONDING: ps_preco TO ls_preco.
  wl_monat = ls_preco-monat.
  CLEAR: ls_preco.
  ls_preco-monat = wl_monat.
  MOVE-CORRESPONDING: ls_preco TO <fs_preco>.
*  LS_PRECO-monat = LS_PRECO_Aux-monat.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = <fs_preco>
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                               " add_carrid_line
*&---------------------------------------------------------------------*
*& Module CREATE_SIMPLES OUTPUT
*&---------------------------------------------------------------------*
MODULE create_simples OUTPUT.
  IF container_s IS INITIAL.
    CLEAR: wa_layout.
    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = c_x.
*    WA_STABLE-ROW        = C_X.
    wa_layout-stylefname = 'STYLE'.
    wa_layout-info_fname = 'LINE_COLOR'.

    CREATE OBJECT container_s
      EXPORTING
        container_name = 'CC_PRECO'.

    CREATE OBJECT grid_s
      EXPORTING
        i_parent = container_s.

    PERFORM cl_gui_alv_grid USING 'DINAM'. "Exclui os Botão da ALV

    REFRESH: t_fieldcatalog.
    CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
      EXPORTING
        i_tp_venda      = v_tp_venda
      IMPORTING
        e_table         = t_new_table
      TABLES
        te_fieldcatalog = t_fieldcatalog.

* Cria uma field-symbol como tabela interna
    ASSIGN t_new_table->* TO <fs_table>.
    CREATE DATA t_new_line LIKE LINE OF <fs_table>.

* Cria uma field-symbol como work area
    ASSIGN t_new_line->* TO <fs_line>.
*    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
*      EXPORTING
**       I_BUFFER_ACTIVE        =
*        i_structure_name       = 'ZSDT0059'
**       I_CLIENT_NEVER_DISPLAY = 'X'
**       I_BYPASSING_BUFFER     =
*        i_internal_tabname     = 'TG_0059'
*      CHANGING
*        ct_fieldcat            = t_fieldcatalog
*      EXCEPTIONS
*        inconsistent_interface = 1
*        program_error          = 2
*        OTHERS                 = 3.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.

    CALL METHOD grid_s->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = <fs_table>.

    CALL METHOD grid_s->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid_s->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*    SET HANDLER: lcl_event_handler=>on_double_click FOR grid_s,
*                 lcl_event_handler=>on_data_changed_finished_preco FOR grid_s,
*                 lcl_event_handler=>on_data_changed_preco FOR grid_s.
  ELSE.
    CALL METHOD grid_s->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  PERFORM monta_dados.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_FRAME OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE create_frame OUTPUT.


  IF container_f IS INITIAL.
    CREATE OBJECT tree_event_receiver.

    CREATE OBJECT container_f
      EXPORTING
        container_name = 'CC_FRAME'.

    CREATE OBJECT splitter_f
      EXPORTING
        parent  = container_f
        rows    = 1
        columns = 2.

    CALL METHOD splitter_f->set_column_sash
      EXPORTING
        id    = 1
        type  = splitter_f->type_movable
        value = splitter_f->false.

    CALL METHOD splitter_f->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = container_spliter1.

*    posiciona spliter na altura x
    CALL METHOD splitter_f->set_column_width
      EXPORTING
        id    = 1
        width = 100.

*    PERFORM MONTAR_LAYOUT USING 'PRECO_FRAME'.
    CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
      EXPORTING
        i_tp_venda      = v_tp_venda
      IMPORTING
        e_table         = t_new_table
      TABLES
        te_fieldcatalog = t_fieldcatalog.

* Cria uma field-symbol como tabela interna
    ASSIGN t_new_table->* TO <fs_table_frame>.
    LOOP AT t_fieldcatalog INTO w_fieldcatalog.
      IF w_fieldcatalog-fieldname EQ 'BEZEI'.
        w_fieldcatalog-no_out = c_x.
      ENDIF.
      w_fieldcatalog-tabname = '<FS_TABLE_FRAME>'.
      MODIFY t_fieldcatalog FROM w_fieldcatalog.
    ENDLOOP.

* create tree control
    CREATE OBJECT tree1
      EXPORTING
        parent                      = container_spliter1
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_multiple
        item_selection              = 'X'
        no_html_header              = 'X'
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
      MESSAGE x208(00) WITH 'ERROR'.                        "#EC NOTEXT
    ENDIF.

    PERFORM build_hierarchy_header CHANGING l_hierarchy_header.

    ls_variant-report = sy-repid.

* create emty tree-control
    CALL METHOD tree1->set_table_for_first_display
      EXPORTING
        is_hierarchy_header = l_hierarchy_header
        i_save              = 'A'
        is_variant          = ls_variant
      CHANGING
        it_outtab           = <fs_table_frame>
        it_fieldcatalog     = t_fieldcatalog.

    CLEAR l_event.
    l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
    l_event-appl_event = 'X'.
    APPEND l_event TO lt_events.

* define the events which will be passed to the backend
    CLEAR l_event.
    l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
    l_event-appl_event = 'X'.
    APPEND l_event TO lt_events.
    CLEAR l_event.
    l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
    l_event-appl_event = 'X'.
    APPEND l_event TO lt_events.
    CLEAR l_event.
    l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
    l_event-appl_event = 'X'.
    APPEND l_event TO lt_events.
* register events
    CALL METHOD tree1->set_registered_events
      EXPORTING
        events                    = lt_events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.

* set handler for tree1
    SET HANDLER: tree_event_receiver->handle_double_click       FOR tree1,
                 tree_event_receiver->handle_item_double_click  FOR tree1,
                 tree_event_receiver->handle_expand_no_children FOR tree1.
* calculate totals
    CALL METHOD tree1->update_calculations.
  ELSE.
* calculate totals
    CALL METHOD tree1->update_calculations.
* this method must be called to send the data to the frontend
    CALL METHOD tree1->frontend_update.
  ENDIF.

  PERFORM get_frame.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form monta_dados
*&---------------------------------------------------------------------*
FORM monta_dados .
  REFRESH: t_fieldcatalog.
  CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
    EXPORTING
      i_tp_venda      = v_tp_venda
    IMPORTING
      e_table         = t_new_table
    TABLES
      te_fieldcatalog = t_fieldcatalog
      te_fields       = t_fields.


  CALL METHOD grid_s->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = t_fieldcatalog.

* Cria uma field-symbol como tabela interna
  ASSIGN t_new_table->* TO <fs_table>.
  CREATE DATA t_new_line LIKE LINE OF <fs_table>.
  CREATE DATA t_new_table LIKE <fs_table>.
* Cria uma field-symbol como tabela interna
  ASSIGN t_new_table->* TO <fs_table_frame>.

* Cria uma field-symbol como work area
  ASSIGN t_new_line->* TO <fs_line>.
  ASSIGN t_new_line->* TO <fs_line_aux>.

  SORT tg_0059 BY posnr ASCENDING nivel ASCENDING cod_fp ASCENDING .

  APPEND LINES OF tg_0059_2 TO tg_0059.

  LOOP AT tg_0059.

    MOVE-CORRESPONDING: tg_0059 TO <fs_line>.

    PERFORM get_set_valores USING 'REDIST'
                                  'S'
                         CHANGING tg_0059-redist.


    PERFORM get_set_valores USING 'TIPO_CALC'
                                  'S'
                         CHANGING tg_0059-tipo_calc.

    PERFORM get_set_valores USING 'C_DECIMAIS'
                                  'S'
                         CHANGING tg_0059-c_decimais.

    PERFORM get_set_valores USING 'PRECO_ITEM'
                                  'S'
                         CHANGING tg_0059-preco.

    PERFORM get_set_valores USING 'WAERS'
                                  'S'
                         CHANGING tg_0059-waers.

    PERFORM get_set_valores USING 'OCBOT'
                                  'S'
                         CHANGING tg_0059-ocbot.

    CONDENSE tg_0059-formula2 NO-GAPS.

    IF tg_0059-c_decimais EQ 2.
      p_2 = tg_0059-formula2.
      WRITE p_2 TO t_preco-formula.
    ELSEIF tg_0059-c_decimais EQ 4.
      p_4 = tg_0059-formula2.
      WRITE p_4 TO t_preco-formula.
    ELSEIF tg_0059-c_decimais EQ 5.
      p_5 = tg_0059-formula2.
      WRITE p_5 TO t_preco-formula.
    ENDIF.
    CONDENSE t_preco-formula NO-GAPS.

    PERFORM get_set_valores USING tg_0059-field
                                  'S'
                         CHANGING t_preco-formula.

    CLEAR:w_tabix.
    LOOP AT <fs_table> ASSIGNING <fs_line_aux>.

      CLEAR: w_valor, w_tabix.
      PERFORM get_set_valores USING  'COD_FP'
                                     'G'
                            CHANGING w_valor.

      CLEAR w_valor_aux.

      ASSIGN COMPONENT 'COD_FP'  OF STRUCTURE <fs_line_aux> TO <fs_campo>.

      w_valor_aux = <fs_campo>.

      IF w_valor EQ w_valor_aux.

        CLEAR w_valor_aux.

        ASSIGN COMPONENT 'POSNR'  OF STRUCTURE <fs_line_aux> TO <fs_campo>.

        w_valor_aux = <fs_campo>.

        IF tg_0059-posnr EQ w_valor_aux AND tg_0059-redist = abap_false.

          ASSIGN COMPONENT tg_0059-field  OF STRUCTURE <fs_line_aux> TO <fs_campo>.

          <fs_campo> = t_preco-formula.

          w_tabix = sy-tabix.

        ENDIF.

      ENDIF.

      IF lv_redist = abap_true.

        IF tg_0059-redist = abap_true.

          ASSIGN COMPONENT 'BEZEI' OF STRUCTURE <fs_line_aux> TO <fs_campo>.

          IF <fs_campo> = tg_0059-bezei.

            ASSIGN COMPONENT 'REDIST' OF STRUCTURE <fs_line_aux> TO <fs_campo>.
            <fs_campo> = tg_0059-redist.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDLOOP.
    IF w_tabix IS INITIAL.

      " 22.02.2023 - RAMON -->
      PERFORM get_set_valores USING  'NIVEL' 'G' CHANGING w_valor.
      gv_ultimo_nvl = w_valor.
      " 22.02.2023 - RAMON --<


      APPEND <fs_line> TO <fs_table>.
    ELSEIF w_tabix IS NOT INITIAL.
*        MODIFY <FS_TABLE> FROM <FS_LINE> INDEX WL_TABIX.
    ENDIF.
    CLEAR: t_preco, tg_0059, <fs_line>.
  ENDLOOP.

  LOOP AT tg_0059 INTO tg_0059.
    MOVE-CORRESPONDING: tg_0059 TO t_preco.
    IF tg_0059-tipo_calc EQ 'C'
    OR tg_0059-tipo_calc EQ 'R'.
      MOVE: tg_0059-formula TO t_preco-formula.
    ELSE.
      IF tg_0059-c_decimais EQ 2.
        p_2 = tg_0059-formula2.
        WRITE p_2 TO t_preco-formula.
      ELSEIF tg_0059-c_decimais EQ 4.
        p_4 = tg_0059-formula2.
        WRITE p_4 TO t_preco-formula.
      ELSEIF tg_0059-c_decimais EQ 5.
        p_5 = tg_0059-formula2.
        WRITE p_5 TO t_preco-formula.
      ENDIF.
      CONDENSE t_preco-formula NO-GAPS.
    ENDIF.

    " 21.02.2024 - RAMON -->
    IF tg_0059-redist = abap_true.
      t_preco-invisible = abap_true.
    ENDIF.
    " 21.02.2024 - RAMON --<

    APPEND t_preco.
  ENDLOOP.
  READ TABLE t_preco  INDEX 1.
  DELETE t_preco WHERE posnr NE t_preco-posnr.

  IF grid_s IS NOT INITIAL.
    PERFORM redefine_objetos USING '1'.
  ENDIF.

  tg_0059_old[] = tg_0059[].
ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0521 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0521 OUTPUT.
  DATA: BEGIN OF tl_ucomm OCCURS 0,
          ucomm TYPE  sy-ucomm,
        END OF tl_ucomm,

        tl_0064      TYPE TABLE OF zsdt0064 WITH HEADER LINE,
        tl_t052      TYPE TABLE OF t052 WITH HEADER LINE,
        wl_tipo_calc TYPE zsdt0059-tipo_calc,
        wl_vbfa      TYPE vbfa,
        wl_style     LIKE cl_gui_alv_grid=>mc_style_enabled,
        wa_t052      TYPE t052,
        block_r      TYPE posnr,
        tabix_r      TYPE sy-tabix.

  IF grid_s IS NOT INITIAL.
    CALL METHOD grid_s->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = t_fieldcatalog.

    LOOP AT t_fieldcatalog INTO w_fieldcatalog.

      READ TABLE t_fields TRANSPORTING NO FIELDS
        WITH KEY field = w_fieldcatalog-fieldname.

      IF sy-subrc IS INITIAL.
        w_fieldcatalog-edit = space.
        MODIFY t_fieldcatalog FROM w_fieldcatalog.
      ENDIF.


      IF w_fieldcatalog-fieldname EQ 'CBOT'.
        w_fieldcatalog-edit = abap_true.
        MODIFY t_fieldcatalog FROM w_fieldcatalog.
      ENDIF.

      IF w_fieldcatalog-fieldname EQ 'SAFRA'.
        w_fieldcatalog-edit = abap_true.
        MODIFY t_fieldcatalog FROM w_fieldcatalog.
      ENDIF.

    ENDLOOP.

    CALL METHOD grid_s->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog.

    LOOP AT <fs_table> ASSIGNING <fs_line>.
      REFRESH: style. "TG_PRECO-STYLE.
      CLEAR: w_valor, wl_tipo_calc.
*        PERFORM GET_SET_VALORES
*                    USING
*                       'STYLE'
*                       'S'
*                    CHANGING
*                       WG_VALOR.
      ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs_line> TO <fs_campo>.
      <fs_campo> = style[].

      PERFORM get_set_valores
                  USING
                     'TIPO_CALC'
                     'G'
                  CHANGING
                      wl_tipo_calc.

      IF wl_tipo_calc EQ c_c
      OR wl_tipo_calc EQ c_r.
      ENDIF.
      w_style-fieldname = 'BEZEI'.
      IF wl_tipo_calc NE c_c
      AND wl_tipo_calc NE c_r.
        w_style-style = alv_style_font_bold_no.
      ELSE.
        w_style-style =  alv_style_font_bold.
        w_style-style2 =  alv_style2_no_border_left.
      ENDIF.
      INSERT  w_style INTO TABLE style .

      LOOP AT t_fields.
        w_style-fieldname = t_fields-field. "'FORMULA2'.
        IF wl_tipo_calc NE c_c
        AND wl_tipo_calc NE c_f
        AND wl_tipo_calc NE c_r.
          w_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_font_bold_no.
        ELSE.
          IF wl_tipo_calc EQ c_c
          OR wl_tipo_calc EQ c_r.
            w_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_font_bold.
          ELSE.
            w_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_font_bold_no.
          ENDIF.
        ENDIF.
        INSERT  w_style INTO TABLE style .
      ENDLOOP.
      w_style-fieldname = 'WAERS'.
      w_style-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT  w_style INTO TABLE style .

      w_style-fieldname = 'CBOT'.
      w_style-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT  w_style INTO TABLE style .


* RJF
      w_style-fieldname = 'SAFRA'.
      w_style-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT  w_style INTO TABLE style .

*        IF wg_header-param_espec EQ c_m.
*          w_style-fieldname = 'ZMENG'.
*          w_style-style = cl_gui_alv_grid=>mc_style_disabled.
*          INSERT  w_style INTO TABLE style .
*          w_style-fieldname = 'VALDT'.
*          w_style-style = cl_gui_alv_grid=>mc_style_disabled.
*          INSERT  w_style INTO TABLE style .
*          w_style-fieldname = 'MONAT'.
*          w_style-style = cl_gui_alv_grid=>mc_style_disabled.
*          INSERT  w_style INTO TABLE style.
*        ENDIF.
*        INSERT LINES OF style INTO TABLE tg_preco-style.
*        PERFORM GET_SET_VALORES
*                   USING
*                      'STYLE'
*                      'S'
*                   CHANGING
*                      STYLE[].
      ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs_line> TO <fs_campo>.
      <fs_campo> = style[].
      IF wl_tipo_calc EQ c_c
      OR wl_tipo_calc EQ c_r.
        w_valor = 'C305'.
*          MOVE: 'C305' TO TG_PRECO-LINE_COLOR.
        PERFORM get_set_valores
                 USING
                    'LINE_COLOR'
                    'S'
                 CHANGING
                    w_valor.
      ELSE.
*          CLEAR TG_PRECO-LINE_COLOR.
        CLEAR: w_valor.

        PERFORM get_set_valores
                 USING
                    'LINE_COLOR'
                    'S'
                 CHANGING
                    w_valor.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0522 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0522 OUTPUT.
*      IF grid_f IS NOT INITIAL.
*
*      CALL METHOD grid_f->get_frontend_fieldcatalog
*        IMPORTING
*          et_fieldcatalog = t_fieldcatalog.
*
*
*        LOOP AT <fs_table> ASSIGNING <fs_line>.
*          REFRESH: style.
*          CLEAR: w_valor, wl_tipo_calc.
*
*
*          PERFORM get_set_valores USING 'POSNR' 'G' CHANGING w_valor.
*          READ TABLE tg_itens TRANSPORTING NO FIELDS WITH KEY fixacao     = w_valor
*                                                              status_itm  = c_f.
*          IF sy-subrc IS INITIAL.
*            PERFORM get_set_valores USING 'TIPO_CALC' 'G' CHANGING wl_tipo_calc.
*
*            CASE wl_tipo_calc.
*              WHEN c_c OR c_r.
*              WHEN OTHERS.
*                ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs_line> TO <fs_campo>.
*                <fs_campo> = style[].
*            ENDCASE.
*          ELSE.
*            ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs_line> TO <fs_campo>.
*
*            w_style-fieldname = 'CBOT'.
*            w_style-style = cl_gui_alv_grid=>mc_style_disabled.
*            INSERT  w_style INTO TABLE style .
*
*            <fs_campo> = style[].
*
*          ENDIF.
*        ENDLOOP.
*
*        FREE: wg_acao.
*
*      ENDIF.
*
*      CALL METHOD grid_f->set_frontend_fieldcatalog
*        EXPORTING
*          it_fieldcatalog = t_fieldcatalog.
*
*
*    ENDIF.
ENDMODULE.

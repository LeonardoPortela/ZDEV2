*&---------------------------------------------------------------------*
*& Report  ZLESR0086
*&
*&---------------------------------------------------------------------*
*&TITULO  : Cockipt do Balanceiro
*&AUTOR   : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA.   : 29.07.2014
*TRANSACAO: ZLES0106
*&---------------------------------------------------------------------*
REPORT  zlesr0086.

INCLUDE <cl_alv_control>.

*----------------------------------------------------------------------*
* TYPE POOLS.
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: vbak,vbkd, ekko,zsdt0001,lfa1,vbpa.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

DATA: wl_mode(1).
DATA: l_var TYPE indx-srtfd.

DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_zsdt0001.
         INCLUDE STRUCTURE zsdt0001.
TYPES:   route  TYPE vbap-route,
         shtyp  TYPE zsdt0011-shtyp,
         lzonea TYPE a910-lzonea,
         lzonez TYPE a910-lzonez,
         add01  TYPE a915-add01,
         del(1),
       END OF ty_zsdt0001.

TYPES:BEGIN OF ty_vbpa_cr, "Ponto de coleta  REMESSA
        vbeln TYPE vbpa-vbeln,
        lifnr TYPE vbpa-lifnr,
      END OF ty_vbpa_cr,

      BEGIN OF ty_mara,
        matnr TYPE mara-matnr,
        spart TYPE mara-spart,
      END OF ty_mara,

      BEGIN OF ty_vbpa_co, "Ponto de coleta  ORDEM
        vbeln TYPE vbpa-vbeln,
        lifnr TYPE vbpa-lifnr,
      END OF ty_vbpa_co,

      BEGIN OF ty_ekpa_pr, "Ponto de coleta  Pedido
        ebeln TYPE ekpa-ebeln,
        lifn2 TYPE ekpa-lifn2,
      END OF ty_ekpa_pr,

      BEGIN OF ty_vbap, "Itinerário  ORDE M
        vbeln TYPE vbap-vbeln,
        route TYPE vbap-route,
      END OF ty_vbap,

      BEGIN OF ty_ekpv, "Itinerário  PEDIDO
        ebeln TYPE ekpv-ebeln,
        route TYPE ekpv-route,
      END OF ty_ekpv,

      BEGIN OF ty_vbak,
        vbeln        TYPE vbak-vbeln,
        auart        TYPE vbak-auart,
        kunnr        TYPE vbak-kunnr,
        tp_movimento TYPE zsdt0001-tp_movimento,
      END OF ty_vbak,

      BEGIN OF ty_tvakt,
        auart TYPE tvakt-auart,
        bezei TYPE tvakt-bezei,
      END OF ty_tvakt,

      BEGIN OF ty_tvtk,
        shtyp TYPE tvtk-shtyp,
        laufk TYPE tvtk-laufk,
      END OF ty_tvtk,

      BEGIN OF ty_t161t,
        bsart TYPE t161t-bsart,
        batxt TYPE t161t-batxt,
      END OF ty_t161t,

      BEGIN OF ty_vbkd,
        vbeln TYPE vbkd-vbeln,
        inco1 TYPE vbkd-inco1,
      END OF ty_vbkd,

      BEGIN OF ty_ekko,
        ebeln        TYPE ekko-ebeln,
        bsart        TYPE ekko-bsart,
        reswk        TYPE ekko-reswk,
        tp_movimento TYPE zsdt0001-tp_movimento,
      END OF ty_ekko,

      BEGIN OF ty_ekpo,
        ebeln TYPE ekpo-ebeln,
        werks TYPE ekpo-werks,
        inco1 TYPE ekpo-inco1,
        lifnr TYPE lfa1-lifnr,
      END OF ty_ekpo,

      BEGIN OF ty_vbpa,
        vbeln TYPE vbpa-vbeln,
        parvw TYPE vbpa-parvw,
        lifnr TYPE vbpa-lifnr,
        kunnr TYPE vbpa-kunnr,
      END OF ty_vbpa,

      BEGIN OF ty_kna1,
        kunnr TYPE kna1-kunnr,
        name1 TYPE kna1-name1,
        lzone TYPE kna1-lzone,
      END OF ty_kna1,

      BEGIN OF ty_lfa1,
        lifnr TYPE lfa1-lifnr,
        name1 TYPE lfa1-name1,
        dlgrp TYPE lfa1-dlgrp,
        lzone TYPE lfa1-lzone,
        regio TYPE lfa1-regio,
      END OF ty_lfa1,

      BEGIN OF ty_t001w,
        werks TYPE t001w-werks,
        name1 TYPE t001w-name1,
      END OF ty_t001w,

      BEGIN OF ty_makt,
        matnr TYPE makt-matnr,
        maktx TYPE makt-maktx,
      END OF ty_makt,


      BEGIN OF ty_trans,
        lifnr TYPE lfa1-lifnr , " Agente Frete
        name1 TYPE lfa1-name1 , " Agente Frete
        cnpj  TYPE lfa1-stcd1,
      END OF ty_trans,

      BEGIN OF ty_veic,
        placa    TYPE zsdt0001-placa_cav,
        tipo     TYPE zlest0002-tp_veiculo,
        cod_prop TYPE zlest0002-proprietario,
        nom_prop TYPE lfa1-name1,
        rntc(10),
        renavam  TYPE zlest0002-cd_renavam,
        cnpj     TYPE lfa1-stcd1,
        cpf      TYPE lfa1-stcd2,
        cidade   TYPE zlest0002-cd_cidade,
        uf       TYPE zlest0002-cd_uf,
      END OF ty_veic,

      BEGIN OF ty_mot,
        motorista TYPE lfa1-lifnr,
        name1     TYPE lfa1-name1,
        cpf       TYPE lfa1-stcd2,
        habil     TYPE lfa1-stcd4,
        uf_habil  TYPE lfb1-zsabe,
        rg        TYPE lfa1-stcd3,
        uf_rg     TYPE lfb1-eikto,
      END OF ty_mot,

      BEGIN OF ty_ag_frete,
        transpor1 TYPE lfa1-lifnr,
        name1     TYPE lfa1-name1,
        cnpj1     TYPE lfa1-stcd1,
        inscr1    TYPE lfa1-stcd3,
        transpor2 TYPE lfa1-lifnr,
        name2     TYPE lfa1-name1,
        cnpj2     TYPE lfa1-stcd1,
        inscr2    TYPE lfa1-stcd3,
      END OF ty_ag_frete,

      BEGIN OF ty_editor,
        line(100),
      END   OF ty_editor,


      BEGIN OF ty_saida,
        icon(4),
        bukrs         TYPE zsdt0001-bukrs,
        branch        TYPE zsdt0001-branch,
        ch_referencia TYPE zsdt0001-ch_referencia,
        dt_movimento  TYPE zsdt0001-dt_movimento,
        nr_romaneio   TYPE zsdt0001-nr_romaneio,
        placa_cav     TYPE zsdt0001-placa_cav,
        region        TYPE zsdt0001-region,
        operacao(25),
        vbeln         TYPE zsdt0001-vbeln,
        peso_liq      TYPE zsdt0001-peso_liq,
        lifnr_c       TYPE lfa1-lifnr , " Ponto de coleta
        name1_c       TYPE lfa1-name1, " Ponto de coleta
        kunnr         TYPE kna1-kunnr,
        name1         TYPE kna1-name1,
        inco1         TYPE vbkd-inco1,
        route         TYPE ekpv-route, " Itinerário
        kbetr         TYPE konp-kbetr, " Vlr frete
        lifnr         TYPE vbpa-lifnr , " Agente Frete
        st_proc       TYPE zsdt0001-st_proc,
        shtyp         TYPE zsdt0011-shtyp,
        cont_fre      TYPE i,
        remessa(45),
*        REMESSA(10),
        fatura(10),
        danfe(10),
        transp(15),
        doccus(10),
        ovserv(10),
        fatserv(10),
        dacte(10),
        tipo(1),
        matnr         TYPE zsdt0001-matnr,
* ---> S4 Migração - 19/06/2023 - FC - Inicio
        "MATERIAL(60),
        material(81),
* <--- S4 Migração - 19/06/2023 - FC - Fim
        id_ordem      TYPE zde_id_ordem,
        line_color(4) TYPE c, "Used to store row color attributes
        color_cell    TYPE lvc_t_scol,  " Cell color
        style         TYPE lvc_t_styl,
      END OF ty_saida.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA: it_a900 TYPE TABLE OF a900,
      wa_a900 TYPE a900,

      it_a910 TYPE TABLE OF a910,
      wa_a910 TYPE a910,

      it_a911 TYPE TABLE OF a911,
      wa_a911 TYPE a911,

      it_a915 TYPE TABLE OF a915,
      wa_a915 TYPE a915,

      it_a918 TYPE TABLE OF a918,
      wa_a918 TYPE a918,

      it_a919 TYPE TABLE OF a919,
      wa_a919 TYPE a919,

      it_konp TYPE TABLE OF konp,
      wa_konp TYPE konp,

      w_vbap  TYPE vbap.

DATA: it_zsdt0001       TYPE TABLE OF ty_zsdt0001,
      it_zsdt0001_fre   TYPE TABLE OF ty_zsdt0001,
      it_zsdt0011_o     TYPE TABLE OF zsdt0011,
      it_zsdt0011_p     TYPE TABLE OF zsdt0011,
      it_zlest0132      TYPE TABLE OF zlest0132,
      it_mara           TYPE TABLE OF ty_mara,
      it_vbak           TYPE TABLE OF ty_vbak,
      it_tvakt          TYPE TABLE OF ty_tvakt,
      it_t161t          TYPE TABLE OF ty_t161t,
      it_vbkd           TYPE TABLE OF ty_vbkd,
      it_tvtk           TYPE TABLE OF ty_tvtk,
      it_ekko           TYPE TABLE OF ty_ekko,
      it_ekpo           TYPE TABLE OF ty_ekpo,
      it_vbpa           TYPE TABLE OF ty_vbpa,
      it_vbpa_2         TYPE TABLE OF ty_vbpa,
      it_kna1           TYPE TABLE OF ty_kna1,
      it_lfa1           TYPE TABLE OF ty_lfa1,
      it_t001w          TYPE TABLE OF ty_t001w,
      it_makt           TYPE TABLE OF ty_makt,
      it_vbpa_cr        TYPE TABLE OF ty_vbpa_cr, "Ponto de coleta  REMESSA
      it_vbpa_co        TYPE TABLE OF ty_vbpa_co, "Ponto de coleta  ORDEM
      it_ekpa_pr        TYPE TABLE OF ty_ekpa_pr, "Ponto de coleta  Pedido
      it_vbap           TYPE TABLE OF ty_vbap, "Itinerário  ORDE M
      it_ekpv           TYPE TABLE OF ty_ekpv, "Itinerário  PEDIDO
      it_veic           TYPE TABLE OF ty_veic,


      ti_zlest0100      TYPE TABLE OF zlest0100  WITH HEADER LINE,
      it_zlest0100      TYPE TABLE OF zlest0100  WITH HEADER LINE,
      t_auart           TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
      t_fatura_agrupada TYPE TABLE OF zsdt0121,
      t_usermd          TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
      wa_msg            TYPE          bdcmsgcoll,

      it_saida          TYPE TABLE OF ty_saida WITH HEADER LINE,
      tl_texto          TYPE catsxt_longtext_itab,
      wl_texto          TYPE LINE OF catsxt_longtext_itab,
      it_color          TYPE TABLE OF lvc_s_scol,
      wa_style          TYPE lvc_s_styl,
      style             TYPE lvc_t_styl WITH HEADER LINE.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: wa_zsdt0001      TYPE ty_zsdt0001,
      wa_zsdt0011      TYPE zsdt0011,
      wa_zlest0132     TYPE zlest0132,
      wa_mara          TYPE ty_mara,
      wa_vbak          TYPE ty_vbak,
      wa_tvakt         TYPE ty_tvakt,
      wa_t161t         TYPE ty_t161t,
      wa_vbkd          TYPE ty_vbkd,
      wa_tvtk          TYPE ty_tvtk,
      wa_ekko          TYPE ty_ekko,
      wa_ekpo          TYPE ty_ekpo,
      wa_vbpa          TYPE ty_vbpa,
      wa_vbpa_2        TYPE ty_vbpa,
      wa_zdco_produtor TYPE zdco_produtor,
      wa_kna1          TYPE ty_kna1,
      wa_lfa1          TYPE ty_lfa1,
      w_lfa1           TYPE lfa1,
      wa_t001w         TYPE ty_t001w,
      wa_makt          TYPE ty_makt,
      wa_t005s         TYPE t005s,
      wa_vbpa_cr       TYPE ty_vbpa_co, "Ponto de coleta  REMESSA
      wa_vbpa_co       TYPE ty_vbpa_co, "Ponto de coleta  ORDEM
      wa_ekpa_pr       TYPE ty_ekpa_pr, "Ponto de coleta  Pedido
      wa_vbap          TYPE ty_vbap, "Itinerário  ORDE M
      wa_ekpv          TYPE ty_ekpv, "Itinerário  PEDIDO
      wa_trans         TYPE ty_trans, "Transportadora
      wa_mot           TYPE ty_mot , "Motorista
      wa_ag_frete      TYPE ty_ag_frete , "Agente de Frete
      wa_likp          TYPE likp , "Remessa
      v_xblnr          TYPE likp-xblnr,
      wa_zlest0100     TYPE zlest0100,
      wa_zsdt0151      TYPE zsdt0151,
      wa_zcte_ciot     TYPE zcte_ciot,
      wa_saida         TYPE ty_saida,
      wa_saida2        TYPE ty_saida,
      wa_color         TYPE lvc_s_scol.


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*

DATA: editcontainer      TYPE REF TO cl_gui_custom_container,
      cl_container       TYPE REF TO cl_gui_custom_container,
      cl_container_95    TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id      TYPE REF TO cl_dd_document,
      cl_grid            TYPE REF TO cl_gui_alv_grid,

      container          TYPE REF TO cl_gui_custom_container,
      picture            TYPE REF TO cl_gui_picture,
      url(255)           TYPE c,
      graphic_url(255),

      grid1              TYPE REF TO cl_gui_alv_grid,
      grid2              TYPE REF TO cl_gui_alv_grid,
      obg_conteiner      TYPE REF TO cl_gui_custom_container,
      obg_conteiner_veic TYPE REF TO cl_gui_custom_container,

      wa_stable          TYPE lvc_s_stbl,
      is_stable          TYPE lvc_s_stbl VALUE 'XX',

      wa_afield          TYPE lvc_s_fcat,
      it_fieldcat        TYPE lvc_t_fcat,
      w_fieldcat         TYPE lvc_s_fcat,
      it_except_qinfo    TYPE lvc_t_qinf,
      wa_except_qinfo    TYPE lvc_s_qinf,

      gt_f4              TYPE lvc_t_f4 WITH HEADER LINE,
      i_sort             TYPE lvc_t_sort,
      wa_layout          TYPE lvc_s_layo,

      gs_variant_c       TYPE disvariant,
      ok-code            TYPE sy-ucomm,

      wa_estrutura       TYPE ty_estrutura,
      estrutura          TYPE TABLE OF ty_estrutura.


DATA: BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

DATA: l_graphic_conv TYPE i.
DATA: l_graphic_offs TYPE i.
DATA: graphic_size   TYPE i.
DATA: l_graphic_xstr TYPE xstring.

DATA lo_container      TYPE REF TO cl_gui_custom_container.
DATA lo_pic            TYPE REF TO cl_gui_picture.
DATA: g_custom_cont_desc TYPE REF TO cl_gui_custom_container,
      obg_descbox        TYPE REF TO cl_gui_textedit,
      g_descbox          TYPE scrfname VALUE 'CC_DESC',
      txt_correc         TYPE c LENGTH 1000,
      tg_editor          TYPE TABLE OF ty_editor,
      wl_cont            TYPE sy-tabix,
      wl_cont_aux        TYPE sy-tabix,
      wl_cont_aux2       TYPE sy-tabix,
      wg_editor          TYPE ty_editor.

*&--------------------------------------------------------------------&*
*& Variáveis                                                          &*
*&--------------------------------------------------------------------&*
DATA:
  wg_exit(1)       TYPE c,
  wg_save(1)       TYPE c,
  wg_documento(10),
  w_answer(1),
  wl_erro(1),
  wl_ordemt        TYPE thead-tdname,
  vl_delivery      TYPE bapishpdelivnumb-deliv_numb,
  v_docnum         TYPE j_1bnfe_active-docnum,
  vl_vbeln         TYPE vbfa-vbeln,
  vl_fksto         TYPE vbrk-fksto,
  vl_mjahr         TYPE vbfa-mjahr,
  vl_refkey        TYPE j_1bnflin-refkey,
  vl_fknum         TYPE zsdt0001-fknum,
  vl_ov_frete      TYPE zsdt0001-ov_frete,
  v_tknum          TYPE vttk-tknum,
  vl_fatura_frete  TYPE zsdt0001-fatura_frete,
  sperr_user       TYPE sy-msgv1,

  vl_bukrs         TYPE j_1bnfdoc-bukrs,
  vl_docnum        TYPE j_1bnflin-docnum,
  w_erro(1),

  vl_message       TYPE char600,
  vl_ponteiro      TYPE zlest0100-cont,
  vcandat          TYPE j_1bnfdoc-candat.



DATA: t_success         TYPE STANDARD TABLE OF bapivbrksuccess WITH HEADER LINE,
      w_success         LIKE LINE OF t_success,
      t_billing         TYPE STANDARD TABLE OF bapivbrk WITH HEADER LINE,
      w_billing         LIKE LINE OF t_billing,
      wa_setleaf        TYPE setleaf,
      t_textdatain      TYPE STANDARD TABLE OF bapikomfktx WITH HEADER LINE,
      w_textdatain      LIKE LINE OF t_textdatain,
      x_header          TYPE thead,
      it_lines          TYPE STANDARD TABLE OF tline WITH HEADER LINE,
      wa_lines          LIKE LINE OF it_lines,

      tl_bapiparex      TYPE TABLE OF bapiparex,
      sl_bapiparex      TYPE bapiparex,
      wl_orderheaderin  TYPE bapisdh1,
      wl_orderheaderinx TYPE bapisdh1x,
      wl_bape_vbak      TYPE bape_vbak,
      wl_bape_vbakx     TYPE bape_vbakx,
      t_itemdata        TYPE TABLE OF bapishipmentitem,
      st_itemdata       TYPE bapishipmentitem,
      t_return_vt       LIKE bapiret2 OCCURS 0 WITH HEADER LINE,

      zid               TYPE thead-tdid,
      zname             TYPE thead-tdname,

      t_return          TYPE STANDARD TABLE OF bapireturn1 WITH HEADER LINE.

*&--------------------------------------------------------------------&*
*& SHDB                                                               &*
*&--------------------------------------------------------------------&*
DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      wa_bdcdata LIKE LINE OF ti_bdcdata.


*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS:
         c_x               TYPE c VALUE 'X'.

FIELD-SYMBOLS: <wdlv>   TYPE any. "BAPISHPDELIVNUMB-DELIV_NUMB.


*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECT-OPTIONS: s_chave FOR zsdt0001-ch_referencia NO-DISPLAY.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_bukrs  TYPE zsdt0001-bukrs OBLIGATORY,  "Empresa
              p_branch TYPE zsdt0001-branch OBLIGATORY. "Filial

  SELECT-OPTIONS: s_vbeln FOR vbak-vbeln,                             "Ordem de venda
                  s_ebeln FOR ekko-ebeln,                             "Pedido
                  s_matnr FOR zsdt0001-matnr,                         "Produto
                  s_inco1 FOR vbkd-inco1,                             "Tipo Frete
                  s_lifnr FOR lfa1-lifnr NO INTERVALS ,               "Agente de frete
                  s_data  FOR zsdt0001-dt_movimento,                  "Data de Movimento
                  s_doc   FOR zsdt0001-doc_transp.                    "Documento Transporte
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS:
    r_dt_a RADIOBUTTON GROUP rad1 USER-COMMAND act DEFAULT 'X',
    r_dt_f RADIOBUTTON GROUP rad1.
SELECTION-SCREEN: END OF BLOCK b2.

PARAMETERS: p_inter TYPE zsdt0001-id_interface NO-DISPLAY.


INITIALIZATION.

  DATA: show_msg_desativ    TYPE c LENGTH 100,
        show_msg_desativ_01 TYPE c LENGTH 100.

  CLEAR: show_msg_desativ.

  IF 1 = 2.
    DELETE FROM MEMORY ID 'MSG_DESATIV_ZLES0106'.
  ENDIF.

  show_msg_desativ_01 = sy-uname && sy-datum.

  IMPORT show_msg_desativ FROM MEMORY ID 'MSG_DESATIV_ZLES0106'.

  IF sy-datum <= '20181202'.

    IF show_msg_desativ_01 NE show_msg_desativ.

      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Atenção'
          txt1  = 'Essa transação será desativada em 03/12/2018! A nova transação que entrará em'
          txt2  = 'uso é a ZLES0136 que já pode ser utilizada! Caso não possua acesso, favor'
          txt3  = 'abrir um S.A no SoftExpert! Além do acesso a transação, é necessário incluir o'
          txt4  = 'objeto de autorização ZLES0136 no perfil - Campo: ZTP_FT_ROM - Valor Campo: 01!'.

    ENDIF.

  ELSE.

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Atenção'
        txt1  = 'Essa transação foi desativada! Usar a transação ZLES0136! Caso não possua'
        txt2  = 'acesso, favor abrir um S.A no SoftExpert! Além do acesso a transação,'
        txt3  = 'é necessário incluir o objeto de autorização ZLES0136 no'
        txt4  = 'perfil - Campo: ZTP_FT_ROM - Valor Campo: 01!'.

    LEAVE PROGRAM.

  ENDIF.

  show_msg_desativ = sy-uname && sy-datum.
  EXPORT show_msg_desativ TO MEMORY ID 'MSG_DESATIV_ZLES0106'.



* CONCATENATE 'ZLESR0086' SY-UNAME INTO  L_VAR.
*
*  CALL FUNCTION 'ENQUEUE_ESINDX'
*    EXPORTING
*      MODE_INDX      = 'E'
*      MANDT          = SY-MANDT
*      RELID          = 'ZZ'
*      SRTFD          = L_VAR
*    EXCEPTIONS
*      FOREIGN_LOCK   = 1
*      SYSTEM_FAILURE = 2
*      OTHERS         = 3.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

  s_data-low    = sy-datum.
  s_data-sign   = 'I'.
  s_data-option = 'EQ'.
  APPEND s_data.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_branch IS NOT INITIAL.
      AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
        ID 'WERKS' FIELD  p_branch
        ID 'ACTVT' FIELD '03'.    "Alteração

      CASE sy-subrc.
        WHEN 0.
*  tem autorização!
        WHEN 4.
          MESSAGE 'Sem autorização para esta filial' TYPE 'I'.
          SET CURSOR FIELD 'P_BRANCH'.
        WHEN 12.
          MESSAGE 'Sem autorização neste objeto ' TYPE 'I'.
          SET CURSOR FIELD 'P_BRANCH'.
        WHEN OTHERS.
      ENDCASE.

    ENDIF.

    IF  s_data IS INITIAL
        AND r_dt_f = 'X'
        AND p_bukrs IS NOT INITIAL
        AND p_branch IS NOT INITIAL.
      MESSAGE 'Informe a Data de Movimento.'
                                             TYPE 'W'.
      SET CURSOR FIELD 'S_DATA-LOW' .
    ENDIF.

    "Campo agente frete
    IF ( screen-name CS 'BUKRS' OR screen-name CS 'BRANCH' OR screen-name CS 'VBELN' OR screen-name CS 'EBELN' OR screen-name CS 'MATNR' OR screen-name CS 'INCO1' OR screen-name CS 'DATA' OR screen-name CS 'DOC' ) OR ( NOT screen-name CS 'S_LIFNR' ).
      IF ( sy-tcode = 'ZLES0106' ) OR ( sy-tcode = 'ZMM0127' ).
        screen-invisible = 0.
        screen-input = 1.
        MODIFY SCREEN.
      ELSE.
        screen-invisible = 0.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF  screen-name CS 'S_LIFNR'.
      IF sy-tcode = 'ZLES0107'.
        screen-invisible = 0.
        screen-input = 1.
        MODIFY SCREEN.
      ELSE.
        screen-invisible = 1.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

  ENDLOOP.


*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      catch_hotspot
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id
                  es_row_no.
    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.
    CLASS-METHODS:
      on_f4                      FOR EVENT onf4                 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

    CLASS-METHODS:
      set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING
        e_ucomm.


ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD catch_hotspot.
    DATA wl_tvro      TYPE tvro.
    DATA shipment_row TYPE sy-tabix.
    DATA current_row  TYPE sy-tabix.
    DATA lw_saida_aux TYPE ty_saida.
    DATA wa_zsdt0023  TYPE zsdt0023.
    DATA v_werks      TYPE ekpo-werks.
    DATA v_lifnr      TYPE ekko-lifnr.
    DATA v_centro_real  TYPE zsdt_depara_cen-centro_real.
    DATA v_error_mov_sobra TYPE c.
    DATA v_charg      TYPE mch1-charg.
    DATA v_matnr      TYPE mara-matnr.
    DATA v_tenta      TYPE i.

    FIELD-SYMBOLS <fs_saida> TYPE ty_saida.

    READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
    IF sy-subrc = 0.
      IF e_column_id = 'ICON'.
        IF wa_saida-icon = icon_led_red.
          SELECT *
            FROM zlest0100
            INTO TABLE ti_zlest0100
            WHERE ch_referencia = wa_saida-ch_referencia.

          IF ti_zlest0100[] IS NOT INITIAL.
            PERFORM montar_layout.
            CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
              EXPORTING
                it_fieldcat           = estrutura[]
                i_save                = 'A'
                i_screen_start_column = 3
                i_screen_start_line   = 3
                i_screen_end_column   = 100
                i_screen_end_line     = 13
              TABLES
                t_outtab              = ti_zlest0100.
          ENDIF.

        ENDIF.
      ELSEIF e_column_id = 'REMESSA'.
        shipment_row = e_row_id-index.
        IF ( wa_saida-remessa EQ icon_execute_object ).
          CLEAR v_tenta.
          WHILE shipment_row IS NOT INITIAL.
            READ TABLE it_saida INTO wa_saida INDEX shipment_row.

            current_row = sy-tabix.

            ADD 1 TO v_tenta.

            CLEAR wa_zsdt0001.
            SELECT SINGLE *
              FROM zsdt0001
              INTO wa_zsdt0001
             WHERE ch_referencia = wa_saida-ch_referencia.

            v_charg = wa_zsdt0001-nr_safra.

            IF wa_saida-matnr IS NOT INITIAL.
              CALL FUNCTION 'ENQUEUE_EMMCH1E'
                EXPORTING
                  mode_mch1      = 'E'
                  mandt          = sy-mandt
                  matnr          = wa_saida-matnr
                  charg          = v_charg
                  _scope         = '2'
                EXCEPTIONS
                  foreign_lock   = 1
                  system_failure = 2
                  OTHERS         = 3.

              sperr_user     = sy-msgv1.
              IF sy-subrc <> 0.
                IF v_tenta GE 30.
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                    EXPORTING
                      input  = wa_saida-matnr
                    IMPORTING
                      output = v_matnr.
                  MESSAGE |Material { v_matnr } para o lote { v_charg }, bloqueado por { sperr_user }.| TYPE 'I'.
                  EXIT.
                ENDIF.
                WAIT UP TO 1 SECONDS.
                CONTINUE.
              ENDIF.

              CALL FUNCTION 'DEQUEUE_EMMCH1E'
                EXPORTING
                  mode_mch1 = 'E'
                  mandt     = sy-mandt
                  matnr     = wa_saida-matnr
                  charg     = v_charg.
            ENDIF.

            CLEAR:shipment_row.

            CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
              EXPORTING
                percentage = sy-tabix
                text       = |Criando documento de remessa para o romaneio { CONV i( wa_saida-nr_romaneio ) }.|.


            CLEAR: v_centro_real,v_werks.
            IF wa_saida-tipo = 'O'.
              SELECT SINGLE werks
                FROM vbap
                INTO v_werks
                WHERE vbeln = wa_zsdt0001-vbeln.
              IF wa_zsdt0001-branch NE v_werks.
                SELECT SINGLE centro_real
                  INTO v_centro_real
                  FROM zsdt_depara_cen
                WHERE  centrov_1 = v_werks.
                IF wa_zsdt0001-branch NE v_centro_real.
                  MESSAGE 'Centro emissor do romaneio diferente do centro emissor da OV' TYPE 'I'.
                  EXIT.
                ENDIF.
              ENDIF.
            ELSE.
              SELECT SINGLE reswk
                 FROM ekko
                 INTO v_werks
                 WHERE ebeln = wa_zsdt0001-vbeln.

              IF wa_zsdt0001-branch NE v_werks.
                MESSAGE 'Centro emissor do romaneio diferente do centro emissor do pedido.' TYPE 'I'.
                EXIT.
              ENDIF.
            ENDIF.


            IF wa_zsdt0001-doc_rem GT 0.
              MESSAGE 'Documento atualizado, click em <ATUALIZAR>' TYPE 'I'.
              EXIT.
            ELSEIF  wa_saida-inco1 = 'CIF'.
              IF wa_zsdt0001-motorista IS INITIAL .
                MESSAGE 'Motorista não informado, reenviar romaneio' TYPE 'I'.
                EXIT.
              ELSEIF wa_zsdt0001-placa_cav IS INITIAL.
                MESSAGE 'Placa cavalo não informada, reenviar romaneio' TYPE 'I'.
                EXIT.
              ENDIF.
            ENDIF.

            v_xblnr = wa_saida-ch_referencia.
            SELECT SINGLE *
              FROM likp
              INTO wa_likp
             WHERE xblnr = v_xblnr
               AND spe_loekz = ''.

            IF sy-subrc = 0.
              MESSAGE |Já existe a remessa { wa_likp-vbeln } para o romaneio { wa_saida-nr_romaneio }, estorne.| TYPE 'I'.
              wa_saida-remessa = wa_likp-vbeln.
              wa_saida-st_proc = '01'.
              MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING remessa st_proc.
              UPDATE zsdt0001 SET doc_rem = wa_likp-vbeln
                                  st_proc = '01'
                WHERE ch_referencia = wa_saida-ch_referencia.

              CALL METHOD cl_grid->refresh_table_display
                EXPORTING
                  is_stable = wa_stable.
              EXIT.
            ENDIF.

            IF sy-tcode NE 'ZLES0106' AND sy-tcode NE 'ZMM0127'.
              MESSAGE 'Transação apenas de visualização' TYPE 'I'.
              EXIT.
            ENDIF.

            IF wa_saida-lifnr IS INITIAL AND t_fatura_agrupada IS INITIAL.
              MESSAGE 'Informar o agente de frete!' TYPE 'I'.
            ELSEIF wa_saida-region IS INITIAL AND t_fatura_agrupada IS INITIAL.
              MESSAGE 'Informar a UF da placa cavalo!' TYPE 'I'.
            ELSE.
              IF t_fatura_agrupada IS INITIAL.
                SELECT SINGLE *
                  FROM t005s
                  INTO wa_t005s
                  WHERE land1 = 'BR'
                  AND   bland = wa_saida-region.
                IF sy-subrc NE 0.
                  MESSAGE |UF { wa_saida-region } é inválida!| TYPE 'I'.
                  EXIT.
                ENDIF.
              ENDIF.

              IF ( wa_saida-inco1 = 'CPT' ).
                UPDATE zsdt0001 SET agente_frete = wa_saida-lifnr region = wa_saida-region
                 WHERE ch_referencia = wa_saida-ch_referencia.

                wa_saida2 = wa_saida.

                REFRESH it_saida.
                CALL METHOD cl_grid->refresh_table_display.

                PERFORM: f_seleciona_dados, "//Form seleciona dados
                         f_saida.           "//Form de saida

                CALL METHOD cl_grid->refresh_table_display.

                READ TABLE it_saida INTO wa_saida WITH KEY ch_referencia = wa_saida2-ch_referencia.
                IF sy-subrc IS NOT INITIAL.
                  MESSAGE |Chave de referência { wa_saida2-ch_referencia } não localizada na seleção (CPT).| TYPE 'I'.
                  EXIT.
                ELSE.
                  wa_saida-lifnr = wa_saida2-lifnr.
                  MODIFY it_saida INDEX sy-tabix FROM wa_saida TRANSPORTING lifnr.
                ENDIF.
              ENDIF.

              "//Validação para campo "relevância para transporte"
              IF ( 'CPT_CIF' CS wa_saida-inco1 ).
                CLEAR wl_tvro.

                SELECT SINGLE *
                  INTO wl_tvro
                  FROM tvro
                  WHERE route EQ wa_saida-route.

                IF wl_tvro-tdiix IS INITIAL.
                  MESSAGE |Itinerário { wa_saida-route } sem relevância para transporte. Solicite regularização para à logística.| TYPE 'I'.
                  EXIT.
                ENDIF.
              ENDIF.

              SELECT SINGLE lifnr name1 dlgrp
                FROM lfa1
                INTO wa_lfa1
               WHERE lifnr = wa_saida-lifnr.

              IF wa_lfa1-dlgrp NE '0001' AND wa_saida-inco1 NE 'FOB' AND wa_saida-inco1 NE 'CFR'.
                CLEAR: wa_saida-lifnr, wa_saida-region.
                MODIFY it_saida INDEX current_row FROM wa_saida TRANSPORTING lifnr region.

                CALL METHOD cl_grid->refresh_table_display
                  EXPORTING
                    is_stable = wa_stable.

                UPDATE zsdt0001 SET agente_frete = '' region =  ''
                 WHERE ch_referencia = wa_saida-ch_referencia.

                MESSAGE |Fornecedor { wa_saida-lifnr } não configurado como agente de frete. Solicite ajuste à central de cadastro.| TYPE 'I'.
                EXIT.
              ENDIF.

              IF ( wa_saida-tipo = 'O' AND  wa_saida-operacao+0(4) = 'ZRDC' ). "DCO
                SELECT SINGLE *
                  FROM zdco_produtor
                  INTO wa_zdco_produtor
                 WHERE vbeln       = wa_saida-vbeln
                   AND cd_material = wa_saida-matnr
                   AND cd_centro   = wa_saida-branch.

                IF sy-subrc NE 0.
                  UPDATE zsdt0001 SET agente_frete = '' region =  ''
                   WHERE ch_referencia = wa_saida-ch_referencia.

                  MESSAGE 'DCO não cadastrado para essa Ordem de Venda. Contactar o mercado interno' TYPE 'I'.
                  EXIT.
                ENDIF.
              ENDIF.

              "Bloqueia romaneio
              CALL FUNCTION 'ENQUEUE_EZSDT0001'
                EXPORTING
                  ch_referencia  = wa_saida-ch_referencia
                EXCEPTIONS
                  foreign_lock   = 1
                  system_failure = 2
                  OTHERS         = 3.

              IF sy-subrc <> 0.
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              ENDIF.

              UPDATE zsdt0001 SET st_proc = wa_saida-st_proc region = wa_saida-region
               WHERE ch_referencia = wa_saida-ch_referencia.

              wa_saida-line_color = 'C310'.

              REFRESH it_color.
              MOVE 'REMESSA'  TO wa_color-fname.
              MOVE '5'        TO wa_color-color-col.
              MOVE '1'        TO wa_color-color-int.
              MOVE '1'        TO wa_color-color-inv.
              APPEND wa_color TO it_color.

              wa_saida-color_cell[] = it_color[].
              MODIFY it_saida FROM wa_saida INDEX current_row TRANSPORTING color_cell line_color .

*              CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY
*                EXPORTING
*                  IS_STABLE = WA_STABLE.

              SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_saida-ch_referencia.
              IF wa_saida-tipo = 'O'.
                SUBMIT zsdi0009 WITH p_vbeln = wa_saida-vbeln
                                WITH p_lifnr = wa_saida-lifnr
                AND RETURN.
              ELSE.
                SUBMIT ztransf WITH p_ebeln = wa_saida-vbeln
                               WITH p_lifnr = wa_saida-lifnr
                AND RETURN.
              ENDIF.

              CLEAR vl_delivery.
              GET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_delivery.

              IF ( vl_delivery IS NOT INITIAL ).
                IF ( vl_delivery = '9999999999' ). "Erro de bloqueio no OPUS
                  shipment_row = current_row.
                  CONTINUE.
                ENDIF.
                wa_saida-remessa = vl_delivery. " Já grava em ZSDT0001-DOC_REM

                IF wa_saida-tipo = 'P'.
                  SELECT SINGLE vbeln mjahr
                    INTO (vl_vbeln,vl_mjahr)
                    FROM vbfa
                   WHERE vbelv = vl_delivery
                     AND vbtyp_n  = 'R'
                     AND vbtyp_v  = 'J'.

                  wa_saida-fatura = vl_vbeln.
                ENDIF.

                IF wa_saida-lifnr IS NOT INITIAL.
                  REFRESH: style.
                  CLEAR: wa_style.

                  wa_style-fieldname = 'LIFNR'.
                  wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.

                  DELETE wa_saida-style WHERE fieldname EQ 'LIFNR'.
                  INSERT  wa_style INTO TABLE style .
                  wa_saida-style[] = style[].
                ENDIF.

                MODIFY it_saida FROM wa_saida INDEX current_row TRANSPORTING remessa fatura style.

                IF line_exists( t_fatura_agrupada[ werks = p_branch kunnr = wa_saida-kunnr inco1 = wa_saida-inco1 ] ).
                  LOOP AT it_saida INTO it_saida WHERE ( dt_movimento EQ wa_saida-dt_movimento ) AND
                                                       ( matnr        EQ wa_saida-matnr        ) AND
                                                       ( kunnr        EQ wa_saida-kunnr        ) AND
                                                       ( operacao(4)  EQ wa_saida-operacao(4)  ) AND
                                                       ( inco1        EQ wa_saida-inco1        ) AND
                                                       ( remessa      IS INITIAL               ).

                    shipment_row = sy-tabix.
                    EXIT.
                  ENDLOOP.
                ENDIF.

*                CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY
*                  EXPORTING
*                    IS_STABLE = WA_STABLE.

                "//Grava vinculo DCO
                IF wa_saida-tipo = 'O' AND  wa_saida-operacao+0(4) = 'ZRDC'. "DCO
                  SUBMIT zsdi0006 WITH p_vbeln = wa_saida-remessa(10)
                                  WITH p_vinc  = 'X'
                  AND RETURN.
                ENDIF.

                IF wa_saida-tipo = 'O'.
                  UPDATE zsdt0001 SET st_proc = '01' agente_frete = wa_saida-lifnr
                   WHERE ch_referencia = wa_saida-ch_referencia.

                  wa_saida-st_proc = '01'.
                  MODIFY it_saida FROM wa_saida INDEX current_row TRANSPORTING st_proc.

                ELSE.
                  IF wa_saida-inco1 = 'FOB' OR wa_saida-inco1 = 'CFR'. " Finaliza processo com a Danfe autorizada
                    UPDATE zsdt0001 SET st_proc = '99' " Finalizado
                                        fatura_prod = wa_saida-fatura
                                        agente_frete = wa_saida-lifnr
                    WHERE ch_referencia = wa_saida-ch_referencia.

                    "apaga log Erros
                    CLEAR wa_saida-icon.
*                  DELETE FROM ZLEST0100 WHERE CH_REFERENCIA = WA_SAIDA-CH_REFERENCIA.
                    wa_saida-st_proc = '99'.
                    MODIFY it_saida FROM wa_saida INDEX current_row TRANSPORTING icon st_proc.

*                    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY
*                      EXPORTING
*                        IS_STABLE = WA_STABLE.

                  ELSE.
                    UPDATE zsdt0001 SET st_proc      = '02' " Danfe mudanã para 02 fatura até aprovação sefaz
                                        fatura_prod  = wa_saida-fatura
                                        agente_frete = wa_saida-lifnr
                    WHERE ch_referencia = wa_saida-ch_referencia.

                    wa_saida-st_proc = '02'.
                    MODIFY it_saida FROM wa_saida INDEX current_row TRANSPORTING st_proc.
                  ENDIF.
                ENDIF.

              ELSE.
                CLEAR: wa_saida-lifnr, wa_saida-region.
                MODIFY it_saida INDEX current_row FROM wa_saida TRANSPORTING lifnr region.

                UPDATE zsdt0001 SET agente_frete = '' region =  ''
                 WHERE ch_referencia = wa_saida-ch_referencia.
              ENDIF.

              "//Desbloqueia romaneio
              CALL FUNCTION 'DEQUEUE_EZSDT0001'
                EXPORTING
                  ch_referencia = wa_saida-ch_referencia.
            ENDIF.
            "
          ENDWHILE.

          CALL METHOD cl_grid->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

        ELSEIF wa_saida-remessa IS NOT INITIAL AND wa_saida-remessa(1) NE '@'.
          CLEAR shipment_row.
          SET PARAMETER ID 'VL'    FIELD wa_saida-remessa(10).
          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
        ENDIF.

      ELSEIF e_column_id = 'FATURA'.
        IF wa_saida-fatura = icon_execute_object.
          CLEAR wa_zsdt0001.

          SELECT SINGLE vbeln mjahr
            INTO (vl_vbeln,vl_mjahr)
            FROM vbfa
           WHERE vbelv = wa_saida-remessa(10)
             AND vbtyp_n  = 'R'
             AND vbtyp_v  = 'J'.
          IF sy-subrc NE 0.
            MESSAGE 'Picking não realizado, estorne a remessa' TYPE 'I'.
            EXIT.
          ELSE.
            SELECT SINGLE vbeln mjahr
                 INTO (vl_vbeln,vl_mjahr)
                 FROM vbfa
                WHERE vbelv = wa_saida-remessa(10)
                  AND vbtyp_n  = 'h'
                  AND vbtyp_v  = 'J'.
            IF sy-subrc EQ 0.
              MESSAGE 'Picking estornado, estorne a remessa' TYPE 'I'.
              EXIT.
            ENDIF.
          ENDIF.
          "checa o picking
          IF wa_saida-operacao+0(4) EQ 'ZRDC' OR
             wa_saida-operacao+0(4) EQ 'ZRFL' .
            "MBST (estorno de migo)
            CLEAR wa_zsdt0023.
            SELECT SINGLE *
                FROM zsdt0023
                INTO wa_zsdt0023
                WHERE vbeln   = wa_saida-remessa(10).
            IF wa_zsdt0023-mblnr_s IS INITIAL.
              MESSAGE 'Picking não realizado, estorne a remessa' TYPE 'I'.
              EXIT.
            ELSEIF wa_zsdt0023-es_mblnr_s IS NOT INITIAL.
              MESSAGE 'Picking estornado, estorne a remessa' TYPE 'I'.
              EXIT.
            ENDIF.
          ENDIF.

          SELECT SINGLE *
            FROM zsdt0001
            INTO wa_zsdt0001
           WHERE ch_referencia = wa_saida-ch_referencia.

          IF wa_zsdt0001-fatura_prod GT 0.
            MESSAGE 'Documento atualizado, click em <ATUALIZAR>' TYPE 'I'.
            EXIT.
          ENDIF.

          IF sy-tcode NE 'ZLES0106' AND sy-tcode NE 'ZMM0127'.
            MESSAGE 'Transação apenas de visualização' TYPE 'I'.
            EXIT.
          ENDIF.

          "Check se Movimento Sobra foi gerado 11.09.2017
          PERFORM f_check_mov_sobra USING wa_saida-ch_referencia
                                 CHANGING v_error_mov_sobra.
          CHECK v_error_mov_sobra IS INITIAL.
          "Fim Check Sobra

          IF wa_saida-remessa = icon_execute_object.
            MESSAGE 'Gerar a Remessa!' TYPE 'I'.
          ELSE.

            IF line_exists( t_fatura_agrupada[ werks = p_branch kunnr = wa_saida-kunnr inco1 = wa_saida-inco1 ] ).
              TRY.
                  it_saida = it_saida[ dt_movimento = wa_saida-dt_movimento
                                       matnr        = wa_saida-matnr
                                       kunnr        = wa_saida-kunnr
                                       operacao(4)  = wa_saida-operacao(4)
                                       inco1        = wa_saida-inco1
                                       remessa      = icon_execute_object
                                     ].

                  MESSAGE |Para prosseguir com a fatura agrupada é necessário gerar o doc.remessa do romaneio { it_saida-nr_romaneio }.|
                     TYPE 'I' DISPLAY LIKE 'W'.

                  EXIT.
                CATCH cx_sy_itab_line_not_found.
              ENDTRY.
            ENDIF.

            IF wa_saida-tipo = 'O' AND wa_saida-operacao+0(4) = 'ZRDC'. "DCO
              SELECT SINGLE vbeln
                FROM zdco_vinculo
                INTO vl_vbeln
               WHERE vbeln EQ wa_saida-remessa(10).

              IF NOT sy-subrc IS INITIAL.
                MESSAGE 'Remessa sem vinculo com DCO.' TYPE 'I'.
                EXIT.
              ENDIF.
            ENDIF.

            IF wa_saida-inco1 = 'CPT'.
              IF wa_saida-kbetr LE 0.
                MESSAGE i000(z01) WITH 'Não existe valor de frete cadastrado.'
                                       'Solicite à transportadora da sua região'.
                EXIT.
              ENDIF.
            ENDIF.

            "//Bloqueia romaneio
            CALL FUNCTION 'ENQUEUE_EZSDT0001'
              EXPORTING
                ch_referencia  = wa_saida-ch_referencia
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

            UPDATE zsdt0001 SET st_proc = wa_saida-st_proc
             WHERE ch_referencia = wa_saida-ch_referencia.

            wa_saida-line_color = col_yellow_int.
            wa_saida-color_cell = VALUE #( ( fname = 'FATURA'
                                             color-col = col_positive
                                             color-int = col_heading
                                             color-inv = col_heading )
                                         ).

            MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING color_cell line_color.
            REFRESH: t_billing,it_lines,t_textdatain,t_return,t_success.

            "//Buscar Texto da ordem venda
            wl_ordemt = wa_saida-vbeln.
            CALL FUNCTION 'READ_TEXT'
              EXPORTING
                id                      = '0002'
                language                = sy-langu
                name                    = wl_ordemt
                object                  = 'VBBK'
              TABLES
                lines                   = it_lines
              EXCEPTIONS
                id                      = 1
                language                = 2
                name                    = 3
                not_found               = 4
                object                  = 5
                reference_check         = 6
                wrong_access_to_archive = 7
                OTHERS                  = 8.

            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                text_question         = 'Informar texto da fatura?'
                text_button_1         = 'Sim'(100)
                icon_button_1         = 'ICON_OKAY '
                text_button_2         = 'Não'(101)
                icon_button_2         = 'ICON_CANCEL'
                default_button        = '1'
                display_cancel_button = ' '
                start_column          = 25
                start_row             = 6
              IMPORTING
                answer                = w_answer
              EXCEPTIONS
                text_not_found        = 1
                OTHERS                = 2.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

            IF w_answer = '1'.
              REFRESH tl_texto.
              CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
                EXPORTING
                  im_title = 'Texto da Fatura'
                CHANGING
                  ch_text  = tl_texto.

              LOOP AT tl_texto INTO wl_texto.
                wa_lines-tdformat = '*'.
                wa_lines-tdline+0(72) = wl_texto.
                APPEND wa_lines TO it_lines.
                CLEAR  wa_lines.
              ENDLOOP.
            ENDIF.

            "" VERIFICA PERMISSÃO DO USUÁRIO COM RELAÇÃO A DATA RETROATIVA
            "" AJUSTE POR ERRO (06/11/2014) DE BACKUP DO BANCO DB2
            SELECT SINGLE *
              FROM setleaf
              INTO wa_setleaf
             WHERE setname = 'VF01_USUARIO'
               AND valfrom = sy-uname.

            IF sy-subrc IS INITIAL.
              w_billing-bill_date = wa_saida-dt_movimento.
            ELSE.
              w_billing-bill_date = sy-datum.
            ENDIF.

            "//17.01.2017 - EJ (Adiciona faturas agrupadas)
            IF line_exists( t_fatura_agrupada[ werks = p_branch kunnr = wa_saida-kunnr inco1 = wa_saida-inco1 ] ).
              LOOP AT it_saida ASSIGNING <fs_saida> WHERE ( dt_movimento EQ wa_saida-dt_movimento ) AND
                                                          ( matnr        EQ wa_saida-matnr        ) AND
                                                          ( kunnr        EQ wa_saida-kunnr        ) AND
                                                          ( operacao(4)  EQ wa_saida-operacao(4)  ) AND
                                                          ( inco1        EQ wa_saida-inco1        ) AND
                                                          ( fatura       IS INITIAL               ).

                UPDATE zsdt0001 SET st_proc = <fs_saida>-st_proc
                 WHERE ch_referencia = <fs_saida>-ch_referencia.

                <fs_saida>-line_color = col_yellow_int.
                <fs_saida>-color_cell = VALUE #( ( fname = 'FATURA'
                                                   color-col = col_positive
                                                   color-int = col_heading
                                                   color-inv = col_heading )
                                               ).

                w_billing-ref_doc    = <fs_saida>-remessa.
                w_billing-ref_doc_ca = 'J'.
                APPEND w_billing TO t_billing.
              ENDLOOP.
            ENDIF.

            w_billing-ref_doc    = wa_saida-remessa(10).
            w_billing-ref_doc_ca = 'J'.
            APPEND w_billing TO t_billing.

            REFRESH: ti_bdcdata, t_success.
            CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4) INTO DATA(_data).
            PERFORM f_bdc_data USING:
                'SAPMV60A'  '0102'  'X'               ''                 '',
                ' '         ''      ' '               'BDC_CURSOR'       'RV60A-FKDAT',
                ' '         ''      ' '               'BDC_OKCODE'       '=SICH',
                ' '         ''      ' '               'RV60A-FKDAT'      _data,
                ' '         ''      ' '               'KOMFK-VBELN(01)'  wa_saida-remessa(10).

            SELECT SINGLE *
              FROM setleaf INTO @DATA(_setleaf_vf)
             WHERE setname = 'MAGGI_GER_VFSHDB'.

            IF ( sy-subrc NE 0 ).
              CLEAR: wl_erro.
              PERFORM zf_call_transaction USING 'VF01' CHANGING wl_erro.
              IF wl_erro IS INITIAL.
                WAIT UP TO 6 SECONDS.
                CLEAR: vl_vbeln,vl_mjahr.
                IF ( wa_saida-tipo = 'P' ) OR ( wa_saida-tipo = 'T' ).
                  SELECT SINGLE a~vbeln a~mjahr
                    FROM vbfa AS a INTO (vl_vbeln,vl_mjahr)
                   WHERE a~vbelv = wa_saida-remessa(10)
                     AND a~vbtyp_n  = 'R'
                     AND a~vbtyp_v  = 'J'
                     AND NOT EXISTS ( SELECT *
                                        FROM vbfa AS b
                                       WHERE b~vbelv   = a~vbeln
                                         AND b~vbtyp_n = 'N' "estorno
                                     ).
                ELSE.
                  SELECT SINGLE a~vbeln a~mjahr
                    FROM vbfa AS a INTO (vl_vbeln,vl_mjahr)
                   WHERE a~vbelv = wa_saida-remessa(10)
                     AND a~vbtyp_n  = 'M'
                     AND a~vbtyp_v  = 'J'
                     AND NOT EXISTS ( SELECT *
                                        FROM vbfa AS b
                                       WHERE b~vbelv   = a~vbeln
                                         AND b~vbtyp_n = 'N' "estorno
                                     ).
                ENDIF.

                IF sy-subrc = 0.
                  w_success-ref_doc  = wa_saida-remessa.
                  w_success-bill_doc = vl_vbeln.
                  APPEND w_success TO t_success.
                ENDIF.

              ENDIF.
            ELSE.
              CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE' "#EC CI_USAGE_OK[2438131]
                TABLES
                  billingdatain = t_billing
                  textdatain    = t_textdatain
                  return        = t_return
                  success       = t_success.
            ENDIF.

            IF t_success[] IS NOT INITIAL.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              WAIT UP TO 5 SECONDS.

              LOOP AT t_success INTO w_success.
                TRY.
                    ASSIGN it_saida[ remessa = w_success-ref_doc ] TO <fs_saida>.

                    <fs_saida>-fatura = w_success-bill_doc.
                    <fs_saida>-st_proc = '02'.

                    UPDATE zsdt0001 SET fatura_prod = <fs_saida>-fatura st_proc = <fs_saida>-st_proc
                     WHERE ch_referencia = <fs_saida>-ch_referencia.
                  CATCH cx_sy_itab_line_not_found.
                ENDTRY.
              ENDLOOP.

              MESSAGE |Remessa gerada sob. nº { w_success-bill_doc }.| TYPE 'S'.

              "//Texto de cabeçalho
              IF ( it_lines[] IS NOT INITIAL ).
                zid               = '0002'.
                zname             = w_success-bill_doc.
                x_header-tdobject = 'VBBK'.
                x_header-tdname   = zname.
                x_header-tdid     = zid.
                x_header-tdspras  = sy-langu.

                CALL FUNCTION 'SAVE_TEXT'
                  EXPORTING
                    client          = sy-mandt
                    header          = x_header
                    savemode_direct = 'X'
                  TABLES
                    lines           = it_lines
                  EXCEPTIONS
                    id              = 1
                    language        = 2
                    name            = 3
                    object          = 4
                    OTHERS          = 5.
              ENDIF.
            ENDIF.

            "//Desbloqueia romaneio
            CALL FUNCTION 'DEQUEUE_EZSDT0001'
              EXPORTING
                ch_referencia = wa_saida-ch_referencia.

            CALL METHOD cl_grid->refresh_table_display
              EXPORTING
                is_stable = wa_stable.
          ENDIF.

        ELSEIF wa_saida-fatura IS NOT INITIAL.
          IF wa_saida-tipo = 'O'.
            SET PARAMETER ID 'VF'    FIELD wa_saida-fatura.
            CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
          ELSE.
            SELECT SINGLE vbeln mjahr
                INTO (vl_vbeln,vl_mjahr)
                FROM vbfa
                WHERE vbelv = wa_saida-remessa(10)
                AND vbtyp_n  = 'R'
                AND vbtyp_v  = 'J'.
            SET PARAMETER ID 'MBN'    FIELD wa_saida-fatura.
            SET PARAMETER ID 'MJA'    FIELD vl_mjahr.
            CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN. "#EC CI_USAGE_OK[1804812]
          ENDIF.
        ENDIF.
        "
      ELSEIF e_column_id = 'DANFE'.

        IF wa_saida-danfe = icon_execute_object. " Executar ZOPUS
          CLEAR wa_zsdt0001.
          SELECT SINGLE *
           FROM zsdt0001
           INTO wa_zsdt0001
           WHERE ch_referencia = wa_saida-ch_referencia.
          IF wa_zsdt0001-nro_nf_prod  GT 0.
            MESSAGE 'Documento atualizado, click em <ATUALIZAR>' TYPE 'I'.
            EXIT.
          ENDIF.
          IF sy-tcode NE 'ZLES0106' AND sy-tcode NE 'ZMM0127'.
            MESSAGE 'Transação apenas de visualização' TYPE 'I'.
            EXIT.
          ENDIF.
          IF wa_saida-fatura = icon_execute_object.
            MESSAGE 'Gerar a Fatura!' TYPE 'I'.
          ELSE.

            "Bloqueia romaneio
            CALL FUNCTION 'ENQUEUE_EZSDT0001'
              EXPORTING
                ch_referencia  = wa_saida-ch_referencia
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

            UPDATE zsdt0001 SET st_proc = wa_saida-st_proc
             WHERE ch_referencia = wa_saida-ch_referencia.

            REFRESH it_color.
            MOVE 'REMESSA'   TO wa_color-fname.
            MOVE '5'         TO wa_color-color-col.
            MOVE '1'         TO wa_color-color-int.
            MOVE '1'         TO wa_color-color-inv.
            APPEND wa_color TO it_color.
            wa_saida-color_cell[] = it_color[].
            MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING color_cell line_color .

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_saida-fatura
              IMPORTING
                output = wa_saida-fatura.

            "verifica estorno fatura
            SELECT SINGLE vbeln mjahr
               INTO (vl_vbeln,vl_mjahr)
               FROM vbfa
              WHERE vbelv = wa_saida-fatura "Fatura que encontrou
                AND vbtyp_n  = 'N'. "estorno
            IF sy-subrc = 0.
              MESSAGE 'O doc de fatura está cancelado. Refazer o lançamento!' TYPE 'I'.
              EXIT.
            ENDIF.

            IF wa_saida-tipo = 'P'.
              SELECT SINGLE vbeln mjahr
                INTO (vl_vbeln,vl_mjahr)
                FROM vbfa
                WHERE vbelv = wa_saida-remessa(10)
                AND vbtyp_n  = 'R'
                AND vbtyp_v  = 'J'.

              CONCATENATE vl_vbeln vl_mjahr INTO vl_refkey.
              SELECT SINGLE docnum
                FROM j_1bnflin
                INTO vl_docnum
                WHERE refkey = vl_refkey.
            ELSE.
              SELECT SINGLE docnum
                FROM j_1bnflin
                INTO vl_docnum
                WHERE refkey = wa_saida-fatura.
            ENDIF.

            SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD vl_docnum.
            SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD wa_saida-bukrs.

            CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.
            GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_saida-danfe.

            IF ( wa_saida-danfe = icon_complete ).
              wa_saida-danfe = vl_docnum.
              MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING danfe.

              "//18.01.2017 - Enio Jesus (Ajuste p/ faturas agrupadas)
              LOOP AT it_saida ASSIGNING <fs_saida> WHERE ( fatura = wa_saida-fatura )
                                                      AND ( danfe  = space           ).
                <fs_saida>-danfe   = wa_saida-danfe.
                <fs_saida>-st_proc = '99'.

                "//Finaliza processo com a DANFE autorizada
                UPDATE zsdt0001 SET nro_nf_prod = <fs_saida>-danfe st_proc = <fs_saida>-st_proc
                 WHERE ch_referencia = <fs_saida>-ch_referencia.

                CLEAR <fs_saida>-icon.
              ENDLOOP.

              "//Finaliza processo com a DANFE autorizada
              IF ( wa_saida-inco1 = 'FOB' OR wa_saida-inco1 = 'CFR' ).
                UPDATE zsdt0001 SET nro_nf_prod = wa_saida-danfe st_proc = '99' "Finalizado
                 WHERE ch_referencia = wa_saida-ch_referencia.

                wa_saida-transp  = icon_icon_list.
                wa_saida-dacte   = icon_icon_list.
                wa_saida-st_proc = '99'.

                CLEAR wa_saida-icon.
                MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING transp dacte icon st_proc.
              ELSE.
                UPDATE zsdt0001 SET nro_nf_prod = wa_saida-danfe st_proc = '03' "Danfe
                 WHERE ch_referencia = wa_saida-ch_referencia.

                wa_saida-st_proc = '03'.
                MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING st_proc.
              ENDIF.

              "//Gerar CPT logo apos aprovação
              IF ( wa_saida-inco1 = 'CPT' ).
                CLEAR: wl_erro, v_tknum.

                PERFORM f_gerar_vt CHANGING wl_erro.

                IF wl_erro EQ 'N' AND v_tknum IS NOT INITIAL.
                  wa_saida-transp = v_tknum.
                  MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING transp.

*                  CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY
*                    EXPORTING
*                      IS_STABLE = WA_STABLE.

                  UPDATE zsdt0001 SET doc_transp = wa_saida-transp st_proc = '04' "doc transporte
                   WHERE ch_referencia = wa_saida-ch_referencia.

                  wa_saida-st_proc = '04'.
                  MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING st_proc.

                  PERFORM memorizar_dt_movimento_badi USING wa_saida-dt_movimento.

                  "//Gerar custo
                  SUBMIT zlesr0013 WITH so_tknum = wa_saida-transp
                                   WITH rb_out   = ''
                                   WITH rb_cus   = 'X'
                                   WITH rb_dtfat = wa_saida-dt_movimento
                   AND RETURN.

                  CLEAR: vl_fknum,vl_ov_frete,vl_fatura_frete.
                  GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD vl_fknum.
                  GET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_ov_frete.
                  GET PARAMETER ID 'Z_MY_PARAMETER_3' FIELD vl_fatura_frete.

                  IF vl_fknum IS NOT INITIAL.
                    wa_saida-doccus  = vl_fknum.
                    wa_saida-ovserv  = vl_ov_frete.
                    wa_saida-fatserv = vl_fatura_frete.
                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                      EXPORTING
                        input  = wa_saida-fatserv
                      IMPORTING
                        output = wa_saida-fatserv.
                    MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING doccus ovserv fatserv.

                    IF vl_fatura_frete IS NOT INITIAL.
                      UPDATE zsdt0001 SET st_proc      = '07' " Fatura Frete
                                          fknum        = vl_fknum
                                          ov_frete     = vl_ov_frete
                                          fatura_frete = vl_fatura_frete
                       WHERE ch_referencia = wa_saida-ch_referencia.
                      wa_saida-st_proc = '07'.
                      MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING st_proc.

                    ELSEIF vl_ov_frete IS NOT INITIAL.
                      UPDATE zsdt0001 SET st_proc      = '06' " OV_FRETE
                                         fknum        = vl_fknum
                                         ov_frete     = vl_ov_frete
                                         fatura_frete = vl_fatura_frete
                      WHERE ch_referencia = wa_saida-ch_referencia.
                      wa_saida-st_proc = '06'.
                      MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING st_proc.
                    ELSEIF vl_fknum IS NOT INITIAL.
                      UPDATE zsdt0001 SET st_proc      = '05' " Doc.Custo
                                         fknum        = vl_fknum
                                         ov_frete     = vl_ov_frete
                                         fatura_frete = vl_fatura_frete
                      WHERE ch_referencia = wa_saida-ch_referencia.

                    ENDIF.
                    IF wa_saida-inco1 = 'CPT'. " Finaliza processo com a Fatura serviço gerada
                      UPDATE zsdt0001 SET   st_proc     = '99' " Finalizado
                      WHERE ch_referencia = wa_saida-ch_referencia.

                      "apaga log Erros
*                      DELETE FROM ZLEST0100 WHERE CH_REFERENCIA = WA_SAIDA-CH_REFERENCIA.
                      CLEAR wa_saida-icon.
                      wa_saida-dacte    = icon_icon_list.
                      wa_saida-st_proc = '99'.
                      MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING  dacte icon st_proc.
                      CALL METHOD cl_grid->refresh_table_display
                        EXPORTING
                          is_stable = wa_stable.
                    ENDIF.
                    "refresh na tela
                    CALL METHOD cl_grid->refresh_table_display
                      EXPORTING
                        is_stable = wa_stable.
                  ENDIF.

                ELSE.
                  MESSAGE 'Erro ao gerar transporte!' TYPE 'I'.
                ENDIF.
              ENDIF.
            ELSE.
              MESSAGE 'Danfe ainda não autorizado pela SEFAZ' TYPE 'I'.
            ENDIF.

            "//Desbloqueia romaneio
            CALL FUNCTION 'DEQUEUE_EZSDT0001'
              EXPORTING
                ch_referencia = wa_saida-ch_referencia.

            CALL METHOD cl_grid->refresh_table_display
              EXPORTING
                is_stable = wa_stable.
          ENDIF.
        ELSE.
          SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_saida-danfe.
          SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD wa_saida-bukrs.
          CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.
        ENDIF.
        "
      ELSEIF e_column_id = 'TRANSP'.
        IF wa_saida-transp = icon_execute_object.
          CLEAR: wa_zsdt0001.

          SELECT SINGLE *
            FROM zsdt0001
            INTO wa_zsdt0001
           WHERE ch_referencia = wa_saida-ch_referencia.

          IF wa_zsdt0001-fknum GT 0.
            MESSAGE 'Documento atualizado, click em <ATUALIZAR>' TYPE 'I'.
            EXIT.
          ENDIF.

          IF sy-tcode NE 'ZLES0106' AND sy-tcode NE 'ZMM0127'.
            MESSAGE 'Transação apenas de visualização' TYPE 'I'.
            EXIT.
          ENDIF.

          "verifica estorno fatura
          SELECT SINGLE vbeln mjahr
             INTO (vl_vbeln,vl_mjahr)
             FROM vbfa
            WHERE vbelv = wa_saida-fatura "Fatura que encontrou
              AND vbtyp_n  = 'N'. "estorno
          IF sy-subrc = 0.
            MESSAGE 'O doc de fatura está cancelado. Refazer o lançamento!' TYPE 'I'.
            EXIT.
          ENDIF.

          CLEAR vl_fksto.
          SELECT SINGLE fksto
            INTO vl_fksto
            FROM vbrk
           WHERE vbeln = wa_saida-fatura.

          IF  vl_fksto = 'X'.
            MESSAGE 'o doc de fatura está cancelado. Refazer o lançamento' TYPE 'I'.
            EXIT.
          ENDIF.

          SELECT SINGLE lifnr name1 dlgrp
            FROM lfa1
            INTO wa_lfa1
           WHERE lifnr = wa_saida-lifnr.

          IF sy-subrc = 0 AND wa_lfa1-dlgrp NE '0001' AND wa_saida-inco1 NE 'FOB' AND wa_saida-inco1 NE 'CFR'.
            MESSAGE |Fornecedor { wa_saida-lifnr } não configurado como agente de frete. Solicite ajuste à central de cadastro.| TYPE 'I'.
            EXIT.
          ENDIF.

          IF wa_saida-danfe = icon_execute_object.
            MESSAGE 'Gerar a DANFE!' TYPE 'I'.
          ELSE.
*            IF WA_SAIDA-INCO1 = 'CPT'.  " Gerar CPT logo apos aprovação
*              MESSAGE 'Tipo de Frete CPT gerado logo após a danfe' TYPE 'I'.
*              EXIT.
*            ENDIF.

            "Bloqueia romaneio
            CALL FUNCTION 'ENQUEUE_EZSDT0001'
              EXPORTING
                ch_referencia  = wa_saida-ch_referencia
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

            ENDIF.

            UPDATE zsdt0001 SET st_proc = wa_saida-st_proc
             WHERE ch_referencia = wa_saida-ch_referencia.

            CLEAR: wl_erro, v_tknum.
            PERFORM f_gerar_vt CHANGING wl_erro.

            IF wl_erro EQ 'N' AND v_tknum IS NOT INITIAL.
              wa_saida-transp = v_tknum.
              MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING transp.

              CALL METHOD cl_grid->refresh_table_display
                EXPORTING
                  is_stable = wa_stable.

              UPDATE zsdt0001 SET doc_transp = wa_saida-transp st_proc = '04' "Doc transporte
               WHERE ch_referencia = wa_saida-ch_referencia.

              wa_saida-st_proc = '04'.
              MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING st_proc.

              PERFORM memorizar_dt_movimento_badi USING wa_saida-dt_movimento.

              "Gerar custo
              IF wa_saida-inco1 = 'CPT'.
                SUBMIT zlesr0013 WITH so_tknum = wa_saida-transp
                                 WITH p_chave  = wa_saida-ch_referencia
                                 WITH rb_out   = ''
                                 WITH rb_cus   = 'X'
                                 WITH rb_dtfat = wa_saida-dt_movimento
                AND RETURN.
              ELSE.
                SUBMIT zlesr0013 WITH so_tknum = wa_saida-transp
                                 WITH p_chave  = wa_saida-ch_referencia
                                 WITH rb_dtfat = wa_saida-dt_movimento
                AND RETURN.
              ENDIF.

              CLEAR: vl_fknum,vl_ov_frete,vl_fatura_frete.
              GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD vl_fknum.
              GET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_ov_frete.
              GET PARAMETER ID 'Z_MY_PARAMETER_3' FIELD vl_fatura_frete.

              IF vl_fknum IS NOT INITIAL.
                wa_saida-doccus  = vl_fknum.
                wa_saida-ovserv  = vl_ov_frete.
                wa_saida-fatserv = vl_fatura_frete.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = wa_saida-fatserv
                  IMPORTING
                    output = wa_saida-fatserv.

                MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING doccus ovserv fatserv.

                IF vl_fatura_frete IS NOT INITIAL.
                  UPDATE zsdt0001 SET st_proc      = '07' " Fatura Frete
                                      fknum        = vl_fknum
                                      ov_frete     = vl_ov_frete
                                      fatura_frete = vl_fatura_frete
                   WHERE ch_referencia = wa_saida-ch_referencia.
                  wa_saida-st_proc = '07'.

                  MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING st_proc.

                ELSEIF vl_ov_frete IS NOT INITIAL.
                  UPDATE zsdt0001 SET st_proc     = '06' " OV_FRETE
                                     fknum        = vl_fknum
                                     ov_frete     = vl_ov_frete
                                     fatura_frete = vl_fatura_frete
                  WHERE ch_referencia = wa_saida-ch_referencia.
                  wa_saida-st_proc = '06'.

                  MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING st_proc.

                ELSEIF vl_fknum IS NOT INITIAL.
                  UPDATE zsdt0001 SET st_proc      = '05' " Doc.Custo
                                     fknum        = vl_fknum
                                     ov_frete     = vl_ov_frete
                                     fatura_frete = vl_fatura_frete
                  WHERE ch_referencia = wa_saida-ch_referencia.

                  wa_saida-st_proc = '05'.

                  MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING st_proc.

                  IF wa_saida-inco1 = 'CPT'. " Finaliza processo com a Fatura serviço gerada
                    UPDATE zsdt0001 SET   st_proc     = '99' " Finalizado
                    WHERE ch_referencia = wa_saida-ch_referencia.

                    CLEAR wa_saida-icon.
                    "apaga log Erros
*                    DELETE FROM ZLEST0100 WHERE CH_REFERENCIA = WA_SAIDA-CH_REFERENCIA.

                    wa_saida-dacte    = icon_icon_list.
                    wa_saida-st_proc = '99'.

                    MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING  dacte icon st_proc.

                    CALL METHOD cl_grid->refresh_table_display
                      EXPORTING
                        is_stable = wa_stable.
                  ENDIF.
                ENDIF.
              ENDIF.

            ELSE.
              MESSAGE 'Erro ao gerar transporte!' TYPE 'I'.
            ENDIF.

            "desbloqueia romaneio
            CALL FUNCTION 'DEQUEUE_EZSDT0001'
              EXPORTING
                ch_referencia = wa_saida-ch_referencia.

            "refresh na tela
            CALL METHOD cl_grid->refresh_table_display
              EXPORTING
                is_stable = wa_stable.

          ENDIF.

        ELSEIF wa_saida-transp NE icon_icon_list AND wa_saida-transp+0(4) NE '@11@'.
          SET PARAMETER ID 'TNR' FIELD wa_saida-transp+0(10).
          CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
        ENDIF.

      ELSEIF e_column_id = 'DOCCUS'.
        IF wa_saida-doccus = icon_icon_list.
        ELSE.
          SET PARAMETER ID 'FKK'    FIELD wa_saida-doccus.
          CALL TRANSACTION 'VI03' AND SKIP FIRST SCREEN.
        ENDIF.
      ELSEIF  e_column_id = 'OVSERV'.
        IF wa_saida-ovserv  NE icon_icon_list.
          SET PARAMETER ID 'AUN'    FIELD wa_saida-ovserv.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        ENDIF.
      ELSEIF  e_column_id = 'FATSERV'.
        IF wa_saida-fatserv  NE icon_icon_list.
          SET PARAMETER ID 'VF'   FIELD wa_saida-fatserv.
          CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
        ENDIF.
        "
      ELSEIF e_column_id = 'DACTE'.
        IF wa_saida-dacte = icon_execute_object. " Executar
          CLEAR wa_zsdt0001.
          SELECT SINGLE *
            FROM zsdt0001
            INTO wa_zsdt0001
            WHERE ch_referencia = wa_saida-ch_referencia.

          IF wa_zsdt0001-nro_nf_frete GT 0.
            MESSAGE 'Documento atualizado, click em <ATUALIZAR>' TYPE 'I'.
            EXIT.
          ENDIF.

          IF sy-tcode NE 'ZLES0106' AND sy-tcode NE 'ZMM0127'.
            MESSAGE 'Transação apenas de visualização' TYPE 'I'.
            EXIT.
          ENDIF.
          IF wa_saida-fatserv = icon_icon_list.
            MESSAGE 'Gerar a Fatura Frete!' TYPE 'I'.
          ELSE.
            "Bloqueia romaneio
            CALL FUNCTION 'ENQUEUE_EZSDT0001'
              EXPORTING
                ch_referencia  = wa_saida-ch_referencia
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
            UPDATE zsdt0001 SET st_proc      = wa_saida-st_proc
                  WHERE ch_referencia = wa_saida-ch_referencia.

            REFRESH it_color.
            MOVE 'REMESSA'   TO wa_color-fname.
            MOVE '5'         TO wa_color-color-col.
            MOVE '1'         TO wa_color-color-int.
            MOVE '1'         TO wa_color-color-inv.
            APPEND wa_color TO it_color.
            wa_saida-color_cell[] = it_color[].
            MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING color_cell line_color .

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_saida-fatserv
              IMPORTING
                output = wa_saida-fatserv.

            SELECT SINGLE j_1bnfdoc~bukrs j_1bnflin~docnum
              FROM j_1bnflin
              INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
              INTO (vl_bukrs,vl_docnum)
              WHERE j_1bnflin~refkey = wa_saida-fatserv.


            SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD vl_docnum.
            SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_bukrs.
            CALL TRANSACTION 'ZCTE' AND SKIP FIRST SCREEN.
            GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_saida-dacte.
            IF wa_saida-dacte = icon_complete.
              wa_saida-dacte = vl_docnum.
              CLEAR wa_saida-icon.
              "apaga log Erros
*              DELETE FROM ZLEST0100 WHERE CH_REFERENCIA = WA_SAIDA-CH_REFERENCIA.
              MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING dacte icon.
              CALL METHOD cl_grid->refresh_table_display
                EXPORTING
                  is_stable = wa_stable.

              UPDATE zsdt0001 SET nro_nf_frete = wa_saida-dacte
                                  st_proc      = '99' " Fim
              WHERE ch_referencia = wa_saida-ch_referencia.
              wa_saida-st_proc = '99'.
              MODIFY it_saida FROM wa_saida INDEX e_row_id-index TRANSPORTING st_proc.

            ELSE.
              MESSAGE 'Dacte ainda não autorizado pela SEFAZ' TYPE 'I'.
            ENDIF.
            "desbloqueia romaneio
            CALL FUNCTION 'DEQUEUE_EZSDT0001'
              EXPORTING
                ch_referencia = wa_saida-ch_referencia.

          ENDIF.
        ELSEIF wa_saida-dacte NE icon_icon_list.
          SELECT SINGLE bukrs
            FROM j_1bnfdoc
            INTO vl_bukrs
            WHERE docnum = wa_saida-dacte.
          SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_saida-dacte.
          SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_bukrs.
          CALL TRANSACTION 'ZCTE' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "CATCH_HOTSPOT


  METHOD on_data_changed.
    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'LIFNR'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      READ TABLE it_saida INTO wa_saida INDEX ls_good-row_id.

      READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY ch_referencia = wa_saida-ch_referencia BINARY SEARCH.
      "Atualiza variaveis de frete
      REFRESH it_zsdt0001_fre.
      wa_zsdt0001-agente_frete = lv_value.
      APPEND wa_zsdt0001 TO it_zsdt0001_fre.
      PERFORM f_pega_frete.
      wa_zsdt0001-agente_frete = lv_value.
      PERFORM f_atual_frete USING wa_zsdt0001 CHANGING wa_saida.
      REFRESH it_zsdt0001_fre.

      lv_value = wa_saida-kbetr.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'KBETR'
          i_value     = lv_value.

    ENDLOOP.
  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD on_data_changed_finished.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED

  METHOD on_f4.
    TYPES: BEGIN OF t_f4_structure,
             fieldtext TYPE dfies-fieldtext,
             fieldname TYPE dfies-fieldname,
           END OF t_f4_structure.

    FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

    DATA: ls_modi TYPE lvc_s_modi.

    TYPES : BEGIN OF ty_t005s,
              bland TYPE t005s-bland,
            END OF ty_t005s.

    DATA: wl_return_chv TYPE  ddshretval,
          wl_dselchv    TYPE  dselc,
          tl_t005s      TYPE TABLE OF ty_t005s,
          wl_t005s      TYPE ty_t005s,
          tl_return_chv TYPE TABLE OF ddshretval,
          tl_dselchv    TYPE TABLE OF dselc,
          wa_style      TYPE lvc_s_styl.


    SELECT DISTINCT bland
      FROM t005s
      INTO TABLE tl_t005s
      WHERE land1 = 'BR'.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'REGION'
        value_org       = 'S'
      TABLES
        value_tab       = tl_t005s
        return_tab      = tl_return_chv
        dynpfld_mapping = tl_dselchv.

    READ TABLE it_saida INTO wa_saida INDEX es_row_no-row_id.
    READ TABLE wa_saida-style INTO wa_style WITH KEY fieldname = 'REGION'.
    IF sy-subrc NE 0.
      READ TABLE tl_return_chv INTO wl_return_chv INDEX 1.
      IF sy-subrc = 0 AND wl_return_chv-fieldval <> ''.
        ASSIGN er_event_data->m_data->* TO <itab>.
        ls_modi-row_id    = es_row_no-row_id.
        ls_modi-fieldname = 'REGION'.
        ls_modi-value     = wl_return_chv-fieldval.
        APPEND ls_modi TO <itab>.

        er_event_data->m_event_handled = 'X'.
      ENDIF.
    ENDIF.


  ENDMETHOD. "on_f4

  METHOD set_toolbar.
    IF NOT t_fatura_agrupada IS INITIAL AND r_dt_a IS NOT INITIAL.
      APPEND VALUE #( butn_type = 3 ) TO e_object->mt_toolbar.

      APPEND VALUE #( function  = 'BTN_AGRUPAR'
                      icon      = icon_deselect_block
                      butn_type = 0
                      text      = 'Agrupar faturas' ) TO e_object->mt_toolbar.
    ENDIF.
  ENDMETHOD.

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'BTN_AGRUPAR'.
        DATA row_record TYPE ty_saida.
        DATA rows_record TYPE TABLE OF ty_saida.

        CALL METHOD cl_grid->get_selected_rows
          IMPORTING
            et_index_rows = DATA(_rows).

        IF _rows IS NOT INITIAL.
          TRY.
              LOOP AT _rows INTO DATA(_row).
                row_record = it_saida[ _row-index ].

                IF row_record-remessa IS INITIAL OR row_record-remessa = icon_execute_object.
                  CLEAR row_record-remessa.
                ENDIF.

                IF row_record-fatura IS INITIAL OR row_record-fatura = icon_execute_object.
                  CLEAR row_record-fatura.
                ELSE.
                  RAISE EXCEPTION TYPE cx_abap_util_exception.
                ENDIF.

                IF row_record-danfe IS INITIAL OR row_record-danfe = icon_execute_object.
                  CLEAR row_record-danfe.
                ENDIF.

                APPEND row_record TO rows_record.
              ENDLOOP.

            CATCH cx_abap_util_exception.
              MESSAGE |O romaneio { row_record-nr_romaneio } já possui fatura, não é possível agrupar.| TYPE 'S' DISPLAY LIKE 'E'.
              EXIT.
          ENDTRY.

          it_saida[] = rows_record.

          LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
            IF <fs_saida>-remessa IS INITIAL AND NOT line_exists(
                         it_saida[ dt_movimento = <fs_saida>-dt_movimento
                                   matnr        = <fs_saida>-matnr
                                   kunnr        = <fs_saida>-kunnr
                                   operacao(4)  = <fs_saida>-operacao(4)
                                   inco1        = <fs_saida>-inco1
                                   remessa      = icon_execute_object
                                 ] ).

              <fs_saida>-remessa = icon_execute_object.
            ENDIF.

            IF NOT line_exists(
                         it_saida[ dt_movimento = <fs_saida>-dt_movimento
                                   matnr        = <fs_saida>-matnr
                                   kunnr        = <fs_saida>-kunnr
                                   operacao(4)  = <fs_saida>-operacao(4)
                                   inco1        = <fs_saida>-inco1
                                   fatura       = icon_execute_object
                                 ] ).

              <fs_saida>-fatura  = icon_execute_object.
            ENDIF.

            IF NOT line_exists(
                         it_saida[ dt_movimento = <fs_saida>-dt_movimento
                                   matnr        = <fs_saida>-matnr
                                   kunnr        = <fs_saida>-kunnr
                                   operacao(4)  = <fs_saida>-operacao(4)
                                   inco1        = <fs_saida>-inco1
                                   danfe        = icon_execute_object
                                 ] ).

              <fs_saida>-danfe   = icon_execute_object.
            ENDIF.
          ENDLOOP.

          CALL METHOD cl_grid->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
        ELSE.
          MESSAGE 'É necessário selecionar os romaneios que deseja agrupar.' TYPE 'S' DISPLAY LIKE 'W'.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  IF s_data IS INITIAL AND r_dt_f = 'X'.
    EXIT.
  ENDIF.
  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
     ID 'WERKS' FIELD  p_branch
     ID 'ACTVT' FIELD '03'.    "Alteração

  IF sy-subrc = 0.
    PERFORM:
            f_seleciona_dados, " Form seleciona dados
            f_saida, " Form de saida
            f_imprime_dados.
  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .
  DATA tabix TYPE  sy-tabix .

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_ARMAZENAGEM_VA01'
    TABLES
      set_values    = t_auart
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  SORT t_auart BY from.
  "
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_ZLES106_RECUP'
    TABLES
      set_values    = t_usermd
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  SORT t_usermd BY from.

  IF p_inter = '55'.
    SELECT *
      FROM zsdt0001
      INTO TABLE it_zsdt0001
     WHERE bukrs   EQ p_bukrs
       AND branch  EQ p_branch
       AND id_interface NE 48
       AND tp_movimento  = 'S'
       AND ch_referencia IN s_chave.

  ELSE.

    IF r_dt_a = 'X'.
      SELECT *
        FROM zsdt0001
        INTO TABLE it_zsdt0001
        WHERE bukrs   EQ p_bukrs
        AND   branch  EQ p_branch
        AND   vbeln   IN s_vbeln
        AND   vbeln   IN s_ebeln
        AND   matnr   IN s_matnr
        AND   doc_transp  IN s_doc
        AND   vbeln  NE ''
        AND   dt_movimento IN s_data
        AND   dt_movimento GE '20141006'
        AND   st_proc NE '99'
        AND   id_interface NE 48
        AND   tp_movimento  = 'S'
        AND   ch_referencia IN s_chave.
    ELSE.
      SELECT *
        FROM zsdt0001
        INTO TABLE it_zsdt0001
        WHERE bukrs   EQ p_bukrs
        AND   branch  EQ p_branch
        AND   vbeln   IN s_vbeln
        AND   vbeln   IN s_ebeln
        AND   matnr   IN s_matnr
        AND   doc_transp  IN s_doc
        AND   vbeln  NE ''
        AND   dt_movimento IN s_data
        AND   st_proc EQ '99'
        AND   id_interface NE 48
        AND   tp_movimento  = 'S'
        AND   ch_referencia IN s_chave.

    ENDIF.
  ENDIF.
  CHECK it_zsdt0001[] IS NOT INITIAL.

  SELECT matnr spart
    FROM mara
    INTO TABLE it_mara
   FOR ALL ENTRIES IN it_zsdt0001
   WHERE  matnr EQ it_zsdt0001-matnr.

  SORT it_zsdt0001 BY vbeln.

  REFRESH it_lfa1.

  "Ponto de coleta  REMESSA
  SELECT vbeln lifnr
    FROM vbpa
    INTO  TABLE it_vbpa_cr
    FOR ALL ENTRIES IN it_zsdt0001
    WHERE vbeln   = it_zsdt0001-doc_rem
    AND   parvw   = 'PC'.

  IF it_vbpa_cr[] IS NOT INITIAL.
    SELECT lifnr name1 dlgrp lzone regio
      FROM lfa1
      INTO TABLE it_lfa1
      FOR ALL ENTRIES IN it_vbpa_cr
      WHERE lifnr  = it_vbpa_cr-lifnr.

  ENDIF.

  "Ponto de coleta  ORDEM
  SELECT vbeln lifnr
    FROM vbpa
    INTO TABLE it_vbpa_co
    FOR ALL ENTRIES IN it_zsdt0001
    WHERE vbeln = it_zsdt0001-vbeln
    AND   parvw = 'PC'.

  IF it_vbpa_co[] IS NOT INITIAL.
    SELECT lifnr name1 dlgrp lzone regio
      FROM lfa1
      APPENDING TABLE it_lfa1
      FOR ALL ENTRIES IN it_vbpa_co
      WHERE lifnr  = it_vbpa_co-lifnr.

  ENDIF.

  "Ponto de coleta  Pedido
  SELECT ebeln lifn2
    FROM ekpa
    INTO TABLE it_ekpa_pr
    FOR ALL ENTRIES IN it_zsdt0001
    WHERE ebeln = it_zsdt0001-vbeln
    AND   parvw = 'PR'.

  IF it_ekpa_pr[] IS NOT INITIAL.
    SELECT lifnr name1 dlgrp lzone regio
      FROM lfa1
      APPENDING TABLE it_lfa1
      FOR ALL ENTRIES IN it_ekpa_pr
      WHERE lifnr  = it_ekpa_pr-lifn2.
  ENDIF.

  SELECT *
    FROM zlest0132
    INTO TABLE it_zlest0132.

  SORT: it_mara     BY matnr,
        it_lfa1     BY lifnr,
        it_vbpa_co  BY vbeln,  "Ponto de coleta  ORDEM
        it_ekpa_pr  BY ebeln,  "Ponto de coleta  Pedido
        it_zlest0132   BY branch parid.

  LOOP AT it_zsdt0001 INTO wa_zsdt0001.
    tabix = sy-tabix.
    READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_zsdt0001-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      IF wa_mara-spart NE '02'.
        CONTINUE.
      ENDIF.
    ENDIF.
    READ TABLE it_zlest0132 INTO wa_zlest0132 WITH KEY branch = wa_zsdt0001-branch
                                                       parid  = wa_zsdt0001-parid BINARY SEARCH.
    IF sy-subrc = 0.
      wa_zsdt0001-del = 'X'.
      MODIFY it_zsdt0001 FROM wa_zsdt0001 INDEX tabix TRANSPORTING del.
    ENDIF.
*    "PEDIDO
*    READ TABLE IT_EKPA_PR INTO WA_EKPA_PR WITH KEY EBELN = WA_ZSDT0001-VBELN BINARY SEARCH.
*    IF SY-SUBRC = 0.
*      READ TABLE  IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_EKPA_PR-LIFN2 BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        IF WA_LFA1-REGIO EQ 'RO'.
*          WA_ZSDT0001-DEL = 'X'.
*          MODIFY IT_ZSDT0001 FROM WA_ZSDT0001 INDEX TABIX TRANSPORTING DEL.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*    "ORDEM
*    READ TABLE IT_VBPA_CO INTO WA_VBPA_CO WITH KEY VBELN = WA_ZSDT0001-VBELN BINARY SEARCH. " PC Ordem
*    IF SY-SUBRC = 0.
*      READ TABLE  IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_VBPA_CO-LIFNR BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        IF WA_LFA1-REGIO EQ 'RO'.
**        IF WA_LFA1-REGIO NE 'RO'.
*          WA_ZSDT0001-DEL = 'X'.
*          MODIFY IT_ZSDT0001 FROM WA_ZSDT0001 INDEX TABIX TRANSPORTING DEL.
*        ENDIF.
*      ENDIF.
*    ENDIF.
  ENDLOOP.

  DELETE it_zsdt0001 WHERE del = 'X'.

  CHECK it_zsdt0001[] IS NOT INITIAL.

  "Itinerário  ORDEM
  SELECT vbeln route
    FROM vbap
    INTO TABLE it_vbap
    FOR ALL ENTRIES IN it_zsdt0001
    WHERE vbeln = it_zsdt0001-vbeln
    AND   matnr = it_zsdt0001-matnr.
*    AND   POSNR = '000010'.

  "Itinerário  PEDIDO
  SELECT ebeln route
    FROM ekpv
    INTO TABLE it_ekpv
    FOR ALL ENTRIES IN it_zsdt0001
    WHERE ebeln =  it_zsdt0001-vbeln.

  SELECT matnr maktx
    FROM makt
    INTO TABLE it_makt
    FOR ALL ENTRIES IN it_zsdt0001
    WHERE matnr EQ it_zsdt0001-matnr
    AND   spras EQ sy-langu.


  SELECT werks name1
    FROM t001w
    INTO TABLE it_t001w
    FOR ALL ENTRIES IN it_zsdt0001
    WHERE werks EQ it_zsdt0001-branch.

  SELECT vbeln auart kunnr
    FROM vbak
    INTO TABLE it_vbak
    FOR ALL ENTRIES IN it_zsdt0001
    WHERE vbeln	=	it_zsdt0001-vbeln.

  LOOP AT it_vbak INTO wa_vbak.
    tabix = sy-tabix .
    READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
    wa_vbak-tp_movimento = wa_zsdt0001-tp_movimento.
    MODIFY it_vbak FROM wa_vbak INDEX tabix TRANSPORTING tp_movimento.
  ENDLOOP.

  IF it_vbak[] IS NOT INITIAL.
    SELECT  *
      FROM zsdt0011
      INTO TABLE it_zsdt0011_o
      FOR ALL ENTRIES IN it_vbak
      WHERE tp_movimento =  it_vbak-tp_movimento
      AND   auart        =  it_vbak-auart.

    SELECT kunnr name1 lzone
      FROM kna1
      INTO TABLE it_kna1
      FOR ALL ENTRIES IN it_vbak
      WHERE kunnr	=	it_vbak-kunnr.

    SELECT auart bezei
      FROM tvakt
      INTO TABLE it_tvakt
      FOR ALL ENTRIES IN it_vbak
      WHERE auart = it_vbak-auart
      AND   spras = sy-langu.

    SELECT  vbeln inco1
      FROM vbkd
      INTO TABLE it_vbkd
      FOR ALL ENTRIES IN it_zsdt0001
      WHERE vbeln	=	it_zsdt0001-vbeln
      AND   inco1 IN s_inco1
      AND   posnr	=	'000000'.

  ENDIF.

  SELECT ebeln bsart reswk
    FROM ekko
    INTO TABLE it_ekko
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE ebeln EQ it_zsdt0001-vbeln
     AND ebeln IN s_vbeln
     AND bsart EQ 'ZUB'.

  LOOP AT it_ekko INTO wa_ekko.
    tabix = sy-tabix.
    READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY vbeln = wa_ekko-ebeln BINARY SEARCH.
    wa_ekko-tp_movimento = wa_zsdt0001-tp_movimento.
    MODIFY it_ekko FROM wa_ekko INDEX tabix TRANSPORTING tp_movimento.
  ENDLOOP.

  IF it_ekko[] IS NOT INITIAL.
    SELECT  *
      FROM zsdt0011
      INTO TABLE it_zsdt0011_p
      FOR ALL ENTRIES IN it_ekko
      WHERE tp_movimento =  it_ekko-tp_movimento
      AND   bsart        =  it_ekko-bsart.

    SELECT ebeln werks inco1
      FROM ekpo
      INTO TABLE it_ekpo
      FOR ALL ENTRIES IN it_ekko
      WHERE ebeln = it_ekko-ebeln
      AND   inco1 IN s_inco1.

    LOOP AT it_ekpo INTO wa_ekpo.
      tabix = sy-tabix .
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_ekpo-werks
        IMPORTING
          output = wa_ekpo-lifnr.
      MODIFY it_ekpo FROM wa_ekpo INDEX tabix TRANSPORTING lifnr.
    ENDLOOP.

    SELECT lifnr name1 dlgrp lzone
     FROM lfa1
     APPENDING TABLE it_lfa1
     FOR ALL ENTRIES IN it_ekpo
     WHERE lifnr  = it_ekpo-lifnr.

    SELECT bsart batxt
      FROM t161t
      INTO TABLE it_t161t
      FOR ALL ENTRIES IN it_ekko
      WHERE bsart = it_ekko-bsart
      AND   spras = sy-langu.

  ENDIF.

  SELECT vbeln parvw lifnr kunnr
    FROM vbpa
    INTO TABLE it_vbpa
    FOR ALL ENTRIES IN it_zsdt0001
    WHERE vbeln = it_zsdt0001-vbeln
    AND   parvw	IN ('LR','SP').

  IF it_vbpa[] IS NOT INITIAL.
    SELECT kunnr name1 lzone
        FROM kna1
        APPENDING TABLE it_kna1
        FOR ALL ENTRIES IN it_vbpa
        WHERE kunnr	=	it_vbpa-kunnr.
  ENDIF.

*  SELECT *
*    FROM LFA1
*    INTO TABLE IT_LFA1
*    FOR ALL ENTRIES IN IT_VBPA
*    WHERE LIFNR IN S_LIFNR.

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida .
  DATA: tabix      TYPE sy-tabix,
        v_cont_fre TYPE i,
        v_cd_uf    TYPE zlest0002-cd_uf.

  SORT: it_vbak     BY vbeln,
        it_tvakt    BY auart,
        it_t161t    BY bsart,
        it_kna1     BY kunnr,
        it_lfa1     BY lifnr,
        it_t001w    BY werks,
        it_vbkd     BY vbeln,
        it_makt     BY matnr,
        it_ekko     BY ebeln,
        it_ekpo     BY ebeln,
        it_vbpa     BY vbeln parvw,
        it_vbpa_cr  BY vbeln,  "Ponto de coleta  REMESSA
        it_vbpa_co  BY vbeln,  "Ponto de coleta  ORDEM
        it_ekpa_pr  BY ebeln,  "Ponto de coleta  Pedido
        it_vbap     BY vbeln,  "Itinerário  ORDEM
        it_ekpv     BY ebeln.  "Itinerário  PEDIDO


  SORT: it_zsdt0011_o BY tp_movimento auart,
        it_zsdt0011_p BY tp_movimento bsart.

  "Atualiza variaveis de frete
  it_zsdt0001_fre[] = it_zsdt0001[].
  PERFORM f_pega_frete.
  it_zsdt0001[] = it_zsdt0001_fre[].
  REFRESH it_zsdt0001_fre.

  SELECT *
    FROM zsdt0121
    INTO TABLE t_fatura_agrupada
   WHERE werks = p_branch.

  SORT it_zsdt0001 BY ch_referencia.
  LOOP AT it_zsdt0001 INTO wa_zsdt0001.
    REFRESH: style.

    wa_saida-bukrs           = wa_zsdt0001-bukrs.
    wa_saida-branch          = wa_zsdt0001-branch.
    wa_saida-ch_referencia   = wa_zsdt0001-ch_referencia.
    wa_saida-dt_movimento    = wa_zsdt0001-dt_movimento.
    wa_saida-nr_romaneio     = wa_zsdt0001-nr_romaneio.
    wa_saida-placa_cav       = wa_zsdt0001-placa_cav.

    IF wa_zsdt0001-region IS NOT INITIAL.
      wa_saida-region          = wa_zsdt0001-region.
    ELSE.
      SELECT SINGLE cd_uf
        FROM zlest0002
        INTO v_cd_uf
        WHERE pc_veiculo = wa_zsdt0001-placa_cav.
      IF sy-subrc = 0.
        wa_saida-region          = v_cd_uf.
      ENDIF.
    ENDIF.

    wa_saida-route           = wa_zsdt0001-route.
    wa_saida-st_proc         = wa_zsdt0001-st_proc.
    wa_saida-shtyp           = wa_zsdt0001-shtyp.

    CLEAR wa_saida-icon.
    REFRESH ti_zlest0100.
    SELECT *
      FROM zlest0100
      INTO TABLE ti_zlest0100
      WHERE ch_referencia = wa_saida-ch_referencia.

    IF ti_zlest0100[] IS NOT INITIAL.
      wa_saida-icon = icon_led_red.
    ELSE.
      CLEAR wa_saida-icon.
    ENDIF.

    CLEAR wa_vbak.
    READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_zsdt0001-vbeln BINARY SEARCH. " Ordem
    IF sy-subrc = 0.
      wa_saida-tipo = 'O'.
      "Ponto de coleta ordem
      READ TABLE it_vbpa_co INTO wa_vbpa_co WITH KEY vbeln = wa_zsdt0001-vbeln BINARY SEARCH. " Ordem
      IF sy-subrc = 0.
        wa_saida-lifnr_c = wa_vbpa_co-lifnr.
        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_vbpa_co-lifnr BINARY SEARCH.
        wa_saida-name1_c = wa_lfa1-name1.
      ENDIF.

      READ TABLE it_tvakt INTO wa_tvakt WITH KEY auart = wa_vbak-auart BINARY SEARCH.
      CONCATENATE wa_vbak-auart '-' wa_tvakt-bezei INTO wa_saida-operacao.
      CLEAR wa_kna1.
      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
      wa_saida-kunnr           = wa_kna1-kunnr.
      wa_saida-name1           = wa_kna1-name1.

      READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_zsdt0001-vbeln BINARY SEARCH.
      IF sy-subrc = 0 .
        wa_saida-inco1           = wa_vbkd-inco1.
      ELSE.
        CLEAR wa_saida.
        CONTINUE.
      ENDIF.

    ELSE.
      wa_saida-tipo = 'P'.
      "Ponto de coleta pedido
      READ TABLE it_ekpa_pr INTO wa_ekpa_pr WITH KEY ebeln = wa_zsdt0001-vbeln BINARY SEARCH. " Ordem
      IF sy-subrc = 0.
        wa_saida-lifnr_c = wa_ekpa_pr-lifn2.
        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_ekpa_pr-lifn2 BINARY SEARCH.
        wa_saida-name1_c = wa_lfa1-name1.
      ENDIF.

      READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_zsdt0001-vbeln BINARY SEARCH. " Pedidos de transferencia
      READ TABLE it_t161t INTO wa_t161t WITH KEY bsart = wa_ekko-bsart BINARY SEARCH.
      CONCATENATE wa_ekko-bsart '-' wa_t161t-batxt  INTO wa_saida-operacao.

      READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_zsdt0001-vbeln BINARY SEARCH. " Pedidos de transferencia
      IF sy-subrc = 0.
        wa_saida-inco1           = wa_ekpo-inco1.
      ELSE.
        CLEAR wa_saida.
        CONTINUE.
      ENDIF.

      CLEAR wa_lfa1.
      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_ekpo-lifnr BINARY SEARCH.
      wa_saida-kunnr           = wa_lfa1-lifnr.
      wa_saida-name1           = wa_lfa1-name1.

    ENDIF.

    PERFORM f_atual_frete USING wa_zsdt0001 CHANGING wa_saida.

    " substituivalor do frete pelo valor historico
    IF wa_zsdt0001-kbetr GT 0.
      wa_saida-kbetr = wa_zsdt0001-kbetr.
      v_cont_fre = 1.
    ENDIF.

    wa_saida-vbeln           = wa_zsdt0001-vbeln.
    wa_saida-peso_liq        = wa_zsdt0001-peso_liq.

    IF wa_zsdt0001-agente_frete IS NOT INITIAL.
      wa_saida-lifnr = wa_zsdt0001-agente_frete.
      READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = wa_zsdt0001-vbeln
                                                 parvw = 'LR' BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbpa-kunnr BINARY SEARCH.
        IF sy-subrc = 0.
          wa_saida-kunnr           = wa_kna1-kunnr.
          wa_saida-name1           = wa_kna1-name1.
        ENDIF.
      ENDIF.
    ELSEIF wa_saida-tipo = 'O'.
      IF 'ZRFL_ZRDC' CS wa_vbak-auart AND wa_vbak-auart IS NOT INITIAL  AND wa_vbkd-inco1 = 'CIF'.
        READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = wa_zsdt0001-vbeln
                                                  parvw = 'SP' BINARY SEARCH.
        IF sy-subrc = 0.
          wa_saida-lifnr = wa_vbpa-lifnr.
        ENDIF.
      ELSE.
        READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = wa_zsdt0001-vbeln
                                                  parvw = 'LR' BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbpa-kunnr BINARY SEARCH.
          IF sy-subrc = 0.
            wa_saida-kunnr = wa_kna1-kunnr.
            wa_saida-name1 = wa_kna1-name1.
          ENDIF.
        ENDIF.

        IF t_fatura_agrupada IS NOT INITIAL AND 'CFR_FOB' CS wa_saida-inco1.
          wa_style-fieldname = 'LIFNR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT wa_style INTO TABLE style .

          wa_style-fieldname = 'REGION'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .
        ENDIF.
      ENDIF.
    ENDIF.

    "BLOQUEIA CELULA
    CLEAR: wa_style.
    IF wa_saida-region IS NOT INITIAL.
      wa_style-fieldname = 'REGION'.
      wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT  wa_style INTO TABLE style .
    ENDIF.
    CLEAR: wa_style.
    IF wa_saida-lifnr IS NOT INITIAL.
      wa_style-fieldname = 'LIFNR'.
      wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT wa_style INTO TABLE style .
    ENDIF.

    wa_saida-style[] = style[].

    IF wa_saida-name1 IS INITIAL .
      CLEAR wa_kna1.
      READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_zsdt0001-branch  BINARY SEARCH.
      wa_saida-name1           = wa_t001w-name1.
    ENDIF.

    "//Enio Jesus - 17.01.2017
    IF ( wa_zsdt0001-doc_rem IS INITIAL OR  wa_zsdt0001-doc_rem = '' ).

      IF line_exists( t_fatura_agrupada[ werks = p_branch kunnr = wa_saida-kunnr inco1 = wa_saida-inco1 ] ).
        TRY.
            it_saida = it_saida[ dt_movimento = wa_saida-dt_movimento
                                 matnr        = wa_zsdt0001-matnr
                                 kunnr        = wa_saida-kunnr
                                 operacao(4)  = wa_saida-operacao(4)
                                 inco1        = wa_saida-inco1
                                 remessa      = icon_execute_object
                               ].
          CATCH cx_sy_itab_line_not_found.
            wa_saida-remessa = icon_execute_object.
        ENDTRY.

      ELSE.
        wa_saida-remessa = icon_execute_object.
        "Limite de crédito
        SELECT SINGLE *
          FROM zsdt0151
          INTO wa_zsdt0151
          WHERE ch_referencia = wa_zsdt0001-ch_referencia.
        IF sy-subrc = 0.
          IF wa_zsdt0151-status IS INITIAL.
            CONCATENATE icon_workflow_inbox(3) '\QAguardando Aprovação de limite de crédito@' INTO wa_saida-remessa .
          ELSEIF  wa_zsdt0151-status = 'R'.
            CONCATENATE icon_reject(3) '\QLimite de crédito rejeitado@' INTO wa_saida-remessa .
          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.
      wa_saida-remessa = wa_zsdt0001-doc_rem.
      "Ponto de coleta (substitui se ja tiver remessa)
      READ TABLE it_vbpa_cr INTO wa_vbpa_cr WITH KEY vbeln = wa_zsdt0001-doc_rem BINARY SEARCH. " Ordem
      IF sy-subrc = 0.
        wa_saida-lifnr_c = wa_vbpa_cr-lifnr.
        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_vbpa_cr-lifnr BINARY SEARCH.
        wa_saida-name1_c = wa_lfa1-name1.
      ENDIF.
    ENDIF.

    CLEAR vl_docnum.
    IF wa_zsdt0001-fatura_prod IS INITIAL OR wa_zsdt0001-fatura_prod = ''.

      IF line_exists( t_fatura_agrupada[ werks = p_branch kunnr = wa_saida-kunnr inco1 = wa_saida-inco1 ] ).
        TRY.
            it_saida = it_saida[ dt_movimento = wa_saida-dt_movimento
                                 matnr        = wa_zsdt0001-matnr
                                 kunnr        = wa_saida-kunnr
                                 operacao(4)  = wa_saida-operacao(4)
                                 inco1        = wa_saida-inco1
                                 fatura       = icon_execute_object
                               ].
          CATCH cx_sy_itab_line_not_found.
            wa_saida-fatura = icon_execute_object.
        ENDTRY.

      ELSE.
        wa_saida-fatura = icon_execute_object.
      ENDIF.

    ELSE.
      wa_saida-fatura  = wa_zsdt0001-fatura_prod.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_saida-fatura
        IMPORTING
          output = wa_saida-fatura.

      "Verfifica se DANFE já está autorizada e muda o campo
      IF wa_zsdt0001-nro_nf_prod IS INITIAL OR wa_zsdt0001-nro_nf_prod = ''.
        IF wa_saida-tipo = 'P'.
          SELECT SINGLE vbeln mjahr
            INTO (vl_vbeln,vl_mjahr)
            FROM vbfa
            WHERE vbelv = wa_zsdt0001-doc_rem
            AND vbtyp_n  = 'R'
            AND vbtyp_v  = 'J'.

          CONCATENATE vl_vbeln vl_mjahr INTO vl_refkey.
          SELECT SINGLE docnum
            FROM j_1bnflin
            INTO vl_docnum
            WHERE refkey = vl_refkey.
        ELSE.
          SELECT SINGLE docnum
            FROM j_1bnflin
            INTO vl_docnum
            WHERE refkey = wa_saida-fatura.
        ENDIF.

        IF sy-subrc = 0.
          SELECT SINGLE docnum
            FROM j_1bnfe_active
            INTO v_docnum
            WHERE docnum     = vl_docnum
            AND   cancel     = ''
            AND   docsta     = '1'.

          IF sy-subrc = 0.
            wa_zsdt0001-nro_nf_prod = vl_docnum.
            wa_saida-danfe          = vl_docnum.
            UPDATE zsdt0001 SET st_proc = '03'
                                nro_nf_prod = vl_docnum
            WHERE ch_referencia = wa_saida-ch_referencia.
            wa_saida-st_proc = '03'.
            IF wa_saida-inco1 = 'FOB' OR wa_saida-inco1 = 'CFR'. " Finaliza processo com a DANFE autorizada
              UPDATE zsdt0001 SET nro_nf_prod = wa_saida-danfe
                                  st_proc     = '99' " Finalizado
              WHERE ch_referencia = wa_saida-ch_referencia.

              "apaga log Erros
*              DELETE FROM ZLEST0100 WHERE CH_REFERENCIA = WA_SAIDA-CH_REFERENCIA.

              wa_saida-transp  = icon_icon_list.
              wa_saida-dacte   = icon_icon_list.
              wa_saida-st_proc = '99'.
              CLEAR wa_saida-icon.
            ENDIF.
            IF wa_saida-inco1 = 'CPT'.  " Gerar CPT logo apos aprovação
              CLEAR: wl_erro, v_tknum.
              PERFORM f_gerar_vt CHANGING wl_erro.
              wa_zsdt0001-nro_nf_prod = wa_saida-danfe.
              IF wl_erro EQ 'N' AND v_tknum IS NOT INITIAL.
                wa_saida-transp = v_tknum.
                wa_zsdt0001-doc_transp = v_tknum.
                UPDATE zsdt0001 SET doc_transp = wa_saida-transp
                                    st_proc     = '04' " doc transporte
                WHERE ch_referencia = wa_saida-ch_referencia.
                wa_saida-st_proc = '04'.

                PERFORM memorizar_dt_movimento_badi USING wa_saida-dt_movimento.
                "Gerar custo
                SUBMIT zlesr0013
                  WITH so_tknum = wa_saida-transp
                  WITH rb_out   = ''
                  WITH rb_cus   = 'X'
                  WITH rb_dtfat = wa_saida-dt_movimento
                  AND RETURN.

                CLEAR: vl_fknum,vl_ov_frete,vl_fatura_frete.

                GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD vl_fknum.
                GET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_ov_frete.
                GET PARAMETER ID 'Z_MY_PARAMETER_3' FIELD vl_fatura_frete.

                IF vl_fknum IS NOT INITIAL.
                  wa_saida-doccus  = vl_fknum.
                  IF vl_ov_frete IS NOT INITIAL.
                    wa_saida-ovserv  = vl_ov_frete.
                    wa_saida-fatserv = vl_fatura_frete.
                  ENDIF.
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = wa_saida-fatserv
                    IMPORTING
                      output = wa_saida-fatserv.

                  IF vl_fknum IS NOT INITIAL.
                    UPDATE zsdt0001 SET st_proc      = '99' " finaliza CPT
                                       fknum        = vl_fknum
                                       ov_frete     = vl_ov_frete
                                       fatura_frete = vl_fatura_frete
                    WHERE ch_referencia = wa_saida-ch_referencia.
                    wa_saida-st_proc = '99'.
                    wa_zsdt0001-fknum  = vl_fknum.

                    "apaga log Erros
*                    DELETE FROM ZLEST0100 WHERE CH_REFERENCIA = WA_SAIDA-CH_REFERENCIA.
                    CLEAR wa_saida-icon.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF wa_zsdt0001-nro_nf_prod IS INITIAL OR wa_zsdt0001-nro_nf_prod = ''.
      IF line_exists( t_fatura_agrupada[ werks = p_branch kunnr = wa_saida-kunnr inco1 = wa_saida-inco1 ] ).
        TRY.
            it_saida = it_saida[ dt_movimento = wa_saida-dt_movimento
                                 matnr        = wa_zsdt0001-matnr
                                 kunnr        = wa_saida-kunnr
                                 operacao(4)  = wa_saida-operacao(4)
                                 inco1        = wa_saida-inco1
                                 danfe        = icon_execute_object
                               ].
          CATCH cx_sy_itab_line_not_found.
            wa_saida-danfe  = icon_execute_object.
        ENDTRY.
      ELSE.
        wa_saida-danfe = icon_execute_object.
      ENDIF.
    ELSE.
      wa_saida-danfe           = wa_zsdt0001-nro_nf_prod.
    ENDIF.

    IF wa_zsdt0001-doc_transp IS INITIAL OR wa_zsdt0001-doc_transp = ''.
      IF NOT 'CFR_FOB' CS wa_saida-inco1.
        wa_saida-transp = icon_execute_object.
      ELSE.
        wa_saida-transp  = icon_icon_list.
      ENDIF.
    ELSE.
      wa_saida-transp          = wa_zsdt0001-doc_transp.
    ENDIF.

    IF wa_zsdt0001-fknum IS INITIAL OR wa_zsdt0001-fknum = ''.
      wa_saida-doccus = icon_icon_list.
    ELSE.
      wa_saida-doccus = wa_zsdt0001-fknum.
    ENDIF.

    IF wa_zsdt0001-ov_frete IS INITIAL OR wa_zsdt0001-ov_frete = ''.
      wa_saida-ovserv          = icon_icon_list.
    ELSE.
      wa_saida-ovserv          = wa_zsdt0001-ov_frete.
    ENDIF.

    CLEAR vl_docnum.
    IF wa_zsdt0001-fatura_frete IS INITIAL OR wa_zsdt0001-fatura_frete = ''.
      wa_saida-fatserv         = icon_icon_list.
    ELSE.
      wa_saida-fatserv         = wa_zsdt0001-fatura_frete.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_saida-fatserv
        IMPORTING
          output = wa_saida-fatserv.

      IF wa_zsdt0001-nro_nf_frete IS INITIAL OR wa_zsdt0001-nro_nf_frete = ''.
        SELECT SINGLE j_1bnfdoc~bukrs j_1bnflin~docnum
            FROM j_1bnflin
            INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
            INTO (vl_bukrs,vl_docnum)
            WHERE j_1bnflin~refkey = wa_saida-fatserv.

        IF sy-subrc = 0.
          SELECT SINGLE docnum
           FROM j_1bnfe_active
           INTO v_docnum
           WHERE docnum     = vl_docnum
           AND   cancel     = ''
           AND   docsta     = '1'.

          IF sy-subrc = 0.
            wa_zsdt0001-nro_nf_frete = vl_docnum.
            UPDATE zsdt0001 SET st_proc      = '99'
                                nro_nf_frete = vl_docnum
            WHERE ch_referencia = wa_saida-ch_referencia.
            wa_saida-st_proc = '99'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF wa_zsdt0001-nro_nf_frete IS INITIAL OR wa_zsdt0001-nro_nf_frete = ''.
      wa_saida-dacte = icon_execute_object.
    ELSE.
      wa_saida-dacte = wa_zsdt0001-nro_nf_frete.
    ENDIF.

    IF wa_zsdt0001-st_proc = '99' .
      IF wa_saida-transp = icon_execute_object.
        wa_saida-transp = icon_icon_list.
      ENDIF.
      IF wa_saida-dacte = icon_execute_object.
        wa_saida-dacte  = icon_icon_list.
      ENDIF.
    ENDIF.

    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_zsdt0001-matnr BINARY SEARCH.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_zsdt0001-matnr
      IMPORTING
        output = wa_saida-matnr.

    CONCATENATE wa_saida-matnr '-' wa_makt-maktx INTO  wa_saida-material.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_zsdt0001-matnr
      IMPORTING
        output = wa_saida-matnr.

    "Checar se há cancelamento de CTE e estorna documentos
    "DACTE
    IF wa_saida-dacte NE icon_execute_object
      AND wa_saida-dacte NE icon_icon_list
      AND wa_saida-dacte IS NOT INITIAL
      AND wa_saida-dacte NE ''
      AND wa_saida-inco1 EQ 'CIF'
      AND wa_saida-st_proc = '99'. " no automatico somente se estiver finalizado (SEFAZ), senão fazer pelo botão EST_CTE
*
      SELECT SINGLE docnum
        FROM j_1bnfe_active
        INTO v_docnum
        WHERE docnum     = wa_saida-dacte "NRO_NF_FRETE
        AND   cancel       = 'X'.

      IF sy-subrc = 0.
        REFRESH ti_zlest0100.
        "LIMPA CTE
        wa_saida-dacte           = icon_execute_object.
        UPDATE zsdt0001 SET st_proc      = '07'
                            nro_nf_frete = ''
        WHERE ch_referencia = wa_saida-ch_referencia.
        wa_saida-st_proc = '07'.
        PERFORM f_estorno_cte CHANGING wa_saida.
        IF ti_zlest0100[] IS NOT INITIAL.
          wa_saida-icon = icon_led_red.
        ELSE.
          CLEAR wa_saida-icon.
        ENDIF.
      ENDIF.
    ENDIF.

    "Checar se há cancelamento de NFE e estorna documentos
    "DANFE
    IF wa_saida-danfe NE icon_execute_object
      AND wa_saida-danfe IS NOT INITIAL
      AND wa_saida-danfe NE ''
      AND  ( wa_saida-st_proc = '03' OR ( wa_saida-st_proc = '99' AND  'FOB_CPT' CS wa_saida-inco1  ) ).

      SELECT SINGLE docnum
        FROM j_1bnfe_active
        INTO v_docnum
        WHERE docnum     = wa_saida-danfe
        AND   cancel       = 'X'. " no automatico somente se estiver finalizado (SEFAZ), senão fazer pelo botão EST_CTE

      IF sy-subrc EQ 0.
        REFRESH ti_zlest0100.
        IF wa_saida-inco1 = 'CPT'.
          wa_saida-st_proc = '05'.
          UPDATE zsdt0001 SET st_proc      = '05'
          WHERE ch_referencia = wa_saida-ch_referencia.
          PERFORM f_estorno_custo CHANGING wa_saida.
        ENDIF.

        wa_saida-danfe           = icon_execute_object.
        UPDATE zsdt0001 SET   st_proc      = '02'
                              nro_nf_prod  = ''
          WHERE ch_referencia = wa_saida-ch_referencia.

        PERFORM f_estorno_nfe CHANGING wa_saida.
        IF ti_zlest0100[] IS NOT INITIAL.
          wa_saida-icon = icon_led_red.
        ELSE.
          CLEAR wa_saida-icon.
        ENDIF.
      ENDIF.
    ENDIF.

    APPEND wa_saida TO it_saida.
    CLEAR wa_saida.
  ENDLOOP.

  IF s_lifnr-low IS NOT INITIAL.
    DELETE it_saida WHERE lifnr NE s_lifnr-low.
  ENDIF.

  SORT it_zsdt0001 BY ch_referencia.

*  IF T_FATURA_AGRUPADA IS NOT INITIAL.
*    SORT: IT_SAIDA BY DT_MOVIMENTO,
*          IT_SAIDA BY OPERACAO,
*          IT_SAIDA BY INCO1,
*          IT_SAIDA BY LIFNR_C.
*  ELSE.
  SORT it_saida BY nr_romaneio.
*  ENDIF.
ENDFORM.                    " F_SAIDA

*&---------------------------------------------------------------------*
*&      Form  Pega_FRETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_pega_frete.
  DATA: wa_zsdt0011 TYPE zsdt0011,
        wa_carta    TYPE zcarta_correcao,
        vl_id       TYPE zcarta_correcao-id_cc,
        tabix       TYPE sy-tabix,
        v_agregado  TYPE zlest0002-agregado,
        v_cd_uf     TYPE zlest0002-cd_uf,
        v_stcd1k    TYPE kna1-stcd1,
        v_stcd1l    TYPE lfa1-stcd1.

  REFRESH:  it_a900,
            it_a910,
            it_a911,
            it_a915,
            it_a918,
            it_a919,
            it_konp.

  CHECK it_zsdt0001_fre[] IS NOT INITIAL.

  LOOP AT it_zsdt0001_fre INTO wa_zsdt0001.
    tabix = sy-tabix.
    CLEAR wa_saida.

    READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_zsdt0001-vbeln BINARY SEARCH. " Ordem
    IF sy-subrc = 0.
*      " Exclui Romaneio do SET
*      READ TABLE T_AUART WITH KEY FROM = WA_VBAK-AUART BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        CONTINUE.
*      ENDIF.
      "ZONA coleta ordem
      READ TABLE it_vbpa_co INTO wa_vbpa_co WITH KEY vbeln = wa_zsdt0001-vbeln BINARY SEARCH. " Ordem
      IF sy-subrc = 0.
        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_vbpa_co-lifnr BINARY SEARCH.
        wa_zsdt0001-lzonea = wa_lfa1-lzone.
      ENDIF.
      "Zona Local de entrega
      READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = wa_zsdt0001-vbeln
                                                parvw = 'LR' BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbpa-kunnr BINARY SEARCH.
        IF sy-subrc = 0.
          wa_zsdt0001-lzonez = wa_kna1-lzone.
        ENDIF.
      ENDIF.

      "Agregado
      SELECT SINGLE agregado
        FROM zlest0002
        INTO v_agregado
        WHERE pc_veiculo = wa_zsdt0001-placa_cav.

      IF sy-subrc = 0.
        IF v_agregado = 1.
          wa_zsdt0001-add01 = '0000000001'.
        ELSE.
          wa_zsdt0001-add01 = '0000000002'.
        ENDIF.
      ENDIF.

      "Itinerário
      READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_zsdt0001-vbeln BINARY SEARCH. " Ordem
      IF sy-subrc = 0.
        wa_zsdt0001-route = wa_vbap-route.
      ENDIF.
      READ TABLE it_zsdt0011_o INTO wa_zsdt0011 WITH KEY tp_movimento = wa_vbak-tp_movimento
                                                         auart        = wa_vbak-auart BINARY SEARCH.
      IF sy-subrc = 0.
        wa_zsdt0001-shtyp = wa_zsdt0011-shtyp.
      ENDIF.
      IF 'ZRDC_ZRFL' CS wa_zsdt0011-auart . "exceção para selecionar typo de transporte
        SELECT vbeln parvw lifnr kunnr
          FROM vbpa
          INTO TABLE it_vbpa_2
          WHERE vbeln	=	wa_zsdt0001-vbeln
          AND   parvw	IN ('LR','Z1').

        CLEAR: wa_vbpa_2,v_stcd1k.
        READ TABLE it_vbpa_2 INTO wa_vbpa_2 WITH KEY parvw = 'LR'.

        SELECT SINGLE stcd1
          FROM kna1
          INTO v_stcd1k
          WHERE kunnr   = wa_vbpa_2-kunnr.

        CLEAR: wa_vbpa_2,v_stcd1l.
        READ TABLE it_vbpa_2 INTO wa_vbpa_2 WITH KEY parvw = 'Z1'.
        CHECK sy-subrc = 0.
        SELECT SINGLE stcd1
          FROM lfa1
          INTO v_stcd1l
          WHERE lifnr   = wa_vbpa_2-lifnr.

        IF v_stcd1k NE v_stcd1l.
          wa_zsdt0001-shtyp = 'Z001'.
        ELSE.
          wa_zsdt0001-shtyp = 'Z004'.
        ENDIF.
      ENDIF.
    ELSE.
      "Itinerário
      READ TABLE it_ekpv INTO wa_ekpv WITH KEY ebeln = wa_zsdt0001-vbeln BINARY SEARCH. " Pedido
      IF sy-subrc = 0.
        wa_zsdt0001-route = wa_ekpv-route.
      ENDIF.
      READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_zsdt0001-vbeln BINARY SEARCH. " Pedidos de transferencia
      READ TABLE it_zsdt0011_p INTO wa_zsdt0011 WITH KEY tp_movimento = wa_ekko-tp_movimento
                                                         bsart        = wa_ekko-bsart BINARY SEARCH.
      IF sy-subrc = 0.
        wa_zsdt0001-shtyp = wa_zsdt0011-shtyp.
      ENDIF.
      READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_zsdt0001-vbeln BINARY SEARCH. " Pedidos de transferencia
      IF sy-subrc = 0.
        IF wa_ekpo-inco1 = 'CIF'.
          "Agregado
          SELECT SINGLE agregado
            FROM zlest0002
            INTO v_agregado
            WHERE pc_veiculo = wa_zsdt0001-placa_cav.
          IF sy-subrc = 0.
            IF v_agregado = 1.
              wa_zsdt0001-add01 = '0000000001'.
            ELSE.
              wa_zsdt0001-add01 = '0000000002'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.
    "Agente de Frete
    IF wa_zsdt0001-agente_frete IS INITIAL.
      READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_zsdt0001-vbeln BINARY SEARCH. " Ordem
      IF sy-subrc = 0.
        READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_zsdt0001-vbeln BINARY SEARCH.
        IF sy-subrc = 0.
          IF 'ZRFL_ZRDC' CS wa_vbak-auart
              AND wa_vbkd-inco1 = 'CIF'.
            READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = wa_zsdt0001-vbeln
                                                      parvw = 'SP' BINARY SEARCH.
            wa_zsdt0001-agente_frete           = wa_vbpa-lifnr.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    CLEAR vl_id.
    SELECT SINGLE MAX( id_cc )
       FROM zcarta_correcao
      INTO     vl_id
      WHERE docnum = wa_zsdt0001-nro_nf_prod
      AND   authcode      NE ''
      AND   novo_agente   NE ''.

    IF vl_id GT 0.
      SELECT SINGLE *
         INTO wa_carta
         FROM zcarta_correcao
        WHERE docnum = wa_zsdt0001-nro_nf_prod
        AND   authcode      NE ''
        AND   novo_agente   NE ''
        AND   id_cc         EQ vl_id.
      IF sy-subrc = 0.
        IF wa_carta-novo_agente NE wa_zsdt0001-agente_frete.
          wa_zsdt0001-agente_frete = wa_carta-novo_agente.
        ENDIF.
      ENDIF.
    ENDIF.

    MODIFY it_zsdt0001_fre FROM wa_zsdt0001  INDEX tabix TRANSPORTING route shtyp agente_frete  lzonea lzonez add01 .
  ENDLOOP.


  REFRESH it_konp.
  "Tp.transp./ForncServ./ItinTransp/Contrato/Agregado
  SELECT *
    FROM a900
    INTO TABLE it_a900
     FOR ALL ENTRIES IN it_zsdt0001_fre
   WHERE kappl EQ 'F'
    AND  kschl EQ 'ZFRE'
    AND  shtyp EQ it_zsdt0001_fre-shtyp
    AND  tdlnr EQ it_zsdt0001_fre-agente_frete
    AND  route EQ it_zsdt0001_fre-route
    AND  add01 EQ it_zsdt0001_fre-add01
    AND  kfrst EQ ''
    AND  datab LE sy-datum
    AND  datbi GE sy-datum
    AND  EXISTS ( SELECT *
                  FROM  konp
                  WHERE knumh = a900~knumh
                  AND   loevm_ko EQ '' ).

  IF it_a900[] IS NOT INITIAL.
    SELECT *
      FROM konp
      INTO TABLE it_konp
      FOR ALL ENTRIES IN it_a900
      WHERE knumh = it_a900-knumh
      AND   loevm_ko EQ ''.
  ENDIF.

  " Tp.transp./ForncServ./Zona part./Zona cheg.
  SELECT *
   FROM a910
   INTO TABLE it_a910
   FOR ALL ENTRIES IN it_zsdt0001_fre
  WHERE kappl EQ 'F'
  AND   kschl EQ 'ZFRE'
  AND   shtyp EQ it_zsdt0001_fre-shtyp
  AND   tdlnr EQ it_zsdt0001_fre-agente_frete
  AND   lzonea EQ it_zsdt0001_fre-lzonea
  AND   lzonez EQ it_zsdt0001_fre-lzonez
  AND   kfrst EQ ''
  AND   datab LE sy-datum
  AND   datbi GE sy-datum
  AND  EXISTS ( SELECT *
                  FROM  konp
                  WHERE knumh = a910~knumh
                  AND   loevm_ko EQ '' ).

  IF it_a910[] IS NOT INITIAL.
    SELECT *
      FROM konp
      APPENDING TABLE it_konp
      FOR ALL ENTRIES IN it_a910
      WHERE knumh = it_a910-knumh
      AND   loevm_ko EQ ''.
  ENDIF.

  "Tp.transp./ForncServ./ItinTransp/Contrato
  SELECT *
    FROM a911
    INTO TABLE it_a911
    FOR ALL ENTRIES IN it_zsdt0001_fre
  WHERE kappl EQ 'F'
  AND   kschl EQ 'ZFRE'
  AND   shtyp EQ it_zsdt0001_fre-shtyp
  AND   tdlnr EQ it_zsdt0001_fre-agente_frete
  AND   route EQ it_zsdt0001_fre-route
  AND   kfrst EQ ''
  AND   datab LE sy-datum
  AND   datbi GE sy-datum
  AND  EXISTS ( SELECT *
                  FROM  konp
                  WHERE knumh = a911~knumh
                  AND   loevm_ko EQ '' ).

  IF it_a911[] IS NOT INITIAL.
    SELECT *
      FROM konp
      APPENDING TABLE it_konp
      FOR ALL ENTRIES IN it_a911
      WHERE knumh = it_a911-knumh
      AND   loevm_ko EQ ''.
  ENDIF.

  " Tp.transp./ForncServ./Zona part./Zona cheg./Agregado
  SELECT *
   FROM a915
   INTO TABLE it_a915
   FOR ALL ENTRIES IN it_zsdt0001_fre
 WHERE kappl EQ 'F'
 AND   kschl EQ 'ZFRE'
 AND   shtyp EQ it_zsdt0001_fre-shtyp
 AND   tdlnr EQ it_zsdt0001_fre-agente_frete
 AND   lzonea EQ it_zsdt0001_fre-lzonea
 AND   lzonez EQ it_zsdt0001_fre-lzonez
 AND   add01  EQ it_zsdt0001_fre-add01
 AND   kfrst EQ ''
 AND   datab LE sy-datum
 AND   datbi GE sy-datum
 AND  EXISTS ( SELECT *
                  FROM  konp
                  WHERE knumh = a915~knumh
                  AND   loevm_ko EQ '' ).

  IF it_a915[] IS NOT INITIAL.
    SELECT *
      FROM konp
      APPENDING TABLE it_konp
      FOR ALL ENTRIES IN it_a915
      WHERE knumh = it_a915-knumh
      AND   loevm_ko EQ ''.
  ENDIF.


  " Tp.transp./ForncServ./Material/Zona part./Zona cheg./Suplem.
  SELECT *
   FROM a918
   INTO TABLE it_a918
   FOR ALL ENTRIES IN it_zsdt0001_fre
   WHERE kappl EQ 'F'
   AND   kschl EQ 'ZFRE'
   AND   shtyp EQ it_zsdt0001_fre-shtyp
   AND   tdlnr EQ it_zsdt0001_fre-agente_frete
   AND   matnr EQ it_zsdt0001_fre-matnr
   AND   lzonea EQ it_zsdt0001_fre-lzonea
   AND   lzonez EQ it_zsdt0001_fre-lzonez
   AND   add01  EQ it_zsdt0001_fre-add01
   AND   kfrst EQ ''
   AND   datab LE sy-datum
   AND   datbi GE sy-datum
   AND  EXISTS ( SELECT *
                  FROM  konp
                  WHERE knumh = a918~knumh
                  AND   loevm_ko EQ '' ).

  IF it_a918[] IS NOT INITIAL.
    SELECT *
      FROM konp
      APPENDING TABLE it_konp
      FOR ALL ENTRIES IN it_a918
      WHERE knumh = it_a918-knumh
      AND   loevm_ko EQ ''.
  ENDIF.

  "Tp.transp./ForncServ./Material/Zona part./Zona cheg.
  SELECT *
   FROM a919
   INTO TABLE it_a919
   FOR ALL ENTRIES IN it_zsdt0001_fre
   WHERE kappl EQ 'F'
   AND   kschl EQ 'ZFRE'
   AND   shtyp EQ it_zsdt0001_fre-shtyp
   AND   tdlnr EQ it_zsdt0001_fre-agente_frete
   AND   matnr EQ it_zsdt0001_fre-matnr
   AND   lzonea EQ it_zsdt0001_fre-lzonea
   AND   lzonez EQ it_zsdt0001_fre-lzonez
   AND   kfrst EQ ''
   AND   datab LE sy-datum
   AND   datbi GE sy-datum
   AND  EXISTS ( SELECT *
                  FROM  konp
                  WHERE knumh = a919~knumh
                  AND   loevm_ko EQ '' ).

  IF it_a919[] IS NOT INITIAL.
    SELECT *
      FROM konp
      APPENDING TABLE it_konp
      FOR ALL ENTRIES IN it_a919
      WHERE knumh = it_a919-knumh
      AND   loevm_ko EQ ''.
  ENDIF.

  SORT: it_a900 BY shtyp tdlnr route add01,
      it_a910 BY shtyp tdlnr lzonea lzonez,
      it_a911 BY shtyp tdlnr route,
      it_a915 BY shtyp tdlnr lzonea lzonez add01,
      it_a918 BY shtyp tdlnr matnr lzonea lzonez add01,
      it_a919 BY shtyp tdlnr matnr lzonea lzonez,
      it_konp BY knumh.
ENDFORM.                    "Pega_FRETE

*&---------------------------------------------------------------------*
*&      Form  f_atual_frete
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_atual_frete  USING  pa_zsdt0001 TYPE ty_zsdt0001 CHANGING p_wa_saida TYPE ty_saida.
  DATA:          v_cont_fre          TYPE i.

  DATA: _vbeln         TYPE  vbeln,
        _placa_cav     TYPE  zplaca,
        _vlr_frete_neg TYPE  zvalor_frete,
        _id_ordem      TYPE  zde_id_ordem.

  v_cont_fre = 0.
  "Vlr Frete
  READ TABLE it_a900 INTO wa_a900 WITH KEY shtyp = pa_zsdt0001-shtyp
                                           tdlnr = pa_zsdt0001-agente_frete
                                           route = pa_zsdt0001-route
                                           add01 = pa_zsdt0001-add01 BINARY SEARCH.
  IF sy-subrc = 0.
    READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a900-knumh BINARY SEARCH.
    IF sy-subrc = 0.
      p_wa_saida-kbetr = wa_konp-kbetr.
      ADD 1 TO v_cont_fre.
    ENDIF.
  ENDIF.

  READ TABLE it_a910 INTO wa_a910 WITH KEY shtyp  = pa_zsdt0001-shtyp
                                           tdlnr  = pa_zsdt0001-agente_frete
                                           lzonea = pa_zsdt0001-lzonea
                                           lzonez = pa_zsdt0001-lzonez BINARY SEARCH.

  IF sy-subrc = 0.
    READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a910-knumh BINARY SEARCH.
    IF sy-subrc = 0.
      p_wa_saida-kbetr = wa_konp-kbetr.
      ADD 1 TO v_cont_fre.
    ENDIF.
  ENDIF.

  READ TABLE it_a911 INTO wa_a911 WITH KEY shtyp = pa_zsdt0001-shtyp
                                           tdlnr = pa_zsdt0001-agente_frete
                                           route = pa_zsdt0001-route BINARY SEARCH.
  IF sy-subrc = 0.
    READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a911-knumh BINARY SEARCH.
    IF sy-subrc = 0.
      p_wa_saida-kbetr = wa_konp-kbetr.
      ADD 1 TO v_cont_fre.
    ENDIF.
  ENDIF.

  READ TABLE it_a915 INTO wa_a915 WITH KEY shtyp  = pa_zsdt0001-shtyp
                                           tdlnr  = pa_zsdt0001-agente_frete
                                           lzonea = pa_zsdt0001-lzonea
                                           lzonez = pa_zsdt0001-lzonez
                                           add01  = pa_zsdt0001-add01 BINARY SEARCH.
  IF sy-subrc = 0.
    READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a915-knumh BINARY SEARCH.
    IF sy-subrc = 0.
      p_wa_saida-kbetr = wa_konp-kbetr.
      ADD 1 TO v_cont_fre.
    ENDIF.
  ENDIF.

  READ TABLE it_a918 INTO wa_a918 WITH KEY shtyp  = pa_zsdt0001-shtyp
                                           tdlnr  = pa_zsdt0001-agente_frete
                                           matnr  = pa_zsdt0001-matnr
                                           lzonea = pa_zsdt0001-lzonea
                                           lzonez = pa_zsdt0001-lzonez
                                           add01  = pa_zsdt0001-add01 BINARY SEARCH.
  IF sy-subrc = 0.
    READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a918-knumh BINARY SEARCH.
    IF sy-subrc = 0.
      p_wa_saida-kbetr = wa_konp-kbetr.
      ADD 1 TO v_cont_fre.
    ENDIF.
  ENDIF.

  READ TABLE it_a919 INTO wa_a919 WITH KEY shtyp  = pa_zsdt0001-shtyp
                                           tdlnr  = pa_zsdt0001-agente_frete
                                           matnr  = pa_zsdt0001-matnr
                                           lzonea = pa_zsdt0001-lzonea
                                           lzonez = pa_zsdt0001-lzonez BINARY SEARCH.
  IF sy-subrc = 0.
    READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a919-knumh BINARY SEARCH.
    IF sy-subrc = 0.
      p_wa_saida-kbetr = wa_konp-kbetr.
      ADD 1 TO v_cont_fre.
    ENDIF.
  ENDIF.

  "Check Alteração Preço por Solicitação - Transação ZLES0153 - CS2016001693
  IF ( p_wa_saida-tipo = 'O' ) AND ( pa_zsdt0001-vbeln IS NOT INITIAL ).
    _vbeln         = pa_zsdt0001-vbeln.
    _placa_cav     = pa_zsdt0001-placa_cav.

    CALL FUNCTION 'ZLES_VALOR_FRETE_ORDEM_CAR'
      EXPORTING
        i_vbeln         = _vbeln
        i_placa_cav     = _placa_cav
        i_shtyp         = pa_zsdt0001-shtyp
        i_id_ordem      = pa_zsdt0001-id_ordem
      IMPORTING
        e_vlr_frete_neg = _vlr_frete_neg
        e_id_ordem      = _id_ordem.

    IF _vlr_frete_neg > 0.
      p_wa_saida-kbetr     = _vlr_frete_neg. "Atribuir Valor de Frete Negociado
      p_wa_saida-id_ordem  = _id_ordem.
    ENDIF.
  ENDIF.

  wa_saida-cont_fre        = v_cont_fre.
ENDFORM.                    "f_atual_frete

*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_imprime_dados .
  PERFORM f_alv_fieldcat.

  wa_layout-zebra      = c_x.
  wa_layout-no_rowmark = ''. "C_X.
  wa_stable-row        = c_x.
  wa_layout-stylefname = 'STYLE'.

  wa_layout-sel_mode   = 'A'.
  wa_layout-cwidth_opt = 'X'.
  wa_layout-col_opt    = 'X'.

  CLEAR wa_layout-grid_title .


  CALL SCREEN 0100.
ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_fieldcat .
  REFRESH it_fieldcat.

  DATA i TYPE i.
  wa_afield-tabname     = 'IT_SAIDA'.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_opt = 'X'.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'ICON'.
  wa_afield-icon          = 'X'.
  wa_afield-scrtext_s = 'Log'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-hotspot       = 'X'.
  APPEND wa_afield TO it_fieldcat.


  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DT_MOVIMENTO'.
  wa_afield-scrtext_l = 'Dt.Movimento'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  wa_afield-outputlen = 12.
  APPEND wa_afield TO it_fieldcat.

*
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NR_ROMANEIO'.
  wa_afield-scrtext_l = 'Romaneio'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'PLACA_CAV'.
  wa_afield-scrtext_l = 'Placa'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'REGION'.
  wa_afield-scrtext_l = 'UF Placa'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  wa_afield-edit      = 'X'.
  wa_afield-f4availabl = 'X'.
  APPEND wa_afield TO it_fieldcat.
*
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'VBELN'.
  wa_afield-scrtext_l = 'Nro.Documento'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'PESO_LIQ'.
  wa_afield-scrtext_l = 'Quantidade'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'INCO1'.
  wa_afield-scrtext_l = 'Tp.Frete'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'ROUTE'.
  wa_afield-scrtext_l = 'Itinerário'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'KBETR'.
  wa_afield-scrtext_l = 'Vlr frete'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'LIFNR'.
  wa_afield-ref_table     = 'LFA1'.
  wa_afield-ref_field     = 'LIFNR'.
  wa_afield-scrtext_l = 'Agente Frete'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  wa_afield-edit      = 'X'.
  wa_afield-f4availabl = 'X'.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'REMESSA'.
  wa_afield-icon          = 'X'.
  wa_afield-hotspot       = 'X'.
  wa_afield-scrtext_l = 'Remessa'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'FATURA'.
  wa_afield-icon          = 'X'.
  wa_afield-hotspot       = 'X'.
  wa_afield-scrtext_l = 'Nro.Fatura'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DANFE'.
  wa_afield-icon          = 'X'.
  wa_afield-hotspot       = 'X'.
  wa_afield-scrtext_l = 'DANFE'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TRANSP'.
  wa_afield-icon          = 'X'.
  wa_afield-hotspot       = 'X'.
  wa_afield-scrtext_l = 'Doc.Transp.'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DOCCUS'.
  wa_afield-icon          = 'X'.
  wa_afield-hotspot       = 'X'.
  wa_afield-scrtext_l = 'Doc.Custo'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'OVSERV'.
  wa_afield-icon          = 'X'.
  wa_afield-hotspot       = 'X'.
  wa_afield-scrtext_l = 'OV.Serviço'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'FATSERV'.
  wa_afield-icon          = 'X'.
  wa_afield-hotspot       = 'X'.
  wa_afield-scrtext_l = 'Fatura Serv.'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DACTE'.
  wa_afield-icon          = 'X'.
  wa_afield-hotspot       = 'X'.
  wa_afield-scrtext_l = 'DACTE'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'OPERACAO'.
  wa_afield-scrtext_l = 'Operação'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  wa_afield-outputlen = 25.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MATERIAL'.
  wa_afield-scrtext_l = 'Produto'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  wa_afield-outputlen = 30.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NAME1_C'.
  wa_afield-scrtext_l = 'Ponto de Coleta'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NAME1'.
  wa_afield-scrtext_l = 'Nome do Destinario'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

ENDFORM.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: fcode       TYPE TABLE OF sy-ucomm,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE.
  REFRESH: fcode.

*  READ TABLE T_USERMD WITH KEY FROM = SY-UNAME.
*  IF SY-SUBRC NE 0.
*    APPEND 'RECU' TO FCODE.
*  ENDIF.


  SET PF-STATUS 'F_SET_PF' EXCLUDING fcode.
  IF sy-tcode = 'ZLES0106' OR sy-tcode = 'ZMM0127'.
    SET TITLEBAR  'ZFTITLE'.
  ELSE.
    SET TITLEBAR  'ZFTITLE2'.
    wg_save = 'X'.
  ENDIF.

  IF sy-tcode = 'ZLES0106' OR sy-tcode = 'ZMM0127'.

    IF cl_container_95 IS INITIAL.
      CREATE OBJECT cl_container_95
        EXPORTING
          side  = '4'
          ratio = '80'.
    ENDIF.

    IF cl_grid IS INITIAL.
      CREATE OBJECT obj_dyndoc_id
        EXPORTING
*         STYLE      =
*         BACKGROUND_COLOR =
*         BDS_STYLESHEET =
          no_margins = 'X'.

      PERFORM zf_alv_header .


      IF editcontainer IS INITIAL .
        CREATE OBJECT editcontainer
          EXPORTING
            container_name = 'HEADER'.
      ENDIF .

      CALL METHOD obj_dyndoc_id->merge_document.

      CALL METHOD obj_dyndoc_id->display_document
        EXPORTING
          reuse_control      = 'X'
          parent             = editcontainer
        EXCEPTIONS
          html_display_error = 1.

      "grafico 1
      CALL METHOD cl_gui_cfw=>flush.
      CREATE OBJECT:
        container EXPORTING container_name = 'CC_IMG',
        picture EXPORTING parent = container.

      PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

      CALL METHOD picture->load_picture_from_url
        EXPORTING
          url = url.

      CALL METHOD picture->set_display_mode
        EXPORTING
          display_mode = picture->display_mode_fit_center.


      CREATE OBJECT cl_grid
        EXPORTING
          i_parent = cl_container_95.
*         I_PARENT      = CL_CONTAINER
*         I_APPL_EVENTS = 'X'.
*
      CALL METHOD cl_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

      "Hints
      PERFORM fill_it_hints.

      wa_stable-row        = c_x.


      wa_layout-info_fname        = 'LINE_COLOR'.
      wa_layout-ctab_fname        = 'COLOR_CELL'.

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

      SET HANDLER:
          lcl_event_handler=>catch_hotspot            FOR cl_grid,
          lcl_event_handler=>on_data_changed_finished FOR cl_grid,
          lcl_event_handler=>on_f4                    FOR cl_grid,
          lcl_event_handler=>set_toolbar              FOR cl_grid,
          lcl_event_handler=>handle_user_command      FOR cl_grid,
          lcl_event_handler=>on_data_changed          FOR cl_grid.


      gs_variant_c-report = sy-repid.
      CALL METHOD cl_grid->set_table_for_first_display
        EXPORTING
          it_toolbar_excluding = tl_function
*         IS_VARIANT           = GS_VARIANT_C          "Modificar Layout
          is_layout            = wa_layout
*         I_SAVE               = WG_SAVE
          it_except_qinfo      = it_except_qinfo
        CHANGING
          it_fieldcatalog      = it_fieldcat[]        "i_default = 'X'
          it_sort              = i_sort[]
          it_outtab            = it_saida[].

*    CALL METHOD CL_GRID->REGISTER_EDIT_EVENT
*      EXPORTING
*        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

      REFRESH gt_f4.
      gt_f4-fieldname = 'REGION'.
      gt_f4-register = 'X'.
      gt_f4-getbefore = 'X'.
      gt_f4-chngeafter ='X'.
      APPEND gt_f4.

      CALL METHOD cl_grid->register_f4_for_fields
        EXPORTING
          it_f4 = gt_f4[].


      CALL METHOD cl_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.
    ELSE.
      CALL METHOD cl_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    ENDIF.
  ELSE.                                                     "zles0107
    IF cl_container_95 IS INITIAL.
      CREATE OBJECT cl_container_95
        EXPORTING
          side  = '4'
          ratio = '80'.
    ENDIF.

    IF cl_grid IS INITIAL.
      CREATE OBJECT obj_dyndoc_id
        EXPORTING
*         STYLE      =
*         BACKGROUND_COLOR =
*         BDS_STYLESHEET =
          no_margins = 'X'.

      PERFORM zf_alv_header .


      IF editcontainer IS INITIAL .
        CREATE OBJECT editcontainer
          EXPORTING
            container_name = 'HEADER'.
      ENDIF .

      CALL METHOD obj_dyndoc_id->merge_document.

      CALL METHOD obj_dyndoc_id->display_document
        EXPORTING
          reuse_control      = 'X'
          parent             = editcontainer
        EXCEPTIONS
          html_display_error = 1.

      "grafico 1
      CALL METHOD cl_gui_cfw=>flush.
      CREATE OBJECT:
        container EXPORTING container_name = 'CC_IMG',
        picture EXPORTING parent = container.

      PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

      CALL METHOD picture->load_picture_from_url
        EXPORTING
          url = url.

      CALL METHOD picture->set_display_mode
        EXPORTING
          display_mode = picture->display_mode_fit_center.


      CREATE OBJECT cl_grid
        EXPORTING
          i_parent = cl_container_95.
*         I_PARENT      = CL_CONTAINER
*         I_APPL_EVENTS = 'X'.
*
      CALL METHOD cl_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

      wa_stable-row        = c_x.


      wa_layout-info_fname        = 'LINE_COLOR'.
      wa_layout-ctab_fname        = 'COLOR_CELL'.

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

      SET HANDLER:
          lcl_event_handler=>catch_hotspot            FOR cl_grid,
          lcl_event_handler=>on_data_changed_finished FOR cl_grid,
          lcl_event_handler=>on_f4                    FOR cl_grid,
          lcl_event_handler=>set_toolbar              FOR cl_grid,
          lcl_event_handler=>handle_user_command      FOR cl_grid,
          lcl_event_handler=>on_data_changed          FOR cl_grid.

      gs_variant_c-report = sy-repid.
      CALL METHOD cl_grid->set_table_for_first_display
        EXPORTING
          it_toolbar_excluding = tl_function
          is_variant           = gs_variant_c          "Modificar Layout
          is_layout            = wa_layout
          i_save               = wg_save
        CHANGING
          it_fieldcatalog      = it_fieldcat[]        "i_default = 'X'
          it_sort              = i_sort[]
          it_outtab            = it_saida[].

      CALL METHOD cl_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      REFRESH gt_f4.
      gt_f4-fieldname = 'REGION'.
      gt_f4-register = 'X'.
      gt_f4-getbefore = 'X'.
      gt_f4-chngeafter ='X'.
      APPEND gt_f4.

      CALL METHOD cl_grid->register_f4_for_fields
        EXPORTING
          it_f4 = gt_f4[].


      CALL METHOD cl_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.
    ELSE.
      CALL METHOD cl_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
      wg_save = 'X'.
    ENDIF.
  ENDIF.                                                    "zles0106
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_alv_header .
  DATA: wl_data(10),
        wl_hora(8),
        wl_linha(60),
        wl_text TYPE sdydo_text_element.

  DATA: wa_t001       TYPE t001,
        wa_j_1bbranch TYPE j_1bbranch.

*  IF P_INTER = ZCL_ROMANEIO=>INTERFACE_CARGA_SAP.
  IF r_dt_a = 'X'.
    wl_linha = 'Romaneios em aberto'.
  ELSE.
    wl_linha = 'Romaneios finalizados'.
  ENDIF.
*  ELSE.
*    WL_LINHA = 'Romaneio de Entrada Completo'.
*  ENDIF.

  wl_text = wl_linha.


  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>extra_large
      sap_color    = cl_dd_area=>list_heading_int.

  SELECT SINGLE *
    FROM t001
    INTO wa_t001
    WHERE bukrs = p_bukrs.

  SELECT SINGLE *
FROM j_1bbranch
INTO wa_j_1bbranch
WHERE bukrs = p_bukrs
AND   branch = p_branch.

  CONCATENATE  'Empresa:' p_bukrs '-' wa_t001-butxt
          INTO wl_linha SEPARATED BY space.
  wl_text = wl_linha.
  CALL METHOD obj_dyndoc_id->new_line.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      sap_fontsize = cl_dd_area=>list_normal.
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  CALL METHOD obj_dyndoc_id->new_line.

  CONCATENATE  'Filial......:' p_branch '-' wa_j_1bbranch-name
        INTO wl_linha SEPARATED BY space.
  wl_text = wl_linha.
  CALL METHOD obj_dyndoc_id->new_line.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      sap_fontsize = cl_dd_area=>list_normal.
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.


ENDFORM.                    " ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: tl_rows   TYPE lvc_t_row,
        sl_rows   TYPE lvc_s_row,
        v_lines   TYPE i,
        wa_agente TYPE lfa1,
        v_imp_doc TYPE j_1bdocnum.

  CASE  ok-code.
    WHEN 'RECU'. " recupera documentos
      CALL METHOD cl_grid->get_selected_rows
        IMPORTING
          et_index_rows = tl_rows.
      v_lines = lines( tl_rows ).

      IF v_lines NE 1.
        MESSAGE 'Selecione uma linha apenas' TYPE 'I'.
        EXIT.
      ENDIF.
      "em 15/02/2017 acrescentado limpar se doc nao existir

      READ TABLE tl_rows INTO sl_rows INDEX 1.
      READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.
      "Remessa
      IF wa_saida-remessa EQ icon_execute_object.
        v_xblnr = wa_saida-ch_referencia.
        SELECT SINGLE *
          FROM likp
          INTO wa_likp
         WHERE xblnr     = v_xblnr
         AND   spe_loekz = ''.
        IF sy-subrc = 0.
          wa_saida-remessa = wa_likp-vbeln.
          MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING remessa.
          UPDATE zsdt0001 SET doc_rem = wa_likp-vbeln
            WHERE ch_referencia = wa_saida-ch_referencia.
        ENDIF.
      ELSE. "LIMPA

        CLEAR: wa_likp.

        SELECT SINGLE *
          FROM likp
          INTO wa_likp
         WHERE vbeln = wa_saida-remessa(10)
         AND   spe_loekz = ''.
        v_xblnr = wa_likp-xblnr+0(20).
        IF sy-subrc NE 0.
          wa_saida-remessa = icon_execute_object.
          MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING remessa.
          UPDATE zsdt0001 SET doc_rem = ''
            WHERE ch_referencia = wa_saida-ch_referencia.
        ELSEIF  v_xblnr NE wa_saida-ch_referencia.
          wa_saida-remessa = icon_execute_object.
          MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING remessa.
          UPDATE zsdt0001 SET doc_rem = ''
            WHERE ch_referencia = wa_saida-ch_referencia.
        ENDIF.

        IF ( wa_likp-vbeln IS NOT INITIAL ) AND ( wa_saida-tipo NE 'O' ).
          SELECT SINGLE *
            FROM vbpa INTO @DATA(_vbpa)
           WHERE vbeln  = @wa_likp-vbeln
             AND parvw  = 'SP'.

          IF ( sy-subrc = 0 ) AND ( _vbpa-lifnr IS NOT INITIAL ).
            wa_saida-lifnr = _vbpa-lifnr.
            MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING lifnr.
            UPDATE zsdt0001 SET agente_frete = _vbpa-lifnr
             WHERE ch_referencia = wa_saida-ch_referencia.
          ENDIF.
        ENDIF.

      ENDIF.
      "fatura
      IF wa_saida-fatura = icon_execute_object AND wa_saida-remessa NE icon_execute_object.
        IF wa_saida-tipo = 'P'.
          SELECT SINGLE vbeln mjahr
                    INTO (vl_vbeln,vl_mjahr)
                    FROM vbfa
                   WHERE vbelv = wa_saida-remessa(10)
                     AND vbtyp_n  = 'R'
                     AND vbtyp_v  = 'J'.
        ELSE.
          SELECT SINGLE vbeln mjahr
                 INTO (vl_vbeln,vl_mjahr)
                 FROM vbfa
                WHERE vbelv = wa_saida-remessa(10)
                  AND vbtyp_n  = 'M'
                  AND vbtyp_v  = 'J'.
        ENDIF.
        IF sy-subrc = 0.
          SELECT SINGLE vbeln mjahr
               INTO (vl_vbeln,vl_mjahr)
               FROM vbfa
              WHERE vbelv = vl_vbeln "Fatura que encontrou
                AND vbtyp_n  = 'N'. "estorno
          IF sy-subrc NE 0. "Não Achou estorno recupera numero
            wa_saida-fatura = vl_vbeln.
            MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING fatura.
            UPDATE zsdt0001 SET fatura_prod = vl_vbeln
              WHERE ch_referencia = wa_saida-ch_referencia.
          ENDIF.
        ENDIF.

      ELSEIF wa_saida-fatura NE icon_execute_object. "limpa
        SELECT SINGLE fksto
            INTO vl_fksto
            FROM vbrk
           WHERE vbeln = wa_saida-fatura.
        IF sy-subrc NE 0. "Fatura não existe
          wa_saida-fatura = icon_execute_object.
          MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING fatura.
          UPDATE zsdt0001 SET fatura_prod = ''
            WHERE ch_referencia = wa_saida-ch_referencia.
        ELSE.
          SELECT SINGLE vbeln mjahr "se estiver estornada
                   INTO (vl_vbeln,vl_mjahr)
                   FROM vbfa
                  WHERE vbelv = wa_saida-fatura
                    AND vbtyp_n  = 'N'. "estorno
          IF sy-subrc = 0. "Achou estorno
            CLEAR w_erro.
            PERFORM f_chk_estorno_fiscal  USING wa_saida CHANGING w_erro.

            IF w_erro IS INITIAL.
              wa_saida-fatura = icon_execute_object.
              MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING fatura.
              UPDATE zsdt0001 SET fatura_prod = ''
                WHERE ch_referencia = wa_saida-ch_referencia.
            ENDIF.
          ELSE.
            IF wa_saida-tipo = 'P'.
              SELECT SINGLE vbelv mjahr
                        INTO (vl_vbeln,vl_mjahr)
                        FROM vbfa
                       WHERE vbeln = wa_saida-fatura
                         AND vbtyp_n  = 'R'
                         AND vbtyp_v  = 'J'.
            ELSE.
              SELECT SINGLE vbelv mjahr
                     INTO (vl_vbeln,vl_mjahr)
                     FROM vbfa
                    WHERE vbeln = wa_saida-fatura
                      AND vbtyp_n  = 'M'
                      AND vbtyp_v  = 'J'.
            ENDIF.
            IF vl_vbeln NE wa_saida-remessa(10). "fatura é de outra remessa
              wa_saida-fatura = icon_execute_object.
              MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING fatura.
              UPDATE zsdt0001 SET fatura_prod = ''
                WHERE ch_referencia = wa_saida-ch_referencia.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.
      "DANFE
      IF wa_saida-danfe = icon_execute_object AND wa_saida-fatura NE icon_execute_object.
        IF wa_saida-tipo = 'P'.
          SELECT SINGLE vbeln mjahr
            INTO (vl_vbeln,vl_mjahr)
            FROM vbfa
            WHERE vbelv = wa_saida-remessa(10)
            AND vbtyp_n  = 'R'
            AND vbtyp_v  = 'J'.

          CONCATENATE vl_vbeln vl_mjahr INTO vl_refkey.
          SELECT SINGLE docnum
            FROM j_1bnflin
            INTO vl_docnum
            WHERE refkey = vl_refkey.
        ELSE.
          SELECT SINGLE docnum
            FROM j_1bnflin
            INTO vl_docnum
            WHERE refkey = wa_saida-fatura.
        ENDIF.

        IF sy-subrc = 0.
          SELECT SINGLE docnum
            FROM j_1bnfe_active
            INTO v_docnum
            WHERE docnum     = vl_docnum
            AND   cancel     = ''
            AND   docsta     = '1'.

          IF sy-subrc = 0.
            wa_saida-danfe          = vl_docnum.
            MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING danfe.
            UPDATE zsdt0001 SET  nro_nf_prod = vl_docnum
              WHERE ch_referencia = wa_saida-ch_referencia.
          ENDIF.
        ENDIF.
      ELSEIF wa_saida-danfe NE icon_execute_object AND wa_saida-fatura NE icon_execute_object.
        IF wa_saida-tipo = 'P'.
          SELECT SINGLE vbeln mjahr
            INTO (vl_vbeln,vl_mjahr)
            FROM vbfa
            WHERE vbelv = wa_saida-remessa(10)
            AND vbtyp_n  = 'R'
            AND vbtyp_v  = 'J'.

          CONCATENATE vl_vbeln vl_mjahr INTO vl_refkey.
          SELECT SINGLE docnum
            FROM j_1bnflin
            INTO vl_docnum
            WHERE refkey = vl_refkey.
        ELSE.
          SELECT SINGLE docnum
            FROM j_1bnflin
            INTO vl_docnum
            WHERE refkey = wa_saida-fatura.
        ENDIF.

        IF sy-subrc = 0.
          SELECT SINGLE docnum
            FROM j_1bnfe_active
            INTO v_docnum
            WHERE docnum     = vl_docnum
            AND   cancel     = 'X'.
          IF sy-subrc = 0 OR vl_docnum NE wa_saida-danfe. "danfe cancelada  OU fatura/remessa não é da DANFE.
            wa_saida-danfe          = icon_execute_object.
            MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING danfe.
            UPDATE zsdt0001 SET  nro_nf_prod = ''
              WHERE ch_referencia = wa_saida-ch_referencia.
          ENDIF.
        ENDIF.
      ELSEIF wa_saida-fatura EQ icon_execute_object.
        wa_saida-danfe          = icon_execute_object.
        MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING danfe.
        UPDATE zsdt0001 SET  nro_nf_prod = ''
          WHERE ch_referencia = wa_saida-ch_referencia.
      ENDIF.

      "DOC TRANSP
      IF wa_saida-transp = icon_execute_object AND wa_saida-remessa NE icon_execute_object.
        SELECT SINGLE vttk~tknum
              INTO  v_tknum
              FROM vbfa
             INNER JOIN vttk
             ON  vttk~tknum = vbfa~vbeln
             AND vttk~vsart   = '01'
             WHERE vbfa~vbelv = wa_saida-remessa(10)
               AND vbfa~vbtyp_n  = '8'
               AND vbfa~vbtyp_v  = 'J'.
        IF sy-subrc = 0.
          wa_saida-transp = v_tknum.
          MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING transp.
          UPDATE zsdt0001 SET doc_transp = v_tknum
            WHERE ch_referencia = wa_saida-ch_referencia.
        ENDIF.
      ELSEIF wa_saida-transp NE icon_execute_object.
        SELECT SINGLE vttk~tknum
            INTO  v_tknum
            FROM vbfa
           INNER JOIN vttk
           ON  vttk~tknum = vbfa~vbeln
           AND vttk~vsart   = '01'
           WHERE vbfa~vbelv = wa_saida-remessa(10)
             AND vbfa~vbtyp_n  = '8'
             AND vbfa~vbtyp_v  = 'J'.
        IF sy-subrc NE 0.
          wa_saida-transp = icon_execute_object.
          MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING transp.
          UPDATE zsdt0001 SET doc_transp = ''
            WHERE ch_referencia = wa_saida-ch_referencia.
        ELSEIF wa_saida-transp NE v_tknum.
          wa_saida-transp = v_tknum.
          MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING transp.
          UPDATE zsdt0001 SET doc_transp = v_tknum
            WHERE ch_referencia = wa_saida-ch_referencia.
        ENDIF.
      ENDIF.
      "DOC.CUSTO
      IF wa_saida-doccus+0(1) = '@' AND wa_saida-transp NE icon_execute_object.
        SELECT SINGLE fknum
          INTO vl_fknum
        FROM vfkp
         WHERE rebel = wa_saida-transp.
        IF sy-subrc = 0.
          wa_saida-doccus = vl_fknum.
          MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING doccus.
          UPDATE zsdt0001 SET fknum = vl_fknum
            WHERE ch_referencia = wa_saida-ch_referencia.
        ENDIF.
      ELSEIF wa_saida-doccus NE icon_icon_list.
        SELECT SINGLE fknum
          INTO vl_fknum
        FROM vfkp
         WHERE rebel = wa_saida-transp.
        IF sy-subrc NE 0.
          wa_saida-doccus = icon_icon_list.
          MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING doccus.
          UPDATE zsdt0001 SET fknum = ''
            WHERE ch_referencia = wa_saida-ch_referencia.
        ENDIF.
      ENDIF.
      "OV Serviço
      IF wa_saida-ovserv+0(1) = '@' AND wa_saida-transp NE icon_execute_object.
        SELECT SINGLE  vbeln auart  kunnr
        FROM vbak
        INTO wa_vbak
        WHERE tknum = wa_saida-transp.
        IF sy-subrc = 0.
          wa_saida-ovserv = wa_vbak-vbeln.
          MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING ovserv.
          UPDATE zsdt0001 SET ov_frete = wa_vbak-vbeln
            WHERE ch_referencia = wa_saida-ch_referencia.
        ENDIF.
      ELSEIF wa_saida-ovserv NE icon_icon_list.
        SELECT SINGLE  vbeln auart  kunnr
       FROM vbak
       INTO wa_vbak
       WHERE tknum = wa_saida-transp.
        IF sy-subrc NE 0.
          wa_saida-ovserv = icon_icon_list.
          MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING ovserv.
          UPDATE zsdt0001 SET ov_frete = ''
            WHERE ch_referencia = wa_saida-ch_referencia.
        ENDIF.
      ENDIF.
      "Fatura Frete
      IF wa_saida-fatserv+0(1) = '@' AND wa_saida-ovserv NE icon_icon_list.
        SELECT SINGLE vbeln mjahr
           INTO (vl_vbeln,vl_mjahr)
           FROM vbfa
           WHERE vbelv = wa_saida-ovserv
           AND vbtyp_n  = 'M'
           AND vbtyp_v  = 'C'.
        IF sy-subrc = 0.
          SELECT SINGLE vbeln mjahr
              INTO (vl_vbeln,vl_mjahr)
              FROM vbfa
             WHERE vbelv = vl_vbeln "Fatura que encontrou
               AND vbtyp_n  = 'N'. "estorno
          IF sy-subrc NE 0. "Não Achou estorno recupera numero
            wa_saida-fatserv = vl_vbeln.
            MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING fatserv.
            UPDATE zsdt0001 SET fatura_frete = vl_vbeln
              WHERE ch_referencia = wa_saida-ch_referencia.
          ENDIF.
        ENDIF.
      ELSEIF wa_saida-fatserv NE icon_icon_list.
        SELECT SINGLE fksto
           INTO vl_fksto
           FROM vbrk
          WHERE vbeln = wa_saida-fatserv.
        IF sy-subrc NE 0. "Fatura não existe
          wa_saida-fatserv = icon_icon_list.
          MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING fatserv.
          UPDATE zsdt0001 SET fatura_frete = ''
            WHERE ch_referencia = wa_saida-ch_referencia.
        ELSE.
          SELECT SINGLE vbeln mjahr
                 INTO (vl_vbeln,vl_mjahr)
                 FROM vbfa
                WHERE vbelv = wa_saida-fatserv
                  AND vbtyp_n  = 'N'. "estorno
          IF sy-subrc = 0. "Achou estorno
            wa_saida-fatserv = icon_icon_list.
            MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING fatserv.
            UPDATE zsdt0001 SET fatura_frete = ''
              WHERE ch_referencia = wa_saida-ch_referencia.
          ENDIF.
        ENDIF.
      ENDIF.
      SELECT SINGLE *
        FROM zsdt0001
        INTO wa_zsdt0001
       WHERE ch_referencia = wa_saida-ch_referencia.
      IF wa_saida-remessa NE icon_execute_object.
        wa_saida-st_proc = '01'.
      ELSE.
        wa_saida-st_proc = ''.
      ENDIF.
      IF wa_saida-fatura NE icon_execute_object.
        wa_saida-st_proc = '02'.
      ENDIF.
      IF wa_saida-danfe NE icon_execute_object.
        wa_saida-st_proc = '03'.
        IF wa_saida-inco1 = 'FOB' OR wa_saida-inco1 = 'CFR'.
          wa_saida-st_proc = '99'.
        ENDIF.
      ENDIF.
      IF wa_saida-transp NE icon_execute_object.
        wa_saida-st_proc = '04'.
      ENDIF.
      IF wa_saida-doccus NE icon_icon_list.
        wa_saida-st_proc = '05'.
        IF wa_saida-inco1 = 'CPT'.
          wa_saida-st_proc = '99'.
        ENDIF.
      ENDIF.
      IF wa_saida-ovserv NE icon_icon_list.
        wa_saida-st_proc = '06'.
      ENDIF.
      IF wa_saida-fatserv NE icon_icon_list.
        wa_saida-st_proc = '07'.
      ENDIF.
      IF wa_saida-dacte NE icon_execute_object AND wa_saida-inco1 NE 'CPT'.
        wa_saida-st_proc = '99'.
      ENDIF.
      "

      MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING st_proc.
      UPDATE zsdt0001 SET st_proc = wa_saida-st_proc
           WHERE ch_referencia = wa_saida-ch_referencia.

      CALL METHOD cl_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    WHEN 'CARTA'.
      CALL METHOD cl_grid->get_selected_rows
        IMPORTING
          et_index_rows = tl_rows.
      v_lines = lines( tl_rows ).

      IF v_lines NE 1.
        MESSAGE 'Selecione uma linha apenas' TYPE 'I'.
        EXIT.
      ENDIF.

      READ TABLE tl_rows INTO sl_rows INDEX 1.
      READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.
      "
      IF wa_saida-transp NE icon_execute_object.
        MESSAGE i000(z01) WITH 'Dados de transporte '
                               'deve ser estornado'.
        EXIT.
      ENDIF.

      IF wa_saida-danfe EQ icon_execute_object.
        MESSAGE i000(z01) WITH 'Não existe DANFE'
                               'Autorizado'.
        EXIT.
      ENDIF.

      SELECT SINGLE *
        FROM lfa1
        INTO wa_agente
      WHERE lifnr = wa_saida-lifnr.

      IF sy-subrc = 0.
        wa_ag_frete-transpor1  = wa_agente-lifnr.
        wa_ag_frete-name1      = wa_agente-name1.
        wa_ag_frete-cnpj1      = wa_agente-stcd1.
        wa_ag_frete-inscr1     = wa_agente-stcd3.
        CALL SCREEN 0300     STARTING AT 020 1
                             ENDING   AT 145 15.
      ENDIF.

    WHEN 'EX_TRA'.
      CALL METHOD cl_grid->get_selected_rows
        IMPORTING
          et_index_rows = tl_rows.

      v_lines = lines( tl_rows ).

      IF v_lines NE 1.
        MESSAGE 'Selecione uma linha apenas' TYPE 'I'.
        EXIT.
      ENDIF.

      READ TABLE tl_rows INTO sl_rows INDEX 1.
      READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.
      "
      IF wa_saida-inco1  NE 'CIF'.
        MESSAGE i000(z01) WITH 'Dados de transporte não disponível '
                               'para tipo de frete:'
                               wa_saida-inco1.
        EXIT.
      ENDIF.
      "
*      IF WA_SAIDA-TRANSP NE ICON_EXECUTE_OBJECT.
*        MESSAGE 'Alteração de motorista,permitida antes de gerar Doc. Transp' TYPE 'I'.
*        EXIT.
*      ENDIF.

      CLEAR: wa_trans,wa_mot.
      REFRESH it_veic.
      SELECT SINGLE z~motorista la~name1 la~stcd2 la~stcd4 lb~zsabe la~stcd3 lb~eikto
        FROM zsdt0001 AS z
        INNER JOIN lfa1 AS la ON la~lifnr = z~motorista
        INNER JOIN lfb1 AS lb ON lb~lifnr = z~motorista "AND LB~BUKRS = P_BUKRS
      INTO wa_mot
      WHERE ch_referencia = wa_saida-ch_referencia.

      SELECT SINGLE lifnr name1 stcd1
        FROM lfa1
        INTO wa_trans
        WHERE lifnr = wa_saida-lifnr.

      SELECT z1~placa_cav z2~tp_veiculo z2~proprietario la~name1 la~bahns z2~cd_renavam la~stcd1 la~stcd2 z2~cd_cidade z2~cd_uf
        FROM zsdt0001 AS z1
        INNER JOIN zlest0002 AS z2 ON z2~pc_veiculo  = z1~placa_cav
        INNER JOIN lfa1      AS la ON la~lifnr       = z2~proprietario
      INTO TABLE it_veic
      WHERE ch_referencia = wa_saida-ch_referencia.

      SELECT z2~pc_veiculo z2~tp_veiculo z2~proprietario la~name1 la~bahns z2~cd_renavam la~stcd1 la~stcd2 z2~cd_cidade z2~cd_uf
         FROM zsdt0001 AS z1
         INNER JOIN zlest0002 AS z2 ON z2~pc_veiculo  = z1~placa_car1
       INNER JOIN lfa1      AS la ON la~lifnr       = z2~proprietario
       APPENDING TABLE it_veic
       WHERE ch_referencia = wa_saida-ch_referencia.

      SELECT z2~pc_veiculo z2~tp_veiculo z2~proprietario la~name1 la~bahns z2~cd_renavam la~stcd1 la~stcd2 z2~cd_cidade z2~cd_uf
         FROM zsdt0001 AS z1
         INNER JOIN zlest0002 AS z2 ON z2~pc_veiculo  = z1~placa_car2
       INNER JOIN lfa1      AS la ON la~lifnr       = z2~proprietario
       APPENDING TABLE it_veic
       WHERE ch_referencia = wa_saida-ch_referencia.

      SELECT z2~pc_veiculo z2~tp_veiculo z2~proprietario la~name1 la~bahns z2~cd_renavam la~stcd1 la~stcd2 z2~cd_cidade z2~cd_uf
         FROM zsdt0001 AS z1
         INNER JOIN zlest0002 AS z2 ON z2~pc_veiculo  = z1~placa_car3
       INNER JOIN lfa1      AS la ON la~lifnr       = z2~proprietario
       APPENDING TABLE it_veic
       WHERE ch_referencia = wa_saida-ch_referencia.

      CALL SCREEN 0200     STARTING AT 020 1
                           ENDING   AT 140 23.
    WHEN 'DACTE'.
      CALL METHOD cl_grid->get_selected_rows
        IMPORTING
          et_index_rows = tl_rows.

      v_lines = lines( tl_rows ).

      IF v_lines NE 1.
        MESSAGE 'Selecione uma linha para impressão' TYPE 'I'.
        EXIT.
      ENDIF.

      READ TABLE tl_rows INTO sl_rows INDEX 1.
      READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.

      IF wa_saida-dacte = icon_execute_object.
        MESSAGE 'CTE não gerado' TYPE 'I'.
      ELSE.
        v_imp_doc = wa_saida-dacte.
        SELECT SINGLE docnum
          FROM j_1bnfe_active
          INTO v_docnum
         WHERE docnum     = v_imp_doc
           AND   cancel       = ''.

        IF sy-subrc NE 0.
        ELSE.
          CALL FUNCTION 'Z_SD_PRINT_NFE_CTE'
            EXPORTING
              doc_numero     = v_imp_doc
            EXCEPTIONS
              nao_localizado = 1
              OTHERS         = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN 'DANFE'.
      CALL METHOD cl_grid->get_selected_rows
        IMPORTING
          et_index_rows = tl_rows.
      v_lines = lines( tl_rows ).

      IF v_lines NE 1.
        MESSAGE 'Selecione uma linha para impressão' TYPE 'I'.
        EXIT.
      ENDIF.

      READ TABLE tl_rows INTO sl_rows INDEX 1.
      READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.

      IF wa_saida-danfe = icon_execute_object. " Executar ZOPUS
        MESSAGE 'Nota não gerada' TYPE 'I'.
      ELSE.
        SELECT SINGLE docnum
        FROM j_1bnfe_active
        INTO v_docnum
        WHERE docnum     = wa_saida-danfe
        AND   cancel       = ''.
        IF sy-subrc NE 0.

        ELSE.
          v_imp_doc = wa_saida-danfe.
          CALL FUNCTION 'Z_SD_PRINT_NFE_CTE'
            EXPORTING
              doc_numero     = v_imp_doc
            EXCEPTIONS
              nao_localizado = 1
              OTHERS         = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN 'EST_CTE'.
      IF sy-tcode NE 'ZLES0106' AND sy-tcode NE 'ZMM0127'.
        MESSAGE 'Transação apenas de visualização' TYPE 'I'.
        EXIT.
      ENDIF.

      CALL METHOD cl_grid->get_selected_rows
        IMPORTING
          et_index_rows = tl_rows.

      v_lines = lines( tl_rows ).

      IF v_lines IS INITIAL.
        MESSAGE 'Selecione uma linha para o estorno.' TYPE 'I'.
        EXIT.
      ELSEIF v_lines > 1.
        MESSAGE 'Selecionar apenas uma linha de fatura/remessa.' TYPE 'I'.
        EXIT.
      ENDIF.

      LOOP AT tl_rows INTO sl_rows.
        READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.

        IF '07_06_05_04' CS wa_saida-st_proc. " processo estorno não completo
          REFRESH ti_zlest0100.
          "Bloqueia romaneio
          CALL FUNCTION 'ENQUEUE_EZSDT0001'
            EXPORTING
              ch_referencia  = wa_saida-ch_referencia
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

*        UPDATE ZSDT0001 SET ST_PROC = WA_SAIDA-ST_PROC
*         WHERE CH_REFERENCIA = WA_SAIDA-CH_REFERENCIA.

          SELECT SINGLE st_proc
               FROM zsdt0001
               INTO  wa_saida-st_proc
                 WHERE ch_referencia = wa_saida-ch_referencia.

          PERFORM f_estorno_cte CHANGING wa_saida.
          IF ti_zlest0100[] IS NOT INITIAL.
            wa_saida-icon = icon_led_red.
          ELSE.
            CLEAR wa_saida-icon.
          ENDIF.

          "desbloqueia romaneio
          CALL FUNCTION 'DEQUEUE_EZSDT0001'
            EXPORTING
              ch_referencia = wa_saida-ch_referencia.

          MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING   dacte fatserv ovserv transp doccus icon .
          CALL METHOD cl_grid->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
        ELSEIF wa_saida-st_proc = '99'.
          IF wa_saida-inco1 = 'CPT'. "estorna mesmo finalizado se CPT
            "Bloqueia romaneio
            CALL FUNCTION 'ENQUEUE_EZSDT0001'
              EXPORTING
                ch_referencia  = wa_saida-ch_referencia
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

            UPDATE zsdt0001 SET st_proc = wa_saida-st_proc
             WHERE ch_referencia = wa_saida-ch_referencia.

            PERFORM f_estorno_custo CHANGING wa_saida.

            "desbloqueia romaneio
            CALL FUNCTION 'DEQUEUE_EZSDT0001'
              EXPORTING
                ch_referencia = wa_saida-ch_referencia.

            MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING   dacte fatserv ovserv transp doccus icon .
            CALL METHOD cl_grid->refresh_table_display
              EXPORTING
                is_stable = wa_stable.

          ELSE.
            MESSAGE 'Dacte deve ser não autorizada para este processo.' TYPE  'I'.
          ENDIF.
        ELSE.
          MESSAGE 'Não existe documentos de transporte para estorno' TYPE  'I'.
        ENDIF.
      ENDLOOP.

    WHEN 'EST_NFE'.
      DATA index_estorno TYPE sy-tabix.

      IF sy-tcode NE 'ZLES0106' AND sy-tcode NE 'ZMM0127'.
        MESSAGE 'Transação apenas de visualização' TYPE 'I'.
        EXIT.
      ENDIF.

      CALL METHOD cl_grid->get_selected_rows
        IMPORTING
          et_index_rows = tl_rows.

      v_lines = lines( tl_rows ).

      IF v_lines IS INITIAL.
        MESSAGE 'Selecione uma linha para o estorno.' TYPE 'I'.
        EXIT.
      ELSEIF v_lines > 1.
        MESSAGE 'Selecionar apenas uma linha de fatura/remessa.' TYPE 'I'.
        EXIT.
      ENDIF.

      DATA(_saida_aux) = it_saida[ tl_rows[ 1 ]-index ].
      IF line_exists( t_fatura_agrupada[ werks = p_branch kunnr = _saida_aux-kunnr inco1 = _saida_aux-inco1 ] ).

        CLEAR tl_rows[].
        LOOP AT it_saida INTO it_saida WHERE ( dt_movimento EQ _saida_aux-dt_movimento ) AND
                                             ( matnr        EQ _saida_aux-matnr        ) AND
                                             ( kunnr        EQ _saida_aux-kunnr        ) AND
                                             ( operacao(4)  EQ _saida_aux-operacao(4)  ) AND
                                             ( inco1        EQ _saida_aux-inco1        ).

          CHECK ( it_saida-remessa IS NOT INITIAL AND it_saida-remessa NE icon_execute_object
             OR   it_saida-fatura  IS NOT INITIAL AND it_saida-fatura  NE icon_execute_object ).

          APPEND VALUE #( index = sy-tabix ) TO tl_rows.
        ENDLOOP.

        IF lines( tl_rows ) > 1.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Estorno de Fatura Agrupada'
              text_question         = 'As faturas e os documentos de remessa serão estornados. Tem certeza que deseja continuar?'
              text_button_1         = 'Sim'(100)
              icon_button_1         = 'ICON_OKAY '
              text_button_2         = 'Não'(101)
              icon_button_2         = 'ICON_CANCEL'
              default_button        = '1'
              display_cancel_button = ' '
              start_column          = 25
              start_row             = 6
            IMPORTING
              answer                = w_answer
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.

          CHECK w_answer = 1.
        ENDIF.
      ENDIF.

      CLEAR index_estorno.
      LOOP AT tl_rows INTO sl_rows.
        READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.

        ADD 1 TO index_estorno.

        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            text = |Estornando documento(s) { index_estorno } de { lines( tl_rows ) }.|.

        "Verifica se DACTE foi estornada
        IF wa_saida-fatserv    NE icon_execute_object
          AND wa_saida-fatserv NE icon_icon_list
          AND wa_saida-fatserv IS NOT INITIAL
          AND wa_saida-fatserv NE ''.
          MESSAGE 'Estorne desde a DACTE' TYPE  'I'.
          EXIT.
        ENDIF.

        IF '05_04_02_01' CS wa_saida-st_proc. " processo estorno não completo.
          "Bloqueia romaneio
          CALL FUNCTION 'ENQUEUE_EZSDT0001'
            EXPORTING
              ch_referencia  = wa_saida-ch_referencia
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

*        UPDATE ZSDT0001 SET ST_PROC = WA_SAIDA-ST_PROC
*              WHERE CH_REFERENCIA = WA_SAIDA-CH_REFERENCIA.

          SELECT SINGLE st_proc
            FROM zsdt0001
            INTO  wa_saida-st_proc
              WHERE ch_referencia = wa_saida-ch_referencia.

          REFRESH ti_zlest0100.
          IF ( '05_04' CS wa_saida-st_proc ).
            IF ( wa_saida-inco1 = 'CPT' ).
              wa_saida-st_proc = '05'.

              UPDATE zsdt0001
                 SET st_proc       = wa_saida-st_proc
               WHERE ch_referencia = wa_saida-ch_referencia.

              PERFORM f_estorno_custo CHANGING wa_saida.
              IF ti_zlest0100[] IS  INITIAL.
                wa_saida-st_proc = '02'.
              ENDIF.
            ENDIF.
          ENDIF.

          IF ( '02_01' CS wa_saida-st_proc ).
            PERFORM f_estorno_nfe CHANGING wa_saida.

            IF ti_zlest0100[] IS NOT INITIAL.
              wa_saida-icon = icon_led_red.
            ELSE.
              CLEAR wa_saida-icon.
            ENDIF.
          ENDIF.

          "desbloqueia romaneio
          CALL FUNCTION 'DEQUEUE_EZSDT0001'
            EXPORTING
              ch_referencia = wa_saida-ch_referencia.

          "//Wait until all the process be complete
*          WAIT UP TO 3 SECONDS.
          "//

          REFRESH style.
          IF wa_saida-lifnr IS INITIAL.
            IF t_fatura_agrupada IS NOT INITIAL AND 'CFR_FOB' CS wa_saida-inco1.
              wa_style-fieldname = 'LIFNR'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT wa_style INTO TABLE style .
            ELSE.
              DELETE wa_saida-style WHERE fieldname EQ 'LIFNR'.
            ENDIF.
          ELSE.
            wa_style-fieldname = 'LIFNR'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .
          ENDIF.
          "
          IF wa_saida-region IS INITIAL.
            DELETE wa_saida-style WHERE fieldname EQ 'REGION'.
          ELSE.
            wa_style-fieldname = 'REGION'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .
          ENDIF.

          wa_saida-style = style[].
          MODIFY it_saida FROM wa_saida INDEX sl_rows-index TRANSPORTING region remessa lifnr fatura danfe icon style.

        ELSE.
          MESSAGE 'Não existem documentos referentes a NF-e para estorno.' TYPE  'I'.
        ENDIF.
      ENDLOOP.

      CALL METHOD cl_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    WHEN 'REFRESH'.
      REFRESH it_saida.
      CALL METHOD cl_grid->refresh_table_display.
      PERFORM:
          f_seleciona_dados, " Form seleciona dados
          f_saida. " Form de saida
      CALL METHOD cl_grid->refresh_table_display.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_GERAR_VT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_gerar_vt CHANGING p_erro.

  TYPES: BEGIN OF ty_vbpa,
           parvw TYPE vbpa-parvw,
           kunnr TYPE vbpa-kunnr,
           lifnr TYPE vbpa-lifnr,
         END OF ty_vbpa.

  DATA: wl_tvro    TYPE tvro,
        it_vbpa    TYPE TABLE OF ty_vbpa,
        wl_vbpa    TYPE ty_vbpa,
        wl_a917    TYPE a917,
        wa_konp    TYPE konp,

        v_auart    TYPE vbak-auart,
        v_bsart    TYPE ekko-bsart,
        v_lifnr    TYPE lfa1-lifnr,
        v_ktokk    TYPE lfa1-ktokk,
        v_lzonel   TYPE lfa1-lzone,
        v_lzonek   TYPE kna1-lzone,
        v_kunnr    TYPE vbpa-kunnr,
        v_stcd1    TYPE kna1-stcd1,
        v_route    TYPE trolz-route,
        v_knote    TYPE tvkn-knote,
        v_msgi(50).

  " Tabelas BAPI
  DATA:st_headerdata       TYPE bapishipmentheader,
       st_stagedata        TYPE bapishipmentstage,
       t_stagedata         TYPE TABLE OF bapishipmentstage,
       t_itemdata          TYPE TABLE OF bapishipmentitem,
       st_itemdata         TYPE bapishipmentitem,

       st_headerdata2      TYPE bapishipmentheader,
       st_headerdataaction TYPE bapishipmentheaderaction,
       lc_saida            TYPE ty_saida.


  IF wa_saida-transp = icon_execute_object AND wa_saida-remessa NE icon_execute_object.
    SELECT SINGLE vttk~tknum INTO  v_tknum
      FROM vbfa INNER JOIN vttk ON  vttk~tknum = vbfa~vbeln
       AND vttk~vsart   = '01'
     WHERE vbfa~vbelv = wa_saida-remessa(10)
       AND vbfa~vbtyp_n  = '8'
       AND vbfa~vbtyp_v  = 'J'.
    IF sy-subrc = 0.
      MESSAGE i000(z01) WITH 'Documento de transporte já gerado! Atualizar linha de documentos!'.
      EXIT.
    ENDIF.
  ENDIF.

  IF wa_saida-inco1 NE 'FOB' AND wa_saida-inco1 NE 'CFR'.
    IF wa_saida-kbetr LE 0.
      MESSAGE i000(z01) WITH 'Não existe valor de frete cadastrado.'
                             'Solicite à transportadora da sua região'.
      EXIT.
    ENDIF.
    IF wa_saida-cont_fre GT 1.
      MESSAGE i000(z01) WITH 'Existe mais de um valor de frete cadastrado.'
                             'Solicite a regularização à transportadora '
                             'da sua região'.
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY ch_referencia = wa_saida-ch_referencia BINARY SEARCH.
  CHECK sy-subrc = 0.

  IF wa_zsdt0001-doc_rem IS INITIAL.
    SELECT SINGLE doc_rem INTO wa_zsdt0001-doc_rem FROM zsdt0001 WHERE ch_referencia = wa_saida-ch_referencia.
  ENDIF.

  IF wa_saida-inco1 = 'CIF'.
    "Identificar se existe valor de seguro e IOF cadastrado, antes da criação do documento de transporte
    SELECT SINGLE *
      FROM a917
      INTO wl_a917
     WHERE kappl = 'F'
       AND kschl        = 'ZSEG'
       AND matnr        = wa_zsdt0001-matnr
       AND tdlnr        = wa_zsdt0001-agente_frete
       AND kfrst        = ''
       AND datbi        GE sy-datum.

    IF sy-subrc IS NOT INITIAL.
      CONCATENATE 'Agente:' wa_zsdt0001-agente_frete 'Solicite ao depto de logística' INTO v_msgi SEPARATED BY space.
      MESSAGE i000(z01) WITH 'Não existe % p/ desc. de Seguro. Mat.:' wa_zsdt0001-matnr v_msgi.
      EXIT.
    ELSE.
      wa_konp-knumh = wl_a917-knumh.
      wa_konp-konwa = '%'.

      SELECT SINGLE * INTO wa_konp
        FROM konp
       WHERE knumh    EQ wa_konp-knumh
         AND konwa    EQ wa_konp-konwa
         AND loevm_ko EQ abap_false.

      IF sy-subrc IS NOT INITIAL.
        CONCATENATE 'Agente:' wa_zsdt0001-agente_frete 'Solicite ao depto de logística' INTO v_msgi SEPARATED BY space.
        MESSAGE i000(z01) WITH 'Cancelado % p/ desc. de Seguro. Mat.:' wa_zsdt0001-matnr v_msgi.
        EXIT.
      ENDIF.
    ENDIF.

    "Identificar se existe valor de seguro e IOF cadastrado, antes da criação do documento de transporte
    SELECT SINGLE *
      FROM a917
      INTO wl_a917
     WHERE kappl = 'F'
       AND kschl = 'ZIOF'
       AND matnr = wa_zsdt0001-matnr
       AND tdlnr = wa_zsdt0001-agente_frete
       AND kfrst = ''
       AND datbi GE sy-datum.

    IF sy-subrc NE 0.
      CONCATENATE 'Agente:' wa_zsdt0001-agente_frete 'Solicite ao depto de logística' INTO v_msgi SEPARATED BY space.
      MESSAGE i000(z01) WITH 'Não existe % p/ desc. de IOF Mat.:' wa_zsdt0001-matnr v_msgi.
      EXIT.
    ELSE.
      wa_konp-knumh = wl_a917-knumh.
      wa_konp-konwa = '%'.

      SELECT SINGLE * INTO wa_konp
        FROM konp
       WHERE knumh    EQ wa_konp-knumh
         AND konwa    EQ wa_konp-konwa
         AND loevm_ko EQ abap_false.

      IF sy-subrc IS NOT INITIAL.
        CONCATENATE 'Agente:' wa_zsdt0001-agente_frete 'Solicite ao depto de logística' INTO v_msgi SEPARATED BY space.
        MESSAGE i000(z01) WITH 'Cancelado % p/ desc. de IOF Mat.:' wa_zsdt0001-matnr v_msgi.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  " preenche BAPI
  " Cabeçalho
  CLEAR st_headerdata.

  st_headerdata-service_agent_id        = wa_zsdt0001-agente_frete.
  st_headerdata-service_level           = '1'.
  st_headerdata-shipping_type           = '01'.
  st_headerdata-status_plan             = 'X'.
  st_headerdata-status_checkin          = 'X'.
  st_headerdata-status_load_start       = 'X'.
  st_headerdata-special_procedure_id 	  = '0001'.
  st_headerdata-shpmnt_cost_rel         = 'X'.
  st_headerdata-shipment_type           = wa_zsdt0001-shtyp.
  st_headerdata-trans_plan_pt           = wa_zsdt0001-branch.

  v_lifnr = wa_saida-lifnr_c.

  SELECT SINGLE lzone ktokk
    FROM lfa1
    INTO (v_lzonel, v_ktokk)
    WHERE lifnr   = v_lifnr.

  CHECK sy-subrc = 0.

  CLEAR v_route.

  v_route = wa_saida-route.
  st_headerdata-shipment_route  = v_route.

  SELECT SINGLE *
    FROM tvro
    INTO wl_tvro
    WHERE route    = v_route.

  CHECK sy-subrc = 0.
  IF wl_tvro-traztd GT 24.
    wl_tvro-traztd = wl_tvro-traztd / 24.
  ENDIF.

  st_headerdata-distance        = wl_tvro-distz.
  st_headerdata-distance_unit   = wl_tvro-medst.
  st_headerdata-time_travel     =	wl_tvro-traztd.
  st_headerdata-time_unit       =	'H'.

*ETAPA
  CLEAR st_stagedata.
  REFRESH t_stagedata.
  st_stagedata-stage_cat    = '1'.
  st_stagedata-stage_seq 	= '0001'.
  st_stagedata-shipping_type  =  '01'.
  st_stagedata-service_agent  = wa_zsdt0001-agente_frete.

  "Local de Partida
  IF v_ktokk = 'ZFIC'.
    st_stagedata-org_shipp_dpmnt = v_lifnr+6(4).
  ELSE.
    st_stagedata-org_suppl  = v_lifnr.
  ENDIF.

  "Local de entrega/chegada
  SELECT SINGLE kunnr
    FROM vbpa
    INTO v_kunnr
   WHERE vbeln   = wa_zsdt0001-doc_rem
     AND parvw   = 'LR'.  "Local de entrega

  CHECK sy-subrc = 0.

  SELECT SINGLE shtyp laufk
    FROM tvtk
    INTO wa_tvtk
   WHERE shtyp = wa_zsdt0001-shtyp.

  st_stagedata-leg_indicator = wa_tvtk-laufk. "Código de Percurso

  "Se Código de Percurso  igual a 1:  Percurso preliminar
  IF wa_tvtk-laufk = 1.
    CLEAR v_knote.

    SELECT SINGLE knote
      FROM tvkn
      INTO v_knote
     WHERE kunnr   = v_kunnr.

    IF sy-subrc = 0.
      st_stagedata-dest_point   = v_knote.
    ELSE.
      st_stagedata-dest_point   = v_kunnr.
    ENDIF.

    "Se Código de Percurso  igual a 4:  Percurso direto
  ELSEIF wa_tvtk-laufk = 4.
    IF st_headerdata-shipment_type  =  'Z004'.
      SELECT SINGLE stcd1
        FROM kna1
        INTO v_stcd1
       WHERE kunnr = v_kunnr.

      SELECT SINGLE lifnr
        FROM lfa1
        INTO v_lifnr
       WHERE stcd1 = v_stcd1.

      st_stagedata-dest_suppl  = v_lifnr.

    ELSE.
      st_stagedata-dest_cust    = v_kunnr. "Local de entrega (V_KUNNR)
    ENDIF.

  ENDIF.

*  ST_STAGEDATA-DEST_PLANT    =     xxxxx     (Local de chegada: centro)

  APPEND st_stagedata TO t_stagedata.

*Dados itens
  CLEAR st_itemdata.
  REFRESH t_itemdata.
  st_itemdata-delivery      =  wa_zsdt0001-doc_rem.
  st_itemdata-itenerary     =   '000010'.
  APPEND st_itemdata TO t_itemdata.

  CLEAR v_tknum.
  REFRESH t_return_vt.
  "------>Gera o Transporte <------
  CALL FUNCTION 'BAPI_SHIPMENT_CREATE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      headerdata = st_headerdata
    IMPORTING
      transport  = v_tknum
    TABLES
      itemdata   = t_itemdata
      stagedata  = t_stagedata
      return     = t_return_vt.

  IF v_tknum IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    p_erro = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = c_x.

    CLEAR : st_headerdataaction, st_headerdata2.
    REFRESH: t_return_vt.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = v_tknum
      IMPORTING
        output = st_headerdata2-shipment_num.

    st_headerdata2-status_load_end       = 'X'.
    st_headerdataaction-status_load_end  = 'C'.

    CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
      EXPORTING
        headerdata       = st_headerdata2
        headerdataaction = st_headerdataaction
      TABLES
        return           = t_return_vt.
*
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    IF wa_saida-inco1 = 'CIF'.
      CALL FUNCTION 'Z_LES_VERIFICA_PED_ADM'
        EXPORTING
          p_tknum      = st_headerdata2-shipment_num
        EXCEPTIONS
          adiantamento = 1
          pedagio      = 2
          OTHERS       = 3.
    ENDIF.

    IF sy-subrc IS NOT INITIAL.
      p_erro = abap_true.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      "Estorna VT
      REFRESH t_itemdata.
      CLEAR st_itemdata.
      st_itemdata-delivery  = wa_zsdt0001-doc_rem.
      st_itemdata-itenerary = '0010'.
      APPEND st_itemdata TO t_itemdata.
      MOVE-CORRESPONDING wa_zsdt0001 TO lc_saida.
      lc_saida-transp = st_headerdata2-shipment_num.
      PERFORM elimina_vt TABLES t_itemdata CHANGING lc_saida.
    ELSE.

      st_headerdata2-status_compl             = 'X'.
      st_headerdata2-status_shpmnt_start      = 'X'.
      st_headerdataaction-status_compl        = 'C'.
      st_headerdataaction-status_shpmnt_start = 'C'.

      CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
        EXPORTING
          headerdata       = st_headerdata2
          headerdataaction = st_headerdataaction
        TABLES
          return           = t_return_vt.
*
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      "atualiza valor do frete Historico
      UPDATE zsdt0001 SET  kbetr = wa_saida-kbetr  WHERE ch_referencia = wa_saida-ch_referencia.

      IF wa_saida-id_ordem IS NOT INITIAL.
        UPDATE zlest0155 SET ch_referencia = wa_saida-ch_referencia WHERE id_ordem = wa_saida-id_ordem.
      ENDIF.

      p_erro = 'N'.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_GERAR_VT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_exit INPUT.
  CASE ok-code.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN 'UP'.
      SET SCREEN 0.
    WHEN 'CANCEL'.
      SET SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT


*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_program   programa
*      -->P_dynpro    tela
*      -->P_start     define a tela
*      -->P_fnam      nome do campo ou comando
*      -->P_fval      conteúdo do campo ou comando
*----------------------------------------------------------------------*
FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  APPEND wa_bdcdata TO ti_bdcdata.

ENDFORM.                    " F_BDC_DATA

*&---------------------------------------------------------------------*
*&      Form  zf_call_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRANS    text
*      -->P_ERRO     text
*----------------------------------------------------------------------*
FORM zf_call_transaction USING p_trans CHANGING p_erro.
  CONSTANTS: c_msgid LIKE it_msg-msgid VALUE 'F5',
             c_msgnr LIKE it_msg-msgnr VALUE '312',
             c_msgne LIKE it_msg-msgnr VALUE '539'.

  REFRESH it_msg.

  wl_mode = 'E'.

  CALL TRANSACTION p_trans USING ti_bdcdata
        MODE wl_mode
        MESSAGES INTO it_msg.

  READ TABLE it_msg WITH KEY msgtyp = 'E'.
  IF sy-subrc = 0.
    p_erro = 'X'.
    REFRESH ti_zlest0100.
    DELETE it_msg WHERE msgtyp NE 'E'.
    CLEAR vl_ponteiro.
    SELECT  MAX( cont )
       FROM zlest0100
       INTO vl_ponteiro
       WHERE ch_referencia = wa_saida-ch_referencia.

    IF sy-subrc = 0.
      ADD 1 TO vl_ponteiro.
    ELSE.
      vl_ponteiro = 1.
    ENDIF.
    LOOP AT it_msg INTO wa_msg.

      MESSAGE ID     wa_msg-msgid
              TYPE   wa_msg-msgtyp
              NUMBER wa_msg-msgnr
              WITH   wa_msg-msgv1 wa_msg-msgv2 wa_msg-msgv3 wa_msg-msgv4
              INTO   vl_message.

      wa_zlest0100-mandt      = sy-mandt.
      wa_zlest0100-ch_referencia   = wa_saida-ch_referencia.
      wa_zlest0100-msgtyp     = wa_msg-msgtyp.
      wa_zlest0100-msgspra    = sy-langu.
      wa_zlest0100-msgid      = wa_msg-msgid.
      wa_zlest0100-msgnr      = wa_msg-msgnr.
      wa_zlest0100-msgv1      = vl_message.
      wa_zlest0100-data       = sy-datum.
      wa_zlest0100-hora       = sy-uzeit.
      wa_zlest0100-usuario    = sy-uname.
      wa_zlest0100-cont       = vl_ponteiro.

      APPEND wa_zlest0100 TO ti_zlest0100.
      ADD 1 TO vl_ponteiro.
    ENDLOOP.
    MODIFY zlest0100 FROM TABLE ti_zlest0100.
  ENDIF.


ENDFORM.                    "ZF_CALL_TRANSACTION

*&---------------------------------------------------------------------*
*&      Form  ELIMINA_VT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_ITEMDATA  text
*      -->P_V_TKNUM  text
*----------------------------------------------------------------------*
FORM elimina_vt  TABLES   tl_itemdata
                 CHANGING p_wa_saida TYPE ty_saida.
  DATA: st_headerdata2      TYPE bapishipmentheader,
        st_headerdataaction TYPE bapishipmentheaderaction,
        t_itemdataaction    TYPE TABLE OF bapishipmentitemaction WITH HEADER LINE,
        st_headerdata       TYPE bapishipmentheader.


  CLEAR : st_headerdataaction, st_headerdata, t_itemdataaction, st_headerdata2.
  REFRESH: t_itemdataaction,t_return_vt.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_wa_saida-transp+0(10)
    IMPORTING
      output = st_headerdata2-shipment_num.

  st_headerdataaction-shipment_num = 'D'.
  st_headerdataaction-service_agent_id = 'D'.

  LOOP AT tl_itemdata INTO st_itemdata.
    MOVE: 'D' TO t_itemdataaction-delivery,
          'D' TO t_itemdataaction-itenerary.

    APPEND t_itemdataaction.
    CLEAR: t_itemdataaction.

  ENDLOOP.
*
  REFRESH t_return_vt.
  CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
    EXPORTING
      headerdata       = st_headerdata2
      headerdataaction = st_headerdataaction
    TABLES
      itemdata         = tl_itemdata
      itemdataaction   = t_itemdataaction
      return           = t_return_vt.

  READ TABLE t_return_vt WITH KEY type = 'E'.
  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    "atualiza valor do frete Historico
    UPDATE zsdt0001 SET  kbetr = 0  WHERE ch_referencia = p_wa_saida-ch_referencia.

    p_wa_saida-st_proc         = '03'.
    p_wa_saida-transp          = icon_execute_object.
    UPDATE zsdt0001 SET st_proc      = '03'
                        kbetr        = 0
                        doc_transp   = ''

    WHERE ch_referencia = p_wa_saida-ch_referencia.

    IF p_wa_saida-ch_referencia IS NOT INITIAL.
      UPDATE zlest0155 SET ch_referencia = space WHERE ch_referencia = p_wa_saida-ch_referencia.
    ENDIF.


  ELSE.
    REFRESH ti_zlest0100.
    CLEAR vl_ponteiro.
    SELECT  MAX( cont )
       FROM zlest0100
       INTO vl_ponteiro
       WHERE ch_referencia = p_wa_saida-ch_referencia.

    IF sy-subrc = 0.
      ADD 1 TO vl_ponteiro.
    ELSE.
      vl_ponteiro = 1.
    ENDIF.
    LOOP AT t_return_vt.
      wa_zlest0100-mandt      = sy-mandt.
      wa_zlest0100-ch_referencia   = wa_saida-ch_referencia.
      wa_zlest0100-msgtyp     = 'E'.
      wa_zlest0100-msgspra    = sy-langu.
      wa_zlest0100-msgid      = 'LES'.
      wa_zlest0100-msgnr      = '000'.
      wa_zlest0100-msgv1      = t_return_vt-message.
      wa_zlest0100-data       = sy-datum.
      wa_zlest0100-hora       = sy-uzeit.
      wa_zlest0100-usuario    = sy-uname.
      wa_zlest0100-cont       = vl_ponteiro.

      APPEND wa_zlest0100 TO ti_zlest0100.
      ADD 1 TO vl_ponteiro.
    ENDLOOP.
    MODIFY zlest0100 FROM TABLE ti_zlest0100.

  ENDIF.
ENDFORM.                    " ELIMINA_VT
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNO_CUSTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA  text
*----------------------------------------------------------------------*
FORM f_estorno_custo  CHANGING    p_wa_saida TYPE ty_saida.
  DATA vdata(10).
  CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4) INTO vdata.
  IF '99_07_06_05' CS p_wa_saida-st_proc.
    "Estornar o documento de custo
    REFRESH ti_bdcdata.
    IF p_wa_saida-inco1 NE 'CPT'.
      "Estornar o documento de custo
      REFRESH ti_bdcdata.
      IF  p_wa_saida-shtyp = 'Z001'.
        PERFORM f_bdc_data USING:
            'SAPMV54A'  '0020'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
            ''          ''      ''   'BDC_OKCODE'       '=UEBP',
            ''          ''      ''   'VFKK-FKNUM'       p_wa_saida-doccus, "fknum

            'SAPMV54A'  '0030'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(02)',
            ''          ''      ''   'BDC_OKCODE'       '=PLOE',
            ''          ''      ''   'VIM_MARKED(02)'   'X',

            'SAPLSPO1'  '0100'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '=YES',

            'SAPMV54A'  '0030'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(01)',
            ''          ''      ''   'BDC_OKCODE'       '=PDET',

            'SAPMV54A'  '0040'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKP-POSTX',
            ''          ''      ''   'BDC_OKCODE'       '=PABR',

            'SAPMV54A'  '0040'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '=SICH',
            ''          ''      ''   'VFKPD-SLSTOR'     'X'.
      ELSE.
        PERFORM f_bdc_data USING:
                'SAPMV54A'  '0020'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
                ''          ''      ''   'BDC_OKCODE'       '=UEBP',
                ''          ''      ''   'VFKK-FKNUM'       p_wa_saida-doccus, "fknum

                'SAPMV54A'  '0030'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(01)',
                ''          ''      ''   'BDC_OKCODE'       '=PDET',

                'SAPMV54A'  '0040'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_CURSOR'       'VFKP-POSTX',
                ''          ''      ''   'BDC_OKCODE'       '=PABR',

                'SAPMV54A'  '0040'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'       '=SICH',
                ''          ''      ''   'VFKPD-SLSTOR'     'X'.
      ENDIF.

    ELSE.
      IF  p_wa_saida-shtyp = 'Z001'.
        PERFORM f_bdc_data USING:
            'SAPMV54A'  '0020'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
            ''          ''      ''   'BDC_OKCODE'       '=UEBP',
            ''          ''      ''   'VFKK-FKNUM'       p_wa_saida-doccus, "fknum

            'SAPMV54A'  '0030'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(02)',
            ''          ''      ''   'BDC_OKCODE'       '=PLOE',
            ''          ''      ''   'VIM_MARKED(02)'   'X',

            'SAPLSPO1'  '0100'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '=YES',

            'SAPMV54A'  '0030'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(01)',
            ''          ''      ''   'BDC_OKCODE'       '=PDET',

            'SAPMV54A'  '0040'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '=PABR',

            'SAPMV54A'  '0040'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '=KLAC',
            ''          ''      ''  'VFKPD-SLSTOR'      'X',

            'SAPMV54A'  '0040'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '/00',
            ''          ''      ''   'VFKPD-STDAT'      vdata,

            'SAPMV54A'  '0040'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '=SICH'.

      ELSE.
        PERFORM f_bdc_data USING:
                'SAPMV54A'  '0020'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
                ''          ''      ''   'BDC_OKCODE'       '=UEBP',
                ''          ''      ''   'VFKK-FKNUM'       p_wa_saida-doccus, "fknum

                'SAPMV54A'  '0030'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(01)',
                ''          ''      ''   'BDC_OKCODE'       '=PDET',

                'SAPMV54A'  '0040'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'       '=PABR',

                'SAPMV54A'  '0040'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'       '=KLAC',
                ''          ''      ''  'VFKPD-SLSTOR'      'X',

                'SAPMV54A'  '0040'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'       '/00',
                ''          ''      ''   'VFKPD-STDAT'      vdata,

                'SAPMV54A'  '0040'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'       '=SICH'.

      ENDIF.
    ENDIF.

    CLEAR wl_erro.
    PERFORM zf_call_transaction USING 'VI02' CHANGING wl_erro.
    IF wl_erro IS INITIAL.
      COMMIT WORK.
      WAIT UP TO 5 SECONDS.

    ELSE.
      EXIT.
    ENDIF.

    "Eliminar o documento de custo
    REFRESH ti_bdcdata.
    PERFORM f_bdc_data USING:
           'SAPMV54A'  '0020'  'X'  ''                 ' ',
           ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
           ''          ''      ''   'BDC_OKCODE'       '=UEBP',
           ''          ''      ''   'VFKK-FKNUM'       p_wa_saida-doccus, "fknum

           'SAPMV54A'  '0030'  'X'  ''                 ' ',
           ''          ''      ''   'BDC_OKCODE'       '/ELOES'.

    CLEAR wl_erro.
    PERFORM zf_call_transaction USING 'VI02' CHANGING wl_erro.
    IF wl_erro IS INITIAL.
      p_wa_saida-doccus          = icon_icon_list.
      UPDATE zsdt0001 SET st_proc      = '04'
                          fknum        = ''
      WHERE ch_referencia = p_wa_saida-ch_referencia.

      COMMIT WORK.

      WAIT UP TO 2 SECONDS.
    ELSE.
      EXIT.
    ENDIF.
  ENDIF.

  IF '99_07_06_05_04' CS p_wa_saida-st_proc.
**** elimina VT
    REFRESH t_itemdata.
    CLEAR st_itemdata.
    st_itemdata-delivery  = p_wa_saida-remessa(10).
    st_itemdata-itenerary = '0010'.
    APPEND st_itemdata TO t_itemdata.
    PERFORM elimina_vt TABLES t_itemdata
                       CHANGING p_wa_saida.
  ENDIF.


ENDFORM.                    " F_ESTORNO_CUSTO

*&---------------------------------------------------------------------*
*&      Form  F_ESTORNO_CTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_estorno_cte CHANGING pa_saida TYPE ty_saida.

  "Cancela fatura
  REFRESH: t_success, t_return_vt.
  IF '99_07' CS pa_saida-st_proc .
    SELECT SINGLE j_1bnfdoc~bukrs j_1bnflin~docnum
         FROM j_1bnflin
         INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
         INTO (vl_bukrs,vl_docnum)
         WHERE j_1bnflin~refkey = wa_saida-fatserv.

    SELECT SINGLE * FROM zcte_ciot INTO wa_zcte_ciot WHERE docnum EQ vl_docnum.
    IF ( sy-subrc EQ 0 ).
      IF ( ( wa_zcte_ciot-st_ciot NE 8 ) AND (  wa_zcte_ciot-st_ciot NE 0 ) OR ( wa_zcte_ciot-st_ciot NE 0 ) AND ( wa_zcte_ciot-st_ciot NE 8 ) ).
        MESSAGE e000(z01) WITH 'Necessário cancelar a viagem.'.
        EXIT.
      ENDIF.
    ENDIF.
    CALL FUNCTION 'ZBAPI_BILLINGDOC_CANCEL1' "Cancel Customer Individual Billing Document
      EXPORTING
        billingdocument = pa_saida-fatserv           " fatura_frete.
      TABLES
        return          = t_return         " bapireturn1   Table of Error Messages Entered
        success         = t_success.       " bapivbrksuccess  Table of Successfully Processed Documents
  ENDIF.

  IF t_success[] IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = c_x.

    pa_saida-fatserv         = icon_icon_list.
    UPDATE zsdt0001 SET st_proc      = '06'                   "06
                        fatura_frete = ''
    WHERE ch_referencia = pa_saida-ch_referencia.

    WAIT UP TO 5 SECONDS.
  ELSE.
    READ TABLE t_return_vt WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      REFRESH ti_zlest0100.
      CLEAR vl_ponteiro.
      SELECT  MAX( cont )
       FROM zlest0100
        INTO vl_ponteiro
        WHERE ch_referencia = wa_saida-ch_referencia.

      IF sy-subrc = 0.
        ADD 1 TO vl_ponteiro.
      ELSE.
        vl_ponteiro = 1.
      ENDIF.
      LOOP AT t_return_vt.
        wa_zlest0100-mandt      = sy-mandt.
        wa_zlest0100-ch_referencia   = wa_saida-ch_referencia.
        wa_zlest0100-msgtyp     = 'E'.
        wa_zlest0100-msgspra    = sy-langu.
        wa_zlest0100-msgid      = 'LES'.
        wa_zlest0100-msgnr      = '000'.
        wa_zlest0100-msgv1      = t_return_vt-message.
        wa_zlest0100-data       = sy-datum.
        wa_zlest0100-hora       = sy-uzeit.
        wa_zlest0100-usuario    = sy-uname.
        wa_zlest0100-cont       = vl_ponteiro.

        APPEND wa_zlest0100 TO ti_zlest0100.
        ADD 1 TO vl_ponteiro.
      ENDLOOP.
      MODIFY zlest0100 FROM TABLE ti_zlest0100.
      EXIT.
    ENDIF.
  ENDIF.

  REFRESH t_return_vt.
  IF '99_07_06' CS pa_saida-st_proc.
    "Bloqueia Ordem do frete
    CLEAR: wl_orderheaderin,wl_orderheaderinx.
    REFRESH:tl_bapiparex.
    wl_bape_vbak-vbeln   = pa_saida-ovserv.
    wl_bape_vbak-tknum   = ''.
    sl_bapiparex-structure     = 'BAPE_VBAK'.
    sl_bapiparex-valuepart1    = wl_bape_vbak.
    APPEND sl_bapiparex TO tl_bapiparex.
    CLEAR sl_bapiparex.
    wl_bape_vbakx-vbeln  = pa_saida-ovserv.
    wl_bape_vbakx-tknum  = 'X'.
    sl_bapiparex-structure     = 'BAPE_VBAKX'.
    sl_bapiparex-valuepart1 = wl_bape_vbakx.
    APPEND sl_bapiparex TO tl_bapiparex.

    wl_orderheaderin-bill_block  = '10'.
    wl_orderheaderinx-updateflag = 'U'.
    wl_orderheaderinx-bill_block = 'X'.
    CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        salesdocument    = pa_saida-ovserv " ov_frete
        order_header_in  = wl_orderheaderin
        order_header_inx = wl_orderheaderinx
      TABLES
        return           = t_return_vt
        extensionin      = tl_bapiparex.
    "
    READ TABLE t_return_vt WITH KEY type = 'E'.
    IF sy-subrc NE 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = c_x.

      pa_saida-ovserv          = icon_icon_list.
      UPDATE zsdt0001 SET st_proc      = '05'                     "05
                          ov_frete     = ''
      WHERE ch_referencia = pa_saida-ch_referencia.
    ELSE.
      READ TABLE t_return_vt WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        REFRESH ti_zlest0100.
        CLEAR vl_ponteiro.
        SELECT  MAX( cont )
           FROM zlest0100
           INTO vl_ponteiro
           WHERE ch_referencia = wa_saida-ch_referencia.

        IF sy-subrc = 0.
          ADD 1 TO vl_ponteiro.
        ELSE.
          vl_ponteiro = 1.
        ENDIF.

        LOOP AT t_return_vt.
          wa_zlest0100-mandt      = sy-mandt.
          wa_zlest0100-ch_referencia   = wa_saida-ch_referencia.
          wa_zlest0100-msgtyp     = 'E'.
          wa_zlest0100-msgspra    = sy-langu.
          wa_zlest0100-msgid      = 'LES'.
          wa_zlest0100-msgnr      = '000'.
          wa_zlest0100-msgv1      = t_return_vt-message.
          wa_zlest0100-data       = sy-datum.
          wa_zlest0100-hora       = sy-uzeit.
          wa_zlest0100-usuario    = sy-uname.
          wa_zlest0100-cont       = vl_ponteiro.

          APPEND wa_zlest0100 TO ti_zlest0100.
          ADD 1 TO vl_ponteiro.
        ENDLOOP.
        MODIFY zlest0100 FROM TABLE ti_zlest0100.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  IF '99_07_06_05_04' CS pa_saida-st_proc.
    PERFORM f_estorno_custo CHANGING pa_saida.
  ENDIF.

ENDFORM.                    " F_ESTORNO_CTE
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNO_NFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_estorno_nfe CHANGING pa_saida TYPE ty_saida.
  DATA: wa_zsdt0023  TYPE zsdt0023, "Fluxo de documento remessa formação de lote
        wa_lips      TYPE lips,
        is_cancelled TYPE bapivbrkout-cancelled.
  " BAPI cancel MIGO
  DATA: wa_mat_doc               TYPE bapi2017_gm_head_02-mat_doc,
        wa_doc_year              TYPE bapi2017_gm_head_02-doc_year,
        wa_pstng_date            TYPE bapi2017_gm_head_02-pstng_date,
        vg_invoicedocnumber_migo TYPE bapi2017_gm_head_ret,
        v_cd_uf                  TYPE zlest0002-cd_uf.

  " Delete remessa
  DATA: sl_hdata    TYPE bapiobdlvhdrchg,
        sl_hcont    TYPE bapiobdlvhdrctrlchg,
        tl_bapiret2 TYPE bapiret2_t.

  DATA:
    vl_delivery TYPE bapishpdelivnumb-deliv_numb,
    fp_vbeln    TYPE likp-vbeln,
    fp_budat    TYPE sy-datlo,
    fp_tcode    TYPE sy-tcode,
    fp_vbtyp    TYPE likp-vbtyp,
    it_mesg     TYPE STANDARD TABLE OF mesg.

  fp_budat = sy-datlo.
  fp_tcode = 'VL09'.
  fp_vbtyp = 'J'.

  REFRESH: t_success, t_return.
  CLEAR is_cancelled.

  IF pa_saida-operacao+0(4) NE 'ZRDC' AND
     pa_saida-operacao+0(4) NE 'ZRFL' AND
     pa_saida-operacao+0(3) NE 'ZUB'  AND
     pa_saida-operacao+0(4) NE 'ZRAN'.

    IF '03_02' CS pa_saida-st_proc .
      "//Cancela fatura
      CALL FUNCTION 'BAPI_BILLINGDOC_IS_CANCELLED' "#EC CI_USAGE_OK[2228098]
        EXPORTING
          billingdoc_number       = pa_saida-fatura
        IMPORTING
          billingdoc_is_cancelled = is_cancelled.

      IF ( is_cancelled IS INITIAL ).
        "Cancela fatura
        CALL FUNCTION 'ZBAPI_BILLINGDOC_CANCEL1'
          EXPORTING
            billingdocument = pa_saida-fatura
          TABLES
            return          = t_return         " bapireturn1 Table of Error Messages Entered
            success         = t_success.       " bapivbrksuccess Table of Successfully Processed Documents
      ENDIF.
    ENDIF.

    IF ( t_success[] IS NOT INITIAL ) OR ( is_cancelled IS NOT INITIAL ).
      CLEAR w_erro.
      PERFORM f_chk_estorno_fiscal  USING pa_saida CHANGING w_erro.

      IF w_erro IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = c_x.
        WAIT UP TO 5 SECONDS.
        pa_saida-fatura          = icon_execute_object.
        UPDATE zsdt0001 SET st_proc      = '01'
                            fatura_prod  = ''
        WHERE ch_referencia = pa_saida-ch_referencia.
      ELSE.
        EXIT.
      ENDIF.
    ELSEIF '03_02' CS pa_saida-st_proc.
      "gravar log erro
      READ TABLE t_return WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        REFRESH ti_zlest0100.
        CLEAR  vl_ponteiro.
        SELECT  MAX( cont )
          FROM zlest0100
          INTO vl_ponteiro
          WHERE ch_referencia = pa_saida-ch_referencia.

        IF sy-subrc = 0.
          ADD 1 TO vl_ponteiro.
        ELSE.
          vl_ponteiro = 1.
        ENDIF.

        LOOP AT t_return.
          wa_zlest0100-mandt      = sy-mandt.
          wa_zlest0100-ch_referencia   = wa_saida-ch_referencia.
          wa_zlest0100-msgtyp     = 'E'.
          wa_zlest0100-msgspra    = sy-langu.
          wa_zlest0100-msgid      = 'LES'.
          wa_zlest0100-msgnr      = '000'.
          wa_zlest0100-msgv1      = t_return-message.
          wa_zlest0100-data       = sy-datum.
          wa_zlest0100-hora       = sy-uzeit.
          wa_zlest0100-usuario    = sy-uname.
          wa_zlest0100-cont       = vl_ponteiro.

          APPEND wa_zlest0100 TO ti_zlest0100.
          ADD 1 TO vl_ponteiro.
        ENDLOOP.
        MODIFY zlest0100 FROM TABLE ti_zlest0100.
        EXIT.
      ENDIF.
    ENDIF.

    IF '03_02_01' CS pa_saida-st_proc .
      CALL FUNCTION 'WS_REVERSE_GOODS_ISSUE' "VL09  (Picking)
        EXPORTING
          i_vbeln                   = pa_saida-remessa(10)
          i_budat                   = fp_budat
          i_tcode                   = fp_tcode
          i_vbtyp                   = fp_vbtyp
        TABLES
          t_mesg                    = it_mesg
        EXCEPTIONS
          error_reverse_goods_issue = 1
          OTHERS                    = 2.

      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        WAIT UP TO 3 SECONDS.
*   Deleta Delivery Criado
        sl_hdata-deliv_numb = pa_saida-remessa(10).
        sl_hcont-deliv_numb = pa_saida-remessa(10).
        sl_hcont-dlv_del    = 'X'.

        CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE' " (VL02N)
          EXPORTING
            header_data    = sl_hdata
            header_control = sl_hcont
            delivery       = vl_delivery
          TABLES
            return         = tl_bapiret2.

        IF sy-subrc = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          pa_saida-remessa = icon_execute_object.
          pa_saida-lifnr   = ''.
          pa_saida-st_proc = ''.
          CLEAR v_cd_uf.
          SELECT SINGLE cd_uf
                  FROM zlest0002
                  INTO v_cd_uf
                  WHERE pc_veiculo = pa_saida-placa_cav.
          IF sy-subrc = 0.
            pa_saida-region          = v_cd_uf.
          ELSE.
            CLEAR pa_saida-region.
          ENDIF.
          "
          PERFORM f_estorno_res  CHANGING pa_saida.

          "Fim estorno residuo
          UPDATE zsdt0001 SET st_proc      = ''
                              region       = v_cd_uf
                              doc_rem      = ''
                              status       = ''
                              agente_frete = ''
                              kbetr        = 0
          WHERE ch_referencia = pa_saida-ch_referencia.

          IF pa_saida-ch_referencia IS NOT INITIAL.
            UPDATE zlest0155 SET ch_referencia = space WHERE ch_referencia = pa_saida-ch_referencia.
          ENDIF.

          COMMIT WORK.
        ELSE.
        ENDIF.

      ELSE.
      ENDIF.
    ENDIF.
  ELSEIF pa_saida-operacao+0(4) EQ 'ZRDC' OR
         pa_saida-operacao+0(4) EQ 'ZRFL' OR
         pa_saida-operacao+0(4) EQ 'ZRAN'.

    IF '03_02' CS pa_saida-st_proc .
      "//Cancela fatura
      CALL FUNCTION 'BAPI_BILLINGDOC_IS_CANCELLED' "#EC CI_USAGE_OK[2228098]
        EXPORTING
          billingdoc_number       = pa_saida-fatura
        IMPORTING
          billingdoc_is_cancelled = is_cancelled.

      IF ( is_cancelled IS INITIAL ).
        CALL FUNCTION 'ZBAPI_BILLINGDOC_CANCEL1'
          EXPORTING
            billingdocument = pa_saida-fatura
          TABLES
            return          = t_return         " bapireturn1 Table of Error Messages Entered
            success         = t_success.       " bapivbrksuccess Table of Successfully Processed Documents
      ENDIF.
    ENDIF.

    IF ( t_success[] IS NOT INITIAL ) OR ( is_cancelled IS NOT INITIAL ).
      CLEAR w_erro.
      PERFORM f_chk_estorno_fiscal  USING pa_saida CHANGING w_erro.

      IF w_erro IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = c_x.
        WAIT UP TO 5 SECONDS.
        pa_saida-fatura          = icon_execute_object.
        UPDATE zsdt0001 SET st_proc      = '01'
                            fatura_prod  = ''
        WHERE ch_referencia = pa_saida-ch_referencia.
      ELSE.
        EXIT.
      ENDIF.

      pa_saida-fatura  = icon_execute_object.
      pa_saida-danfe   = icon_execute_object.

      UPDATE zsdt0001 SET st_proc = '01' fatura_prod = ''
       WHERE ch_referencia = pa_saida-ch_referencia.
    ELSEIF '03_02' CS pa_saida-st_proc..
      "//Gravar log erro
      READ TABLE t_return WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        REFRESH ti_zlest0100.
        CLEAR vl_ponteiro.
        SELECT  MAX( cont )
           FROM zlest0100
           INTO vl_ponteiro
           WHERE ch_referencia = wa_saida-ch_referencia.

        IF sy-subrc = 0.
          ADD 1 TO vl_ponteiro.
        ELSE.
          vl_ponteiro = 1.
        ENDIF.

        LOOP AT t_return.
          wa_zlest0100-mandt         = sy-mandt.
          wa_zlest0100-ch_referencia = wa_saida-ch_referencia.
          wa_zlest0100-msgtyp     = 'E'.
          wa_zlest0100-msgspra    = sy-langu.
          wa_zlest0100-msgid      = 'LES'.
          wa_zlest0100-msgnr      = '000'.
          wa_zlest0100-msgv1      = t_return-message.
          wa_zlest0100-data       = sy-datum.
          wa_zlest0100-hora       = sy-uzeit.
          wa_zlest0100-usuario    = sy-uname.
          wa_zlest0100-cont       = vl_ponteiro.

          APPEND wa_zlest0100 TO ti_zlest0100.
          ADD 1 TO vl_ponteiro.
        ENDLOOP.
        MODIFY zlest0100 FROM TABLE ti_zlest0100.
        EXIT.
      ENDIF.
    ENDIF.


    IF '03_02_01' CS pa_saida-st_proc .
      CLEAR wa_zsdt0023.
      "MBST (estorno de migo)
      CLEAR wa_zsdt0023.
      SELECT SINGLE *
          FROM zsdt0023
          INTO wa_zsdt0023
          WHERE vbeln   = pa_saida-remessa(10).

      SELECT SINGLE *
        FROM lips
        INTO wa_lips
        WHERE vbeln = pa_saida-remessa(10).

      IF sy-subrc NE 0 AND wa_zsdt0023-es_mblnr_s IS NOT INITIAL. "se nao existir item exclui somente a remessa
        REFRESH ti_bdcdata.
        PERFORM f_bdc_data USING:
            'SAPMV50A'  '4004'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '/00',
            ''          ''      ''   'LIKP-VBELN'       pa_saida-remessa(10),

            'SAPMV50A'  '1000'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '/ELOES_T'.
        CLEAR wl_erro.
        PERFORM zf_call_transaction USING 'VL02N' CHANGING wl_erro.
        IF wl_erro IS INITIAL.
          COMMIT WORK.
          "elimina vinculo DCO-remessa
          IF pa_saida-operacao+0(4) EQ 'ZRDC'.
            SUBMIT zsdi0006 WITH p_vbeln = pa_saida-remessa(10)
                            WITH p_vinc  = ''
                            WITH p_desc  = 'X'
                            AND RETURN.
          ENDIF.

          CLEAR v_cd_uf.

          pa_saida-st_proc = ''.
          pa_saida-remessa = ''.

          SELECT SINGLE cd_uf
            FROM zlest0002
            INTO v_cd_uf
           WHERE pc_veiculo = pa_saida-placa_cav.

          IF sy-subrc = 0.
            pa_saida-region = v_cd_uf.
          ELSE.
            CLEAR pa_saida-region.
          ENDIF.

          PERFORM f_estorno_res  CHANGING pa_saida.

          UPDATE zsdt0001
             SET st_proc       = ''
                 region        = v_cd_uf
                 nro_nf_prod   = ''
                 fatura_prod   = ''
                 doc_rem       = ''
                 status        = ''
                 agente_frete  = ''
                 kbetr         = 0
           WHERE ch_referencia = pa_saida-ch_referencia.

          IF pa_saida-ch_referencia IS NOT INITIAL.
            UPDATE zlest0155 SET ch_referencia = space WHERE ch_referencia = pa_saida-ch_referencia.
          ENDIF.

          pa_saida-lifnr = ''.

          IF line_exists( t_fatura_agrupada[ werks = p_branch kunnr = wa_saida-kunnr inco1 = wa_saida-inco1 ] ).
            TRY.
                it_saida = it_saida[ dt_movimento = pa_saida-dt_movimento
                                     kunnr        = pa_saida-kunnr
                                     operacao(4)  = pa_saida-operacao(4)
                                     inco1        = pa_saida-inco1
                                     remessa      = icon_execute_object
                                   ].
              CATCH cx_sy_itab_line_not_found.
                pa_saida-remessa = icon_execute_object.
            ENDTRY.
          ELSE.
            pa_saida-remessa = icon_execute_object.
          ENDIF.
        ELSE.
          MESSAGE 'Erro ao estornar, remessa sem item! ' TYPE 'I'.
        ENDIF.
      ELSE.
        CALL FUNCTION 'WS_REVERSE_GOODS_ISSUE' "VL09  (Picking)
          EXPORTING
            i_vbeln                   = pa_saida-remessa(10)
            i_budat                   = fp_budat
            i_tcode                   = fp_tcode
            i_vbtyp                   = fp_vbtyp
          TABLES
            t_mesg                    = it_mesg
          EXCEPTIONS
            error_reverse_goods_issue = 1
            OTHERS                    = 2.

*      IF SY-SUBRC = 0 OR WA_ZSDT0023-ES_MBLNR_E IS INITIAL OR WA_ZSDT0023-ES_MBLNR_S IS INITIAL.
        IF wa_zsdt0023-es_mblnr_e IS INITIAL OR wa_zsdt0023-es_mblnr_s IS INITIAL.
          IF sy-subrc = 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            WAIT UP TO 3 SECONDS.
          ENDIF.

          "BAPI Estorno da MIGO - ENTRADA
          REFRESH t_return_vt.
          IF wa_zsdt0023-es_mblnr_e IS INITIAL AND wa_zsdt0023-mblnr_e IS NOT INITIAL. " Só faz se ainda esta em branco o doc material estorno entrada
            wa_mat_doc      = wa_zsdt0023-mblnr_e.
            wa_doc_year    	= wa_zsdt0023-mjahr_e.
            wa_pstng_date   = wa_zsdt0023-dt_saida.

            CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
              EXPORTING
                materialdocument    = wa_mat_doc
                matdocumentyear     = wa_doc_year
                goodsmvt_pstng_date = wa_pstng_date
              IMPORTING
                goodsmvt_headret    = vg_invoicedocnumber_migo
              TABLES
                return              = t_return_vt.

            IF t_return_vt[] IS INITIAL.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = c_x.

              UPDATE zsdt0023 SET es_mblnr_e = vg_invoicedocnumber_migo-mat_doc
                                  es_mjahr_e = vg_invoicedocnumber_migo-doc_year
              WHERE vbeln   = pa_saida-remessa(10).
            ELSE.
              "gravar log
              READ TABLE t_return_vt WITH KEY type = 'E'.
              IF sy-subrc EQ 0.
                REFRESH ti_zlest0100.
                CLEAR vl_ponteiro.
                SELECT  MAX( cont )
                 FROM zlest0100
                 INTO vl_ponteiro
                 WHERE ch_referencia = wa_saida-ch_referencia.

                IF sy-subrc = 0.
                  ADD 1 TO vl_ponteiro.
                ELSE.
                  vl_ponteiro = 1.
                ENDIF.
                LOOP AT t_return_vt.
                  wa_zlest0100-mandt      = sy-mandt.
                  wa_zlest0100-ch_referencia   = wa_saida-ch_referencia.
                  wa_zlest0100-msgtyp     = 'E'.
                  wa_zlest0100-msgspra    = sy-langu.
                  wa_zlest0100-msgid      = 'LES'.
                  wa_zlest0100-msgnr      = '000'.
                  wa_zlest0100-msgv1      = t_return_vt-message.
                  wa_zlest0100-data       = sy-datum.
                  wa_zlest0100-hora       = sy-uzeit.
                  wa_zlest0100-usuario    = sy-uname.
                  wa_zlest0100-cont       = vl_ponteiro.

                  APPEND wa_zlest0100 TO ti_zlest0100.
                  ADD 1 TO vl_ponteiro.
                ENDLOOP.
                MODIFY zlest0100 FROM TABLE ti_zlest0100.
                EXIT.
              ENDIF.
            ENDIF.
          ENDIF.

          "BAPI Estorno da MIGO - SAIDA
          REFRESH t_return_vt.

          "//Só faz se ainda esta em branco o doc material estorno saida
          IF wa_zsdt0023-es_mblnr_s IS INITIAL AND wa_zsdt0023-mblnr_s IS NOT INITIAL.
            wa_mat_doc      = wa_zsdt0023-mblnr_s.
            wa_doc_year    	= wa_zsdt0023-mjahr_s.
            wa_pstng_date   = wa_zsdt0023-dt_saida.

            CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
              EXPORTING
                materialdocument    = wa_mat_doc
                matdocumentyear     = wa_doc_year
                goodsmvt_pstng_date = wa_pstng_date
              IMPORTING
                goodsmvt_headret    = vg_invoicedocnumber_migo
              TABLES
                return              = t_return_vt.

            IF t_return_vt[] IS INITIAL.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = c_x.

              UPDATE zsdt0023 SET es_mblnr_s = vg_invoicedocnumber_migo-mat_doc
                                  es_mjahr_s = vg_invoicedocnumber_migo-doc_year
              WHERE vbeln = pa_saida-remessa(10).
            ELSE.
              READ TABLE t_return_vt WITH KEY type = 'E'.
              IF sy-subrc EQ 0.
                REFRESH ti_zlest0100.
                CLEAR vl_ponteiro.
                SELECT  MAX( cont )
                 FROM zlest0100
                 INTO vl_ponteiro
                 WHERE ch_referencia = wa_saida-ch_referencia.

                IF sy-subrc = 0.
                  ADD 1 TO vl_ponteiro.
                ELSE.
                  vl_ponteiro = 1.
                ENDIF.
                LOOP AT t_return_vt.
                  wa_zlest0100-mandt      = sy-mandt.
                  wa_zlest0100-ch_referencia   = wa_saida-ch_referencia.
                  wa_zlest0100-msgtyp     = 'E'.
                  wa_zlest0100-msgspra    = sy-langu.
                  wa_zlest0100-msgid      = 'LES'.
                  wa_zlest0100-msgnr      = '000'.
                  wa_zlest0100-msgv1      = t_return_vt-message.
                  wa_zlest0100-data       = sy-datum.
                  wa_zlest0100-hora       = sy-uzeit.
                  wa_zlest0100-usuario    = sy-uname.
                  wa_zlest0100-cont       = vl_ponteiro.

                  APPEND wa_zlest0100 TO ti_zlest0100.
                  ADD 1 TO vl_ponteiro.
                ENDLOOP.
                MODIFY zlest0100 FROM TABLE ti_zlest0100.
                EXIT.
              ENDIF.
            ENDIF.
          ENDIF.

*       //Deleta Delivery Criado
          sl_hdata-deliv_numb = pa_saida-remessa(10).
          sl_hcont-deliv_numb = pa_saida-remessa(10).
          sl_hcont-dlv_del    = 'X'.

          CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE' " (VL02N)
            EXPORTING
              header_data    = sl_hdata
              header_control = sl_hcont
              delivery       = pa_saida-remessa(10)
            TABLES
              return         = tl_bapiret2.

          IF sy-subrc = 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            "elimina vinculo DCO-remessa
            IF pa_saida-operacao+0(4) EQ 'ZRDC'.
              SUBMIT zsdi0006 WITH p_vbeln = pa_saida-remessa(10)
                              WITH p_vinc  = ''
                              WITH p_desc  = 'X'
                              AND RETURN.
            ENDIF.

            CLEAR v_cd_uf.

            pa_saida-st_proc = ''.
            pa_saida-remessa = ''.

            SELECT SINGLE cd_uf
              FROM zlest0002
              INTO v_cd_uf
             WHERE pc_veiculo = pa_saida-placa_cav.

            IF sy-subrc = 0.
              pa_saida-region = v_cd_uf.
            ELSE.
              CLEAR pa_saida-region.
            ENDIF.

            PERFORM f_estorno_res  CHANGING pa_saida.

            UPDATE zsdt0001
               SET st_proc       = ''
                   region        = v_cd_uf
                   nro_nf_prod   = ''
                   fatura_prod   = ''
                   doc_rem       = ''
                   status        = ''
                   agente_frete  = ''
                   kbetr         = 0
             WHERE ch_referencia = pa_saida-ch_referencia.

            IF pa_saida-ch_referencia IS NOT INITIAL.
              UPDATE zlest0155 SET ch_referencia = space WHERE ch_referencia = pa_saida-ch_referencia.
            ENDIF.

            pa_saida-lifnr = ''.

            IF line_exists( t_fatura_agrupada[ werks = p_branch kunnr = wa_saida-kunnr inco1 = wa_saida-inco1 ] ).
              TRY.
                  it_saida = it_saida[ dt_movimento = pa_saida-dt_movimento
                                       kunnr        = pa_saida-kunnr
                                       operacao(4)  = pa_saida-operacao(4)
                                       inco1        = pa_saida-inco1
                                       remessa      = icon_execute_object
                                     ].
                CATCH cx_sy_itab_line_not_found.
                  pa_saida-remessa = icon_execute_object.
              ENDTRY.
            ELSE.
              pa_saida-remessa = icon_execute_object.
            ENDIF.

          ENDIF.
        ELSE.
          MESSAGE 'Erro ao estornar! (Documento/Material) bloqueado por outro usuário' TYPE 'I'.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSEIF pa_saida-operacao+0(3) EQ 'ZUB'. "Pedido de transferencia
    IF '03_02' CS pa_saida-st_proc .
      CALL FUNCTION 'WS_REVERSE_GOODS_ISSUE' "VL09  (Picking)
        EXPORTING
          i_vbeln                   = pa_saida-remessa(10)
          i_budat                   = fp_budat
          i_tcode                   = fp_tcode
          i_vbtyp                   = fp_vbtyp
        TABLES
          t_mesg                    = it_mesg
        EXCEPTIONS
          error_reverse_goods_issue = 1
          OTHERS                    = 2.

      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        pa_saida-fatura = icon_execute_object.
        UPDATE zsdt0001 SET st_proc      = '01'
                            fatura_prod  = ''
        WHERE ch_referencia = pa_saida-ch_referencia.
      ELSE.
        MESSAGE 'Erro ao estornar! (Documento/Material) bloqueado por outro usuário' TYPE 'I'.
        EXIT.
      ENDIF.
    ENDIF.

    IF '03_02_01' CS pa_saida-st_proc.
*   Deleta Delivery Criado
      sl_hdata-deliv_numb = pa_saida-remessa(10).
      sl_hcont-deliv_numb = pa_saida-remessa(10).
      sl_hcont-dlv_del    = 'X'.

      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE' " (VL02N)
        EXPORTING
          header_data    = sl_hdata
          header_control = sl_hcont
          delivery       = pa_saida-remessa(10)
        TABLES
          return         = tl_bapiret2.

      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        pa_saida-lifnr   = ''.
        CLEAR v_cd_uf.
        SELECT SINGLE cd_uf
                FROM zlest0002
                INTO v_cd_uf
                WHERE pc_veiculo = pa_saida-placa_cav.
        IF sy-subrc = 0.
          pa_saida-region          = v_cd_uf.
        ELSE.
          CLEAR pa_saida-region.
        ENDIF.

        PERFORM f_estorno_res  CHANGING pa_saida.

        UPDATE zsdt0001 SET st_proc       = ''
                            region       = v_cd_uf
                            doc_rem       = ''
                            agente_frete  = ''
                            kbetr         = 0
                            status        = ''
       WHERE ch_referencia = pa_saida-ch_referencia.

        IF pa_saida-ch_referencia IS NOT INITIAL.
          UPDATE zlest0155 SET ch_referencia = space WHERE ch_referencia = pa_saida-ch_referencia.
        ENDIF.

        pa_saida-st_proc         = ''.
        pa_saida-remessa         = icon_execute_object.
        pa_saida-fatura          = icon_execute_object.
        pa_saida-danfe           = icon_execute_object.
      ENDIF.
    ENDIF.

  ENDIF.

  CLEAR is_cancelled.
ENDFORM.                    " F_ESTORNO_NFE

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

  REFRESH graphic_table.
  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.
  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.
ENDFORM.                    " F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout .
  REFRESH estrutura.
  PERFORM montar_estrutura USING:
     1  ''   ''            'TI_ZLEST0100' 'CONT'     'Seq'         ' ',
     1  ''   ''            'TI_ZLEST0100' 'MSGID'    'ID'          ' ',
     1  ''   ''            'TI_ZLEST0100' 'MSGV1'    'Menssagem'   '60',
     1  ''   ''            'TI_ZLEST0100' 'DATA'     'Data'        '10',
     1  ''   ''            'TI_ZLEST0100' 'HORA'     'Hora'        '10',
     1  ''   ''            'TI_ZLEST0100' 'USUARIO'  'Usuário'     '15'.

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
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
  IF p_outputlen IS INITIAL.
    wa_estrutura-outputlen     = x_contador.
  ELSE.
    wa_estrutura-outputlen     =  p_outputlen.
  ENDIF.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " montar_estrutura

*&---------------------------------------------------------------------*
*&      Form  MEMORIZAR_DT_MOVIMENTO_BADI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA_DT_MOVIMENTO  text
*----------------------------------------------------------------------*
FORM memorizar_dt_movimento_badi  USING p_data_rem TYPE ledat.

  TYPES:
    BEGIN OF tab_type,
      para TYPE string,
      dobj TYPE string,
    END OF tab_type.

  DATA: line TYPE tab_type,
        itab TYPE STANDARD TABLE OF tab_type,
        id   TYPE c LENGTH 10 VALUE 'ROMRETRO'.

  line-para = 'P1'.
  line-dobj = 'P_DATA_REM'.
  APPEND line TO itab.

  EXPORT (itab) TO MEMORY ID 'ROMRETRO'.

ENDFORM.                    " MEMORIZAR_DT_MOVIMENTO_BADI
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS '0200'.
  SET TITLEBAR  '0200'.
  IF wa_saida-transp NE icon_execute_object.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'A1'.
        screen-active    = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  wa_layout-zebra      = c_x.
  wa_layout-no_rowmark = c_x.
  wa_stable-row        = c_x.
  wa_layout-no_toolbar = c_x.

  wa_layout-grid_title = ' '.
  "GRID2
  IF obg_conteiner_veic IS INITIAL.
    CREATE OBJECT obg_conteiner_veic
      EXPORTING
        container_name = 'CC_VEIC'.


    CREATE OBJECT grid2
      EXPORTING
        i_parent = obg_conteiner_veic.


    PERFORM montar_layout_veic.

    REFRESH: tl_function.
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

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-grid_title = ''.
    wa_layout-no_toolbar = ''.


    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_fieldcatalog      = it_fieldcat[]
        it_outtab            = it_veic[].

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
  ELSE.
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE  sy-ucomm.
    WHEN 'SEARCH'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_mot-motorista
        IMPORTING
          output = wa_mot-motorista.

      SELECT SINGLE la~lifnr la~name1 la~stcd2 lb~zsabe la~stcd3 lb~eikto
        FROM lfa1 AS la
        INNER JOIN lfb1 AS lb ON lb~lifnr = la~lifnr AND lb~bukrs = p_bukrs
      INTO wa_mot
      WHERE la~lifnr = wa_mot-motorista.
      IF sy-subrc NE 0.
        CLEAR wa_mot.
      ENDIF.
    WHEN 'ALTM'.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question         = 'Confirma a alteração de Motorista'
          text_button_1         = 'Sim'(100)
          icon_button_1         = 'ICON_OKAY '
          text_button_2         = 'Não'(101)
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '1'
          display_cancel_button = ' '
          start_column          = 25
          start_row             = 6
        IMPORTING
          answer                = w_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      IF w_answer = '1'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_mot-motorista
          IMPORTING
            output = wa_mot-motorista.
        UPDATE zsdt0001 SET motorista = wa_mot-motorista
         WHERE ch_referencia = wa_saida-ch_referencia.
      ENDIF.

    WHEN 'SAIR'.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_VEIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_veic .
  REFRESH it_fieldcat.
  DATA i TYPE i.
  wa_afield-tabname     = 'IT_VEIC'.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_opt = 'X'.


  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'PLACA'.
  wa_afield-scrtext_l = 'Placa'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  wa_afield-outputlen = 12.
  APPEND wa_afield TO it_fieldcat.


  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TIPO'.
  wa_afield-scrtext_l = 'Tipo'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'COD_PROP'.
  wa_afield-scrtext_l = 'Proprietário'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NOM_PROP'.
  wa_afield-scrtext_l = 'Nome'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'RNTC'.
  wa_afield-scrtext_l = 'RNTC'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'RENAVAM'.
  wa_afield-scrtext_l = 'Renavam'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'CNPJ'.
  wa_afield-scrtext_l = 'CPF/CNPJ'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'CIDADE'.
  wa_afield-scrtext_l = 'Cidade'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'UF'.
  wa_afield-scrtext_l = 'UF'.
  wa_afield-scrtext_s = wa_afield-scrtext_l.
  wa_afield-scrtext_m = wa_afield-scrtext_l.
  APPEND wa_afield TO it_fieldcat.
ENDFORM.                    " MONTAR_LAYOUT_VEIC
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  IF g_custom_cont_desc IS INITIAL.

    CREATE OBJECT g_custom_cont_desc
      EXPORTING
        container_name = g_descbox.


    CREATE OBJECT obg_descbox
      EXPORTING
        parent            = g_custom_cont_desc
        wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position = 72
        max_number_chars  = 1000.

    CALL METHOD obg_descbox->set_toolbar_mode
      EXPORTING
        toolbar_mode = '0'.
    CALL METHOD obg_descbox->set_readonly_mode
      EXPORTING
        readonly_mode = 1.
  ELSE.
    CALL METHOD obg_descbox->set_text_as_r3table
      EXPORTING
        table = tg_editor.
    CALL METHOD obg_descbox->set_readonly_mode
      EXPORTING
        readonly_mode = 1.
  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS '0300'.
  SET TITLEBAR '0300'.

ENDMODULE.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE sy-ucomm .
    WHEN 'SEARCH'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_ag_frete-transpor2
        IMPORTING
          output = wa_ag_frete-transpor2.

      SELECT SINGLE *
          FROM lfa1
             INTO wa_agente
           WHERE lifnr = wa_ag_frete-transpor2.
      CLEAR txt_correc.
      IF sy-subrc = 0.
        wa_ag_frete-name2      = wa_agente-name1.
        wa_ag_frete-cnpj2      = wa_agente-stcd1.
        wa_ag_frete-inscr2     = wa_agente-stcd3.

        REFRESH: tg_editor.

        CONCATENATE 'Considerar dados corretos do transportador:CNPJ' wa_ag_frete-cnpj2
        'Razão Social' wa_ag_frete-name2
        'Inscr.Est.' wa_ag_frete-inscr2
        INTO txt_correc SEPARATED BY space.

        CLEAR: wl_cont_aux2, wl_cont_aux, wl_cont.
        wl_cont = strlen( txt_correc ).
        wl_cont_aux = wl_cont / 72.

        DO.
          MOVE: txt_correc+wl_cont_aux2(72) TO wg_editor-line.
          ADD 72 TO wl_cont_aux2.
          APPEND wg_editor TO tg_editor.

          IF wl_cont_aux2 GT wl_cont.
            EXIT.

          ENDIF.
        ENDDO.

        CALL METHOD obg_descbox->delete_text.

        CALL METHOD obg_descbox->set_text_as_r3table
          EXPORTING
            table = tg_editor.
        CALL METHOD obg_descbox->set_readonly_mode
          EXPORTING
            readonly_mode = 1.

      ENDIF.
    WHEN 'SAIR'.
      REFRESH: tg_editor.
      CLEAR wa_ag_frete.
      LEAVE TO SCREEN 0.
    WHEN 'BTNOK'.
      IF txt_correc IS NOT INITIAL.
        PERFORM lanc_carta_correcao.
        REFRESH: tg_editor.
        CLEAR wa_ag_frete.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*&      Form  LANC_CARTA_CORRECAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lanc_carta_correcao .
  DATA : vl_length          TYPE i,
         vl_id              TYPE zcarta_correcao-id_cc,
         ls_zcarta_correcao TYPE zcarta_correcao,
         pa_docnum          TYPE j_1bdocnum.

  CLEAR vl_id.
  SELECT SINGLE MAX( id_cc )
     FROM zcarta_correcao
    INTO     vl_id
    WHERE docnum = wa_saida-danfe
    AND   authcode      EQ ''
    AND   novo_agente   NE ''.

  IF vl_id NE 0.
    MESSAGE 'Já existe carta de correção enviada, aguarde retorno' TYPE 'I'.
    EXIT.
  ENDIF.

  CLEAR: vl_id.
  vl_length = strlen( txt_correc ).

  SELECT SINGLE MAX( id_cc )
    INTO vl_id
    FROM zcarta_correcao
   WHERE docnum = wa_saida-danfe.

  IF vl_id IS INITIAL .
    vl_id  = 0.
  ENDIF.

  vl_id = vl_id + 1.

  REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN txt_correc WITH 'a' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN txt_correc WITH 'e' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        'í'     IN txt_correc WITH 'i' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN txt_correc WITH 'o' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN txt_correc WITH 'u' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN txt_correc WITH 'c' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        '&'     IN txt_correc WITH '&#38;'.
  REPLACE ALL OCCURRENCES OF        ''''    IN txt_correc WITH '&#39;'.
  REPLACE ALL OCCURRENCES OF        'º'     IN txt_correc WITH 'o' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        'ª'     IN txt_correc WITH 'a' IGNORING CASE.

  ls_zcarta_correcao-docnum      = wa_saida-danfe.
  ls_zcarta_correcao-id_cc       = vl_id.
  ls_zcarta_correcao-msg_correc1 = txt_correc(250).
  ls_zcarta_correcao-msg_correc2 = txt_correc+250(250).
  ls_zcarta_correcao-msg_correc3 = txt_correc+500(250).
  ls_zcarta_correcao-msg_correc4 = txt_correc+750(250).
  ls_zcarta_correcao-usuario     = sy-uname.
  ls_zcarta_correcao-novo_agente = wa_ag_frete-transpor2.
  MODIFY zcarta_correcao FROM ls_zcarta_correcao.

  pa_docnum =  wa_saida-danfe.
  CALL FUNCTION 'Z_MONTA_XML_CTA_CORRECAO'
    EXPORTING
      p_docnum     = pa_docnum
      p_txt_correc = txt_correc.

  CLEAR txt_correc.

  CALL METHOD obg_descbox->delete_text.

ENDFORM.                    " LANC_CARTA_CORRECAO

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_CHK_ESTORNO_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_PA_SAIDA  text
*----------------------------------------------------------------------*
FORM f_chk_estorno_fiscal  USING pa_saida TYPE ty_saida CHANGING w_erro TYPE c.

  DATA vdocnum_est TYPE j_1bdocnum.
  "Checar se fiscal estornado ALRS
  WAIT UP TO 5 SECONDS.
  CLEAR: vl_docnum,vl_refkey, w_erro.
  IF pa_saida-tipo = 'P'.
    SELECT SINGLE vbeln mjahr
      INTO (vl_vbeln,vl_mjahr)
      FROM vbfa
      WHERE vbelv = pa_saida-remessa(10)
      AND vbtyp_n  = 'R'
      AND vbtyp_v  = 'J'.

    CONCATENATE vl_vbeln vl_mjahr INTO vl_refkey.
    SELECT SINGLE docnum
      FROM j_1bnflin
      INTO vl_docnum
      WHERE refkey = vl_refkey.
  ELSE.
    SELECT SINGLE docnum
      FROM j_1bnflin
      INTO vl_docnum
      WHERE refkey = pa_saida-fatura.
  ENDIF.
*

  CLEAR vcandat.
  SELECT SINGLE  candat
    FROM j_1bnfdoc
    INTO  vcandat
   WHERE docnum     = vl_docnum.

  IF vcandat IS INITIAL. "Documento Fiscal não está estornado ainda....
*
    "Verificar se documento esta autorizado na SEFAZ
    SELECT SINGLE docnum
     FROM j_1bnfe_active
     INTO v_docnum
     WHERE docnum     = vl_docnum
       AND docsta     = '1'
       AND cancel     = ''.

    IF sy-subrc NE 0. "Caso não esteja, forçar o cancelamento do documento fiscal, serviço que a bapi deveria ter feito e não fez.
      CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
        EXPORTING
          doc_number               = vl_docnum
          ref_type                 = space
          ref_key                  = space
          can_dat                  = sy-datum
        IMPORTING
          doc_number               = vdocnum_est
        EXCEPTIONS
          document_not_found       = 1
          cancel_not_possible      = 2
          nf_cancel_type_not_found = 3
          database_problem         = 4
          docum_lock               = 5
          nfe_cancel_simulation    = 6
          OTHERS                   = 7.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ELSE.
        w_erro = 'X'.
      ENDIF.
    ELSE.
      w_erro = 'X'.
    ENDIF.

    CHECK w_erro IS NOT INITIAL. "Não houve êxito na tentativa do cancelamento do Doc. Fiscal, e prosseguir para gravar o log. de erro.

*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

*    MESSAGE 'Impossível estorno de fatura. Danfe, não estornada!' TYPE  'I'.
    REFRESH ti_zlest0100.
    CLEAR wa_zlest0100.
    CLEAR vl_ponteiro.
    SELECT  MAX( cont )
       FROM zlest0100
       INTO vl_ponteiro
       WHERE ch_referencia = pa_saida-ch_referencia.

    IF sy-subrc = 0.
      ADD 1 TO vl_ponteiro.
    ELSE.
      vl_ponteiro = 1.
    ENDIF.

    wa_zlest0100-mandt         = sy-mandt.
    wa_zlest0100-ch_referencia = pa_saida-ch_referencia.
    wa_zlest0100-msgtyp     = 'E'.
    wa_zlest0100-msgspra    = sy-langu.
    wa_zlest0100-msgid      = 'LES'.
    wa_zlest0100-msgnr      = '000'.
    wa_zlest0100-msgv1      = 'Danfe, não estornada, automaticamente ==>'.
    CONCATENATE wa_zlest0100-msgv1 vdocnum_est INTO wa_zlest0100-msgv1.
    wa_zlest0100-data       = sy-datum.
    wa_zlest0100-hora       = sy-uzeit.
    wa_zlest0100-usuario    = sy-uname.
    wa_zlest0100-cont       = vl_ponteiro.

    APPEND wa_zlest0100 TO ti_zlest0100.
    ADD 1 TO vl_ponteiro.

    MODIFY zlest0100 FROM TABLE ti_zlest0100.
*    W_ERRO = 'X'. "Não retorna erro neste caso
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNO_RES
*&---------------------------------------------------------------------*
*       Estorno de entrada de residuo
*----------------------------------------------------------------------*
*      <--P_PA_SAIDA  text
*----------------------------------------------------------------------*
FORM f_estorno_res  CHANGING pa_saida TYPE ty_saida.
  DATA: w_romaneio               TYPE zsdt0001,
        wa_mat_doc               TYPE bapi2017_gm_head_02-mat_doc,
        wa_doc_year              TYPE bapi2017_gm_head_02-doc_year,
        wa_pstng_date            TYPE bapi2017_gm_head_02-pstng_date,
        vg_invoicedocnumber_migo TYPE bapi2017_gm_head_ret,
        v_budat                  TYPE mkpf-budat,
        w_mseg                   TYPE mseg.

  DATA: wa_goodsmvt_header TYPE bapi2017_gm_head_01,
        t_goodsmvt_item    TYPE TABLE OF bapi2017_gm_item_create,
        wa_goodsmvt_item   TYPE bapi2017_gm_item_create,
        wa_code            TYPE bapi2017_gm_code,
        vl_mat_doc         TYPE bapi2017_gm_head_ret-mat_doc,
        vl_matdocumentyear TYPE bapi2017_gm_head_ret-doc_year.

  "Estorna entrada de Residuo
  SELECT SINGLE *
    FROM zsdt0001
    INTO w_romaneio
    WHERE ch_referencia = pa_saida-ch_referencia.
  IF w_romaneio-doc_material_e IS NOT INITIAL. "doc. material entrada residuo existe
    REFRESH t_return_vt.
    SELECT SINGLE budat INTO v_budat
      FROM mkpf
      WHERE mblnr = w_romaneio-doc_material_e
      AND   mjahr = w_romaneio-ano_material_e.
    "
    wa_mat_doc      = w_romaneio-doc_material_e.
    wa_doc_year    	= w_romaneio-ano_material_e.
    wa_pstng_date   = v_budat.

    SELECT SINGLE *
      INTO w_mseg
      FROM mseg
      WHERE mblnr = w_romaneio-doc_material_e
      AND   mjahr = w_romaneio-ano_material_e
      AND   bwart = 'ZX1'. "inverte

    IF sy-subrc = 0.
      CLEAR: t_goodsmvt_item.
      wa_goodsmvt_header-pstng_date = v_budat.
      wa_goodsmvt_header-doc_date   = v_budat.
      wa_goodsmvt_header-header_txt = w_romaneio-vbeln.

      wa_code-gm_code               = '05'.

* ---> S4 Migração - 19/06/2023 - FC - Inicio
      "WA_GOODSMVT_ITEM-MATERIAL     = W_MSEG-MATNR.

      DATA(v_len) = strlen( w_mseg-matnr ).

      IF v_len > 18.
        wa_goodsmvt_item-material_long = w_mseg-matnr.
      ELSE.
        wa_goodsmvt_item-material = w_mseg-matnr.
      ENDIF.
* <--- S4 Migração - 19/06/2023 - FC - Fim

      wa_goodsmvt_item-plant        = w_mseg-werks.
      wa_goodsmvt_item-stge_loc	    = w_mseg-lgort.
      wa_goodsmvt_item-batch        = w_mseg-charg.

      wa_goodsmvt_item-move_type    = 'ZX2'. "ALRS 24/05/2017
      wa_goodsmvt_item-entry_qnt    = w_mseg-menge.
      APPEND wa_goodsmvt_item TO t_goodsmvt_item.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          goodsmvt_header  = wa_goodsmvt_header
          goodsmvt_code    = wa_code
        IMPORTING
          materialdocument = vl_mat_doc
          matdocumentyear  = vl_matdocumentyear
        TABLES
          goodsmvt_item    = t_goodsmvt_item
          return           = t_return_vt.
    ELSE.
      CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
        EXPORTING
          materialdocument    = wa_mat_doc
          matdocumentyear     = wa_doc_year
          goodsmvt_pstng_date = wa_pstng_date
        IMPORTING
          goodsmvt_headret    = vg_invoicedocnumber_migo
        TABLES
          return              = t_return_vt.
    ENDIF.

    IF t_return_vt[] IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = c_x.
      UPDATE zsdt0001
           SET doc_material_e      = ''
               ano_material_e      = ''
         WHERE ch_referencia = pa_saida-ch_referencia.
    ELSE.
      "gravar log
      READ TABLE t_return_vt WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        REFRESH ti_zlest0100.
        CLEAR vl_ponteiro.
        SELECT  MAX( cont )
         FROM zlest0100
         INTO vl_ponteiro
         WHERE ch_referencia = wa_saida-ch_referencia.

        IF sy-subrc = 0.
          ADD 1 TO vl_ponteiro.
        ELSE.
          vl_ponteiro = 1.
        ENDIF.
        LOOP AT t_return_vt.
          wa_zlest0100-mandt      = sy-mandt.
          wa_zlest0100-ch_referencia   = pa_saida-ch_referencia.
          wa_zlest0100-msgtyp     = 'E'.
          wa_zlest0100-msgspra    = sy-langu.
          wa_zlest0100-msgid      = 'LES'.
          wa_zlest0100-msgnr      = '000'.
          wa_zlest0100-msgv1      = t_return_vt-message.
          wa_zlest0100-data       = sy-datum.
          wa_zlest0100-hora       = sy-uzeit.
          wa_zlest0100-usuario    = sy-uname.
          wa_zlest0100-cont       = vl_ponteiro.

          APPEND wa_zlest0100 TO ti_zlest0100.
          ADD 1 TO vl_ponteiro.
        ENDLOOP.
        MODIFY zlest0100 FROM TABLE ti_zlest0100.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.

FORM f_check_mov_sobra USING p_ch_referencia TYPE zsdt0001-ch_referencia
                    CHANGING p_error_mov.

  DATA: v_nr_rom_ent TYPE zsdt0001-nr_romaneio,
        wl_0001_ent  TYPE zsdt0001,
        v_seq_log    TYPE zsdt_sobra_rom-seq,
        wl_log_mov   TYPE zsdt_sobra_rom.

  CLEAR: p_error_mov, v_nr_rom_ent, wl_0001_ent, wl_log_mov, v_seq_log.

  SELECT SINGLE *
    FROM zsdt0001 INTO @DATA(_wl_0001)
   WHERE ch_referencia = @p_ch_referencia.

  IF ( sy-subrc NE 0 ) OR ( p_ch_referencia IS INITIAL ).
    p_error_mov = 'X'.
    MESSAGE 'Romaneio para verificação de entrada Residuo/Sobra, não foi encontrado!' TYPE 'I'.
    RETURN.
  ENDIF.

  CHECK _wl_0001-branch+0(2) NE '15'. "Agro não faz entrrada de residuo automatica

  SELECT MAX( seq )
    FROM zsdt_sobra_rom INTO v_seq_log
   WHERE ch_referencia = p_ch_referencia.

  ADD 1 TO v_seq_log.
  wl_log_mov-ch_referencia = p_ch_referencia.
  wl_log_mov-dt_reg        = sy-datum.
  wl_log_mov-hr_reg        = sy-uzeit.

  IF ( _wl_0001-matnr IS INITIAL ).
    ADD 1 TO v_seq_log.
    wl_log_mov-seq     = v_seq_log.
    wl_log_mov-msg     = 'Material em branco'.
    MODIFY zsdt_sobra_rom FROM wl_log_mov.
    RETURN.
  ENDIF.

  IF ( _wl_0001-branch IS INITIAL ).
    ADD 1 TO v_seq_log.
    wl_log_mov-seq     = v_seq_log.
    wl_log_mov-msg     = 'Filial em branco'.
    MODIFY zsdt_sobra_rom FROM wl_log_mov.
    RETURN.
  ENDIF.

  IF ( _wl_0001-doc_material_e IS NOT INITIAL ). " gerou Doc. Material.
    ADD 1 TO v_seq_log.
    wl_log_mov-seq     = v_seq_log.
    wl_log_mov-msg     = |Doc. Material foi gerado. Nro: { _wl_0001-doc_material_e } |.
    MODIFY zsdt_sobra_rom FROM wl_log_mov.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM setleaf INTO @DATA(_wl_setleaf)
   WHERE setname EQ 'RESIDUO'
     AND valfrom EQ @_wl_0001-matnr.

  IF ( sy-subrc NE 0 ).
    ADD 1 TO v_seq_log.
    wl_log_mov-seq     = v_seq_log.
    wl_log_mov-msg     = 'Material não parametrizado no set'.
    MODIFY zsdt_sobra_rom FROM wl_log_mov.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM zmmt0074 INTO @DATA(_wl_0074)
   WHERE werks = @_wl_0001-branch
     AND matnr = @_wl_0001-matnr.

  IF sy-subrc NE 0.
    ADD 1 TO v_seq_log.
    wl_log_mov-seq     = v_seq_log.
    wl_log_mov-msg     = 'Material  obrigatório parâmetro Entrada Residuo/Sobra, procure Depto Estoque!'.
    MODIFY zsdt_sobra_rom FROM wl_log_mov.

    p_error_mov = 'X'.
    MESSAGE 'Material  obrigatório parâmetro Entrada Residuo/Sobra, procure Depto Estoque!' TYPE 'I'.
    RETURN.
  ENDIF.

  DATA(_gerar_doc_ent) = 'N'.

  IF _wl_0074-entrada_rom = 'S'. "Checa romaneio de entrada

    IF _wl_0001-id_referencia IS INITIAL.
      ADD 1 TO v_seq_log.
      wl_log_mov-seq     = v_seq_log.
      wl_log_mov-msg     = 'Id Referencia em branco'.
      MODIFY zsdt_sobra_rom FROM wl_log_mov.
      RETURN.
    ENDIF.

    v_nr_rom_ent = _wl_0001-id_referencia.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = v_nr_rom_ent
      IMPORTING
        output = v_nr_rom_ent.

    SELECT SINGLE *
      FROM zsdt0001 INTO wl_0001_ent
     WHERE bukrs         = _wl_0001-bukrs
       AND branch        = _wl_0001-branch
       AND tp_movimento  = 'E'
       AND nr_romaneio   = v_nr_rom_ent
       AND nr_safra      = _wl_0001-nr_safra.

    IF wl_0001_ent-ch_refer_ent IS NOT INITIAL.

      DATA(_peso_orig) = wl_0001_ent-peso_liq. "CS2017001903

      CLEAR wl_0001_ent-peso_liq.
      SELECT SUM( peso_liq )
         FROM zsdt0001 INTO wl_0001_ent-peso_liq
          WHERE bukrs         = wl_0001_ent-bukrs
          AND   branch        = wl_0001_ent-branch
          AND   tp_movimento  = 'E'
          AND   ch_refer_ent  = wl_0001_ent-ch_refer_ent
          AND   nr_safra      = wl_0001_ent-nr_safra.

      IF _peso_orig = wl_0001_ent-peso_liq.

        ADD 1 TO v_seq_log.
        wl_log_mov-seq     = v_seq_log.
        wl_log_mov-msg     = 'Não foram encontrados todos os romaneios de desmembramento!'.
        MODIFY zsdt_sobra_rom FROM wl_log_mov.

        p_error_mov = 'X'.
        MESSAGE 'Não foram encontrados todos os romaneios de desmembramento!' TYPE 'I'.
        RETURN.
      ENDIF.

    ENDIF.

    IF sy-subrc =  0.

      ADD 1 TO v_seq_log.
      wl_log_mov-seq     = v_seq_log.
      wl_log_mov-msg     = | Peso Liq. S: { _wl_0001-peso_liq } - Peso Liq. E: { wl_0001_ent-peso_liq } |.
      MODIFY zsdt_sobra_rom FROM wl_log_mov.

      IF  _wl_0001-peso_liq GT wl_0001_ent-peso_liq.
        _gerar_doc_ent = 'S'.
      ELSE.
        RETURN. "Não gera  doc. entrada
      ENDIF.

    ELSE.
      ADD 1 TO v_seq_log.
      wl_log_mov-seq     = v_seq_log.
      wl_log_mov-msg     = 'Não foi encontrado o romaneio de entrada nesta data!'.
      MODIFY zsdt_sobra_rom FROM wl_log_mov.

      p_error_mov = 'X'.
      MESSAGE 'Não foi encontrado o romaneio de entrada nesta data!' TYPE 'I'.
      RETURN.
    ENDIF.

  ELSE.
    _gerar_doc_ent = 'S'.
  ENDIF.

  IF ( _gerar_doc_ent = 'S' ).
    ADD 1 TO v_seq_log.
    wl_log_mov-seq     = v_seq_log.
    wl_log_mov-msg     = 'Entrada Residuo/Sobra, não gerada! Estornar a Remessa'.
    MODIFY zsdt_sobra_rom FROM wl_log_mov.

    p_error_mov = 'X'.
    MESSAGE 'Entrada Residuo/Sobra, não gerada! Estornar a Remessa' TYPE 'I'.
    RETURN.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_HINTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_hints .
  DATA: it_dd07v TYPE TABLE OF dd07v WITH HEADER LINE.

  CLEAR: it_except_qinfo.


  wa_except_qinfo-type  = cl_salv_tooltip=>c_type_symbol.
  wa_except_qinfo-value = icon_execute_object.
  wa_except_qinfo-text  = 'Gerar a Remessa'.
  wa_except_qinfo-tabname   = 'IT_SAIDA'.
  wa_except_qinfo-fieldname = 'REMESSA'.
  APPEND wa_except_qinfo TO it_except_qinfo.

  wa_except_qinfo-type  = cl_salv_tooltip=>c_type_symbol.
  wa_except_qinfo-value =  icon_workflow_inbox.
  wa_except_qinfo-text  = 'Aguardando Aprovação de limite de crédito'.
  wa_except_qinfo-tabname   = 'IT_SAIDA'.
  wa_except_qinfo-fieldname = 'REMESSA'.
  APPEND wa_except_qinfo TO it_except_qinfo.

  wa_except_qinfo-type  = cl_salv_tooltip=>c_type_symbol.
  wa_except_qinfo-value =  icon_reject.
  wa_except_qinfo-text  = 'Limite de crédito rejeitado'.
  wa_except_qinfo-tabname   = 'IT_SAIDA'.
  wa_except_qinfo-fieldname = 'REMESSA'.
  APPEND wa_except_qinfo TO it_except_qinfo.
ENDFORM.

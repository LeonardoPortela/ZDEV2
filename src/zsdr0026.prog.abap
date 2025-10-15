*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 15/03/2013                                              &*
*& Descrição: Cadastro de Cód. de cadastro de itens de formação de Preço&*
*& Transação: zsdt0064                                                &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                         03.08.2010                            &*
*&--------------------------------------------------------------------&*

REPORT  zsdr0026.

TABLES: zsdt0053.

DEFINE f_preencher_dynpro.
  CLEAR: tl_bdc.
  MOVE &1 TO tl_bdc-dynbegin.
  IF &1 = 'X'.
    MOVE:
     &2  TO tl_bdc-program,
     &3  TO tl_bdc-dynpro.
  ELSE.
    MOVE:
      &2 TO tl_bdc-fnam,
      &3 TO tl_bdc-fval.
  ENDIF.
  APPEND tl_bdc.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: slis, kkblo.
INCLUDE <icon>.

*DATA: IT_MSG TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE,
*      WA_MSG TYPE BDCMSGCOLL,
*
*      IT_DTA TYPE STANDARD TABLE OF BDCDATA,
*      WA_DTA LIKE LINE OF IT_DTA,
*
*      BEGIN OF IT_MSGTEXT OCCURS 0,
*        TEXTO TYPE T100-TEXT,
*      END OF IT_MSGTEXT.

TYPES: BEGIN OF ty_saida,
         mark,
         status(4),
         status_trace      TYPE char4,      "*-CS2023000189-26.04.2023-#108710-JT
         vkorg             TYPE zsdt0051-vkorg,
         vtweg             TYPE zsdt0051-vtweg,
         spart             TYPE zsdt0051-spart,
         vkbur             TYPE zsdt0051-vkbur,
         vkgrp             TYPE zsdt0051-vkgrp,
         nro_sol_ov        TYPE zsdt0051-nro_sol_ov,
         posnr             TYPE zsdt0053-posnr,
         tp_venda          TYPE zsdt0051-tp_venda,
         auart             TYPE zsdt0051-auart,
         contrato(4),  "TYPE ZSDT0053-CONTRATO,
         vbeln             TYPE zsdt0053-vbeln,
         kunnr             TYPE zsdt0051-kunnr,
         adiant(4),
         name1             TYPE kna1-name1,
         matnr             TYPE zsdt0053-matnr,
         maktx             TYPE makt-maktx,
         werks             TYPE zsdt0053-werks,
         w_name1           TYPE t001w-name1,
         zmeng             TYPE zsdt0053-zmeng,
         qtd_remessa       TYPE zsdt0053-zmeng,
         saldo_ov          TYPE zsdt0053-zmeng,
         zieme             TYPE zsdt0053-zieme,
         dmbtr             TYPE zsdt0053-dmbtr,
         vlrtot            TYPE zsdt0053-vlrtot,
         waerk             TYPE zsdt0051-waerk,
         observacao        TYPE zsdt0051-observacao,
         correto           TYPE zsdt0051-correto,
         name1_c           TYPE lfa1-name1,
         bstkd             TYPE zsdt0051-bstkd,
         dco               TYPE zsdt0066-dco,
         aviso             TYPE zsdt0066-aviso,
         inco1             TYPE zsdt0051-inco1,
         docnum_rt         TYPE zsdt0053-docnum_rt,
         nfenum_rt         TYPE j_1bnfdoc-nfenum,
         remessa_exp       TYPE zsdt0053-remessa_exp,
         id_export         TYPE zsdt0053-id_export,
         doc_fat           TYPE vbrk-vbeln,
         nfenum_exp        TYPE j_1bnfdoc-nfenum,
         charg             TYPE zsdt0053-charg,
         terminal          TYPE zsdt0053-terminal,
         navio             TYPE zsdt0053-navio,
         id_due            TYPE zsdt0170-id_due,
         numero_due        TYPE zsdt0170-numero_due,
         id_registro_expo  TYPE zreg_exportacao-id_registro_expo,
         nr_registro_expo  TYPE zreg_exportacao-nr_registro_expo,
         ort01             TYPE kna1-ort01,
         regio             TYPE kna1-regio,
         stcd1             TYPE kna1-stcd1,
         instrucao         TYPE zsdt0053-instrucao,
         anexos            TYPE icon_d,
         numero_ruc        TYPE zsdt0053-numero_ruc,
         id_nomeacao_tran  TYPE zsdt0053-id_nomeacao_tran,
         ds_nome_transpor  TYPE znom_transporte-ds_nome_transpor,
         id_local_descarga TYPE zde_zsdt0001cg_alv-id_local_descarga,
         ds_local_descarga TYPE zde_zsdt0001cg_alv-ds_local_descarga,
         ponto_coleta      TYPE zsdt0066-ponto_c,
         name_pc           TYPE lfa1-name1,
         terminal_z1       TYPE zsdt0066-terminal,
         name_terminal     TYPE lfa1-name1,
         data_venda        TYPE zsdt0051-data_venda,
         safra             TYPE zsdt0066-charg, "102915 CS2023000054 INCLUIR COLUNA "SAFRA" NA TRANSAÇÃO ZSDT0066 - SCABANA
         erdat             TYPE vbak-erdat, "US 152645 // MMSILVA - 11-10-2024

       END OF ty_saida,

       BEGIN OF ty_saldo,
         nro_sol_ov TYPE zsdt0053-nro_sol_ov,
         vbeln      TYPE vbfa-vbeln,
         werks      TYPE zsdt0053-werks,
         zmeng      TYPE zsdt0053-zmeng,
         total      TYPE zsdt0053-zmeng,
         saldo      TYPE zsdt0053-zmeng,
       END OF ty_saldo,

*** Stefanini - IR252396 - 12/08/2025 - LAZAROSR - Início de Alteração
       BEGIN OF ty_saldo_pedido,
         ebeln TYPE ekko-ebeln,
         menge TYPE ekbe-menge,
         total TYPE zsdt0053-zmeng,
         saldo TYPE zsdt0053-zmeng,
       END OF ty_saldo_pedido,
*** Stefanini - IR252396 - 12/08/2025 - LAZAROSR - Fim de Alteração

       BEGIN OF ty_bsad,
         bukrs TYPE bsad-bukrs,
         belnr TYPE bsad-belnr,
         audgt TYPE bsad-augdt,
         augbl TYPE bsad-augbl,
       END OF ty_bsad,

       BEGIN OF ty_objek,
         objek TYPE zsdt0045-objek,
       END OF ty_objek,

       BEGIN OF ty_j_1bnfdoc,
         nfenum TYPE j_1bnfdoc-nfenum,
         docnum TYPE j_1bnfdoc-docnum,
       END OF ty_j_1bnfdoc,

       BEGIN OF ty_j_1bnflin,
         docnum TYPE j_1bnflin-docnum,
         refkey TYPE j_1bnflin-refkey,
       END OF ty_j_1bnflin,

       BEGIN OF ty_vbfa_rt,
         vbelv  TYPE vbfa-vbelv,
         vbeln  TYPE vbfa-vbeln,
         refkey TYPE j_1bnflin-refkey,
       END OF ty_vbfa_rt,

       BEGIN OF ty_check,
         bukrs TYPE n LENGTH 4,
         block TYPE c LENGTH 1,
         livre TYPE c LENGTH 1,
       END OF ty_check,

       BEGIN OF ty_vinc_doc_exp,
         vbeln            TYPE likp-vbeln,
         bukrs            TYPE t001-bukrs,
         butxt            TYPE t001-butxt,
         werks            TYPE t001w-werks,
         gsber            TYPE vbap-gsber,
         name1            TYPE t001w-name1,
         lgort            TYPE t001l-lgort,
         lgobe            TYPE t001l-lgobe,
         numero_due       TYPE zsdt0170-numero_due,
         nr_registro_expo TYPE zreg_exportacao-nr_registro_expo,
         dde              TYPE zdde-nr_dde,
         id_dde           TYPE zdde-id_dde,
         id_nomeacao_tran TYPE zreg_exportacao-id_nomeacao_tran,
         desc             TYPE znom_transporte-ds_nome_transpor,
         id_registro_expo TYPE zreg_exportacao-id_registro_expo,
         id_due           TYPE zsdt0170-id_due,
         cd_material      TYPE zreg_exportacao-cd_material,
       END OF ty_vinc_doc_exp.

DATA: BEGIN OF t_conhec_a OCCURS 0,
        marc  TYPE char1,
        id    TYPE zid_doc,
        item  TYPE zid_item,
        landx TYPE landx.
        INCLUDE TYPE znom_conhec.
DATA:  END  OF t_conhec_a.

TYPES: BEGIN OF ty_fluxosd,
         mark          TYPE char1,
*        PTAX  type VBRK-ptax, "verificar
         kurrf         TYPE vbrk-kurrf,
         fkart         TYPE vbrk-fkart,
         docnum_nfe    TYPE j_1bdocnum, "J_1BNFLIN-DOCNUM
         nfe_num       TYPE j_1bnfnum9,  "J_1BNFDOC-NFNUM9
         chave_nf      TYPE zde_chave_doc_e,
         data_nf       TYPE j_1bcredat,  "J_1BNFDOC-DOCDAT
         quantidade_nf TYPE j_1bnetqty,  "J_1BNFLIN-MENGE
         preco_nf      TYPE j_1bnetpri,  "J_1BNFLIN-NETPR
         valor_nf      TYPE j_1bnftot,  "J_1BNFLIN-NETWR
         inco1         TYPE j_1bdydoc-inco1, "J_1BNFDOC-INCO1
         werks         TYPE j_1bnflin-werks. "J_1BNFLIN-WERKS
         INCLUDE       TYPE tds_docflow.
TYPES: END OF ty_fluxosd.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.


*----------------------------------------------------------------------*
* TABELA INTERNAS
*----------------------------------------------------------------------*
DATA: tg_0051          TYPE TABLE OF zsdt0051,
      tg_0053          TYPE TABLE OF zsdt0053,
      tg_0051_form     TYPE TABLE OF zsdt0051,
      tg_0054          TYPE TABLE OF zsdt0054,
      tg_0066          TYPE TABLE OF zsdt0066,
      tg_0045          TYPE TABLE OF zsdt0045,
      tg_0327          TYPE TABLE OF zsdt0327,
      tg_bsad          TYPE TABLE OF ty_bsad,
      tg_objek         TYPE TABLE OF ty_objek,
      tg_makt          TYPE TABLE OF makt,
      tg_t001w         TYPE TABLE OF t001w,
      tg_kna1          TYPE TABLE OF kna1,
      tg_local_entrega TYPE TABLE OF kna1,
      tg_saida         TYPE TABLE OF ty_saida,
      tg_fluxosd       TYPE TABLE OF zcl_util_sd=>ty_fluxosd_alv, "RJF
      tg_fluxosd_t     TYPE TABLE OF zcl_util_sd=>ty_fluxosd, "RJF
      tg_saldo         TYPE TABLE OF ty_saldo,
*** Stefanini - IR252396 - 12/08/2025 - LAZAROSR - Início de Alteração
      tg_saldo_pedido  TYPE TABLE OF ty_saldo_pedido,
*** Stefanini - IR252396 - 12/08/2025 - LAZAROSR - Fim de Alteração
      tg_lfa1          TYPE TABLE OF lfa1,
      tg_ponto_coleta  TYPE TABLE OF lfa1,
      tg_terminal      TYPE TABLE OF lfa1,
      tg_bdc           TYPE TABLE OF bdcdata,
      tg_zdoc_exp      TYPE TABLE OF zdoc_exp WITH HEADER LINE,
      tg_znom_transp   TYPE TABLE OF znom_transporte,
      it_doc           TYPE TABLE OF ty_j_1bnfdoc,
      it_lin           TYPE TABLE OF ty_j_1bnflin,
      it_vbfa_rt       TYPE TABLE OF ty_vbfa_rt,
      it_check         TYPE TABLE OF ty_check,
      t_conhec_v       LIKE TABLE OF t_conhec_a,
      t_full           LIKE TABLE OF t_conhec_a,
      it_vbak          TYPE TABLE OF vbak. "US 152645 // MMSILVA - 11-10-2024



*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: wg_0051         TYPE zsdt0051,
      wg_0053         TYPE zsdt0053,
      wg_0054         TYPE zsdt0054,
      wg_0066         TYPE zsdt0066,
      wg_0045         TYPE zsdt0045,
      wg_0327         TYPE zsdt0327,
      wg_bsad         TYPE ty_bsad,
      wg_objek        TYPE ty_objek,
      wg_makt         TYPE makt,
      wg_t001w        TYPE t001w,
      wg_kna1         TYPE kna1,
      wg_saida        TYPE ty_saida,
      wg_saldo        TYPE ty_saldo,
*** Stefanini - IR252396 - 12/08/2025 - LAZAROSR - Início de Alteração
      wg_saldo_pedido TYPE ty_saldo_pedido,
*** Stefanini - IR252396 - 12/08/2025 - LAZAROSR - Fim de Alteração
      wg_lfa1         TYPE lfa1,
      wg_ponto_coleta TYPE lfa1,
      wg_terminal     TYPE lfa1,
      wg_bdc          TYPE bdcdata,
      wa_check        TYPE ty_check,
      wa_vc_doc_exp   TYPE ty_vinc_doc_exp,
      p_tpvenda       TYPE char2,
      check_tpvenda,
      msg_venda(255),
      wg_vbak         TYPE vbak. "US 152645 // MMSILVA - 11-10-2024

* Início - Sara Oikawa - CS2020000143 - 30.11.2020
DATA: wa_0158        TYPE zsdt0158.
* Fim - Sara Oikawa - CS2020000143 - 30.11.2020

DATA: vg_mode(1) TYPE c VALUE 'N'.

DATA: it_msg TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
      wa_msg TYPE bdcmsgcoll,

      it_dta TYPE STANDARD TABLE OF bdcdata,
      wa_dta LIKE LINE OF           it_dta,

      BEGIN OF it_msgtext OCCURS 0,
        texto TYPE t100-text,
      END OF it_msgtext.




*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader,
      t_sort       TYPE slis_t_sortinfo_alv WITH HEADER LINE.

DATA: cont TYPE sy-tabix.
DATA: cont_1 TYPE n LENGTH 4.
DATA: concat TYPE c LENGTH 255.
DATA vl_lenght TYPE i.

*----------------------------------------------------------------------*
* TELA DE SELECAO.
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-015.
    SELECT-OPTIONS: s_vkorg  FOR wg_0051-vkorg OBLIGATORY,
                    s_vtweg  FOR wg_0051-vtweg,
                    s_spart  FOR wg_0051-spart,
                    p_matnr  FOR zsdt0053-matnr MATCHCODE OBJECT z_sh_maktg, " Rubenilson Pereira - 10.02.25 - US164018
                    s_kunnr  FOR wg_0051-kunnr MATCHCODE OBJECT z_sh_kunnr," Rubenilson Pereira - 10.02.25 - US164018
                    s_nr_sol FOR wg_0051-nro_sol_ov,
                    s_ordem  FOR wg_0053-vbeln,
                    s_tpven  FOR wg_0051-tp_venda,
                    s_dtven  FOR wg_0051-data_venda,
                    s_inco1  FOR wg_0051-inco1 NO-EXTENSION NO INTERVALS,
                    s_ruc    FOR wg_0053-numero_ruc,
                    s_erdat  FOR wg_vbak-erdat NO-EXTENSION. "US 152645 // MMSILVA - 11-10-2024
  SELECTION-SCREEN: END OF BLOCK b4.

  SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-002.
    PARAMETER: p_form RADIOBUTTON GROUP b2,
               p_venda RADIOBUTTON GROUP b2 DEFAULT 'X'.
  SELECTION-SCREEN: END OF BLOCK b3.

  SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-014.
    PARAMETERS: p_gera RADIOBUTTON GROUP a1,
                p_cons RADIOBUTTON GROUP a1.
  SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: END OF BLOCK b1.


CLASS zcl_retorno DEFINITION.

  PUBLIC SECTION.

    DATA: at_set    TYPE TABLE OF rgsb4.

    TYPES: BEGIN OF ty_msgtext,
             texto TYPE t100-text,
           END OF ty_msgtext.

    TYPES: BEGIN OF ty_msg,
             inco1    TYPE zsdt0041-werks,
             spart    TYPE zsdt0041-spart,
             auart    TYPE zsdt0041-auart,
             werks    TYPE zsdt0041-werks,
             vbeln    TYPE vbak-vbeln,
             msg(255),
           END OF ty_msg,
           tmsg TYPE TABLE OF ty_msg WITH DEFAULT KEY.

    DATA: it_dta        TYPE STANDARD TABLE OF bdcdata,
          it_msg        TYPE TABLE OF bdcmsgcoll,
          it_msgtext    TYPE TABLE OF ty_msgtext,
          at_text       TYPE TABLE OF tline,
          tl_nro_sol_ov TYPE TABLE OF zsds007,
          tl_fields     TYPE TABLE OF sval,
          at_documento  TYPE vbeln,
          at_erro       TYPE sy-subrc.

    DATA  at02 TYPE RANGE OF zsdt0051-nro_sol_ov.

    METHODS:
      get_line
        RETURNING VALUE(return) TYPE ty_saida,
      read_text
        IMPORTING input TYPE vgbel,
      save_text
        IMPORTING input TYPE vgbel,
      retorno
        IMPORTING input TYPE ty_saida OPTIONAL,
      remfat
        IMPORTING input TYPE ty_saida OPTIONAL,
      envia_trace                                 "*-CS2023000189-26.04.2023-#108710-JT
        IMPORTING input TYPE ty_saida OPTIONAL,
      log_trace                                   "*-CS2023000189-26.04.2023-#108710-JT
        IMPORTING input TYPE ty_saida OPTIONAL,
      delivery
        IMPORTING input         TYPE ty_saida OPTIONAL
        CHANGING  input1        TYPE vstel OPTIONAL
                  input2        TYPE ledat OPTIONAL
        RETURNING VALUE(return) TYPE vbeln_vl,
      picking
        IMPORTING input         TYPE vbeln_vl
        RETURNING VALUE(return) TYPE char1,
      get_header
        IMPORTING input         TYPE vgbel OPTIONAL
        RETURNING VALUE(return) TYPE vbrk,
      get_konv
        IMPORTING input TYPE knumv OPTIONAL,
      get_real
        IMPORTING input         TYPE werks_d OPTIONAL
        RETURNING VALUE(return) TYPE werks_d,
      get_vbfa
        IMPORTING input TYPE vgbel OPTIONAL,
      get_itens
        IMPORTING input TYPE vgbel OPTIONAL,
      nf
        IMPORTING input TYPE ty_saida OPTIONAL,
      exibe_erros,
      get_docnum
        IMPORTING input         TYPE vbeln_vl OPTIONAL
        RETURNING VALUE(return) TYPE  j_1bdocnum,
      get_set
        RETURNING VALUE(return) TYPE rgsb4,
      set_0053
        IMPORTING input TYPE STANDARD TABLE,
      set_saida
        IMPORTING input  TYPE STANDARD TABLE
                  input1 TYPE char1,
      set_exists
        IMPORTING input         TYPE auart OPTIONAL
        RETURNING VALUE(return) TYPE char1,
      check_del
        RETURNING VALUE(return) TYPE zsdt0053,
      commit,
      rollback,
      message
        IMPORTING input         TYPE prott
        RETURNING VALUE(return) TYPE bapi_msg,
      retorno_erros
        IMPORTING input TYPE STANDARD TABLE,
      mensagem_acao
        IMPORTING input TYPE char255,
      geraov
        IMPORTING input TYPE ty_saida OPTIONAL,

      " 12.09.2024 - 145583 -- RAMON -->
      geraped
        IMPORTING input TYPE ty_saida OPTIONAL,


      exibe_msg
        IMPORTING it_ret TYPE bapiret2_tab,

      " 12.09.2024 - 145583 -- RAMON --<

      funcao
        IMPORTING input         TYPE char1
        RETURNING VALUE(return) TYPE char255,
      get_tvak
        IMPORTING input         TYPE auart
        RETURNING VALUE(return) TYPE tvak-bezob,
      get_vbrk
        IMPORTING input         TYPE vbeln
        RETURNING VALUE(return) TYPE auart,
      get_tvcpa
        IMPORTING input         TYPE auart
                  input1        TYPE auart
        RETURNING VALUE(return) TYPE sy-subrc,
      get_documento
        IMPORTING input TYPE auart,
      set_documento
        RETURNING VALUE(return) TYPE char1,
      get_vbeln,
      refresh,
      ajuste_53 IMPORTING simulador TYPE zsds007_t,
      desconto_abs IMPORTING simulador TYPE zsds007_t,
      diferenca IMPORTING simulador TYPE zsds007_t,
      zsdmf001_atuali_ov_simulador IMPORTING i_vbeln     TYPE vbeln
                                             i_posnr     TYPE posnr
                                             i_matnr     TYPE matnr
                                             i_diferenca TYPE kbetr.

  PROTECTED SECTION.

    DATA: at_konv   TYPE TABLE OF konv,
          at_itens  TYPE TABLE OF vbrp,
          at_vbfa   TYPE TABLE OF vbfa,
          at_return TYPE TABLE OF bapireturn1.

ENDCLASS.

DATA(obj_ret) = NEW zcl_retorno( ).

CLASS zcl_retorno IMPLEMENTATION.

  METHOD get_line.

    CLEAR return.

    IF REDUCE i( INIT x = 0 FOR ls IN tg_saida WHERE ( mark = abap_true ) NEXT x = x + 1 ) EQ 1.
      return = tg_saida[ mark = abap_true ].
    ELSE.
      MESSAGE |Selecione uma Linha!| TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.

*-CS2023000189-26.04.2023-#108710-JT-inicio
  METHOD envia_trace.

    CHECK input-nro_sol_ov IS NOT INITIAL.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 50
        text       = 'Aguarde...Integrando...'.

    DATA(l_task) = 'TRACE_ORDEM_VENDA' && input-nro_sol_ov && input-posnr.

    CALL FUNCTION 'ZSD_ENVIO_ORDEM_VENDA_TRACE' STARTING NEW TASK l_task
      EXPORTING
        i_nro_sol_ov = input-nro_sol_ov
        i_posnr      = input-posnr
        i_acao       = 'C'
      EXCEPTIONS
        OTHERS       = 1.

    READ TABLE tg_saida INTO DATA(w_saida) WITH KEY nro_sol_ov = input-nro_sol_ov
                                                    posnr      = input-posnr.
    IF sy-subrc = 0.
      w_saida-status_trace = icon_activity.
      MODIFY tg_saida   FROM w_saida INDEX sy-tabix.
    ENDIF.

*   me->refresh( ).

  ENDMETHOD.

  METHOD log_trace.

*    CHECK input-nro_sol_ov IS NOT INITIAL.
*
*    CALL FUNCTION 'ZSD_LOG_ENVIO_TRACE_COTTON'
*      EXPORTING
*        i_nro_sol_ov   = input-nro_sol_ov
*        i_posnr        = input-posnr
*        i_tipo_integra = 'OV'.
*
*    me->refresh( ).

  ENDMETHOD.
*-CS2023000189-26.04.2023-#108710-JT-fim

  METHOD retorno.

    DATA: it_rsparams TYPE TABLE OF rsparams,
          wl_rsparams TYPE rsparams.

    DATA: lv_docnum     TYPE j_1bdocnum,
          lv_idredex    TYPE zid_export,
          lv_qtd2       TYPE c LENGTH 16,
          v_final       TYPE c,
          v_centro_real TYPE zsdt_depara_cen-centro_real.

    IF me->set_exists( input-auart ) IS NOT INITIAL.
      MESSAGE |Tipo de Ordem sem Autorização para esta Ação!| TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF input-docnum_rt IS INITIAL.

      me->mensagem_acao( 'Executando a Transação ZSDT0008!' ).

*      IT_RSPARAMS = VALUE #(
*      ( SELNAME = 'P_BUKRS' KIND = 'P' SIGN = ''  OPTION = ''   LOW = INPUT-VKORG    )
*      ( SELNAME = 'P_FKART' KIND = 'P' SIGN = ''  OPTION = ''   LOW = 'ZRFL'         )
*      ( SELNAME = 'P_WERKS' KIND = 'P' SIGN = ''  OPTION = ''   LOW = GET_REAL( INPUT-WERKS ) )
*      ( SELNAME = 'S_LGORT' KIND = 'S' SIGN = 'I' OPTION = 'CP' LOW = '*'         )
*      ( SELNAME = 'S_SAFRA' KIND = 'S' SIGN = 'I' OPTION = 'EQ' LOW = INPUT-CHARG    )
*      ( SELNAME = 'S_KUNNR' KIND = 'S' SIGN = 'I' OPTION = 'EQ' LOW = GET_REAL( INPUT-WERKS ) )
*      ( SELNAME = 'S_PARC ' KIND = 'S' SIGN = 'I' OPTION = 'EQ' LOW = INPUT-TERMINAL )
*      ( SELNAME = 'S_MATNR' KIND = 'S' SIGN = 'I' OPTION = 'EQ' LOW = INPUT-MATNR    )
*      ( SELNAME = 'P_FINAL' KIND = 'P' SIGN = ''  OPTION = ''   LOW = 'E'            )
*      ( SELNAME = 'P_QUANT' KIND = 'P' SIGN = ''  OPTION = ''   LOW = INPUT-ZMENG    ) ).
*
*      IF P_VENDA IS NOT INITIAL.
*        WL_RSPARAMS-SELNAME = 'P_COMCCT'.
*        WL_RSPARAMS-KIND    = 'P'.
*        WL_RSPARAMS-OPTION  = ''.
*        WL_RSPARAMS-LOW     = ''.
*        APPEND WL_RSPARAMS TO IT_RSPARAMS.
*
*        WL_RSPARAMS-SELNAME = 'P_SEMCCT'.
*        WL_RSPARAMS-KIND    = 'P'.
*        WL_RSPARAMS-OPTION  = ''.
*        WL_RSPARAMS-LOW     = ''.
*        APPEND WL_RSPARAMS TO IT_RSPARAMS.
*
*        WL_RSPARAMS-SELNAME = 'P_ALLCCT'.
*        WL_RSPARAMS-KIND    = 'P'.
*        WL_RSPARAMS-OPTION  = ''.
*        WL_RSPARAMS-LOW     = 'X'.
*        APPEND WL_RSPARAMS TO IT_RSPARAMS.
*      ENDIF.
*
*      SUBMIT ZSDI0001 WITH SELECTION-TABLE IT_RSPARAMS AND RETURN.

      "EXPORT V_COMCCT TO MEMORY ID 'P_COMCCT'.
      "EXPORT V_SEMCCT TO MEMORY ID 'P_SEMCCT'.
      "EXPORT V_ALLCCT TO MEMORY ID 'P_ALLCCT'.

      EXPORT v_tcode FROM sy-tcode TO MEMORY ID 'P_TCODE'.

      MOVE input-zmeng TO lv_qtd2.
      TRANSLATE lv_qtd2 USING '.,'.
      CONDENSE lv_qtd2 NO-GAPS.

      CLEAR: v_centro_real.
      v_centro_real = get_real( input-werks ).

      v_final = 'E'.

      IF input-tp_venda IS NOT INITIAL.
        SELECT SINGLE *
          FROM zsdt0057 INTO @DATA(_wl_0057)
         WHERE tp_venda = @input-tp_venda.

        IF ( sy-subrc EQ 0 ) AND ( _wl_0057-param_espec EQ 'P' ).
          v_final = 'P'.
        ENDIF.
      ENDIF.

      CLEAR: it_dta[].
      PERFORM f_bdc_data USING:
            ''          ''      'T' 'ZSDT0008'      '',
            'ZSDI0001'  '1000'  'X' ''              '',
            ''          ''      ''  'P_BUKRS'       input-vkorg,
            ''          ''      ''  'P_FKART'       'ZRFL',
            ''          ''      ''  'P_WERKS'       v_centro_real,
            ''          ''      ''  'S_LGORT-LOW'   '*',
            ''          ''      ''  'S_SAFRA-LOW'   input-charg,
            ''          ''      ''  'S_KUNNR-LOW'   v_centro_real,
            ''          ''      ''  'S_PARC-LOW'    input-terminal,
            ''          ''      ''  'S_MATNR-LOW'   input-matnr,
            ''          ''      ''  'S_INST-LOW'    input-instrucao, "ZSDT0008 - Adicionar filtro instrucao - BG #128346
            ''          ''      ''  'P_FINAL'       v_final,
            ''          ''      ''  'P_QUANT'       lv_qtd2.

      PERFORM f_call_transaction USING 'ZSDT0008'
                                       'E'
                                       'S'.

      IMPORT docnum TO lv_docnum FROM MEMORY ID 'ZRET'.
      FREE MEMORY ID 'ZRET'.
      "84616 - CS2022000665 - Gravar ID_EXport na tabela zsdt0053 LP "Comentado para subida de outra demanda
      IMPORT id_redex TO lv_idredex FROM MEMORY ID 'ZRET_REDEX'.
      FREE MEMORY ID 'ZRET_REDEX'.
      IF lv_docnum IS NOT INITIAL.

        SELECT COUNT(*)
          FROM j_1bnfdoc
          WHERE docnum = lv_docnum.

        IF sy-subrc IS NOT INITIAL.
          DELETE FROM zsdt_export WHERE  docnum = lv_docnum.
          DELETE FROM zsdt_retlote WHERE docnum_ret = lv_docnum.
          MESSAGE |Nota de retorno não gerada, Tentar Novamente!| TYPE 'I'.
        ELSE.
          me->mensagem_acao( |Atualizando a Tabela ZSDT0053 com a Retorno { lv_docnum }!| ).
          lv_docnum = |{ lv_docnum ALPHA = IN }|.
          UPDATE zsdt0053 SET docnum_rt = lv_docnum
                               id_export = lv_idredex "84616   LP "84616 - CS2022000665 - Gravar ID_EXport na tabela
               WHERE nro_sol_ov EQ input-nro_sol_ov AND
                     posnr      EQ input-posnr.
          MESSAGE |Retorno nº { lv_docnum } gerado com Sucesso!| TYPE 'S'.
        ENDIF.
      ELSE.
        MESSAGE |Nota de retorno não gerada!| TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    ELSE.
      MESSAGE |Documento ja Possui Retorno!| TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

    me->refresh( ).

  ENDMETHOD.

  METHOD remfat.

    DATA: t_success   TYPE TABLE OF bapivbrksuccess,
          t_billing   TYPE TABLE OF bapivbrk,
          vl_delivery TYPE vbeln_vl,
          t_return    TYPE TABLE OF bapireturn1.

    IF me->set_exists( input-auart ) IS NOT INITIAL.
      MESSAGE |Tipo de Ordem sem Autorização para esta Ação!| TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    FREE: t_return, t_success.

    me->mensagem_acao( 'Gerando Documento de Remessa!' ).
    vl_delivery = me->delivery( input ).

    IF vl_delivery IS INITIAL.
      me->exibe_erros( ).
      EXIT.
    ENDIF.

    me->commit( ).

    me->mensagem_acao( 'Realizando o Movimento de Mercadoria!' ).
    IF me->picking( vl_delivery ) IS NOT INITIAL.
      EXIT.
    ENDIF.

    me->mensagem_acao( |Atualizando a Tabela ZSDT0053 com a Remessa { vl_delivery } !| ).
    UPDATE zsdt0053 SET remessa_exp = vl_delivery
         WHERE nro_sol_ov EQ input-nro_sol_ov AND
               posnr      EQ input-posnr.

    APPEND VALUE #(
                    ref_doc    = vl_delivery
                    ref_doc_ca = 'J'
                    bill_date  = sy-datum
                  ) TO t_billing.

    me->mensagem_acao( |Gerando a Fatura!| ).
    CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
      TABLES
        billingdatain = t_billing
        return        = t_return
        success       = t_success.

    me->retorno_erros( t_return ).

    IF t_success IS INITIAL.
      me->exibe_erros( ).
      EXIT.
    ENDIF.

    me->commit( ).

    me->read_text( input-vbeln ).
    me->save_text( t_success[ 1 ]-bill_doc ).

    me->exibe_erros( ).

    me->refresh( ).

  ENDMETHOD.

  METHOD delivery.

    TYPES: bapidlvreftosalesorder TYPE TABLE OF bapidlvreftosalesorder WITH DEFAULT KEY.

    DATA t_return TYPE TABLE OF bapiret2.

    input1 = input-werks.
    input2 = sy-datum.

    DATA(table) =
    VALUE bapidlvreftosalesorder( (
                                    ref_doc    = input-vbeln
                                    ref_item   = 10
                                    dlv_qty    = input-zmeng
                                    sales_unit = input-zieme
                                ) ).

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
      EXPORTING
        ship_point        = input1
        due_date          = input2
      IMPORTING
        delivery          = return
      TABLES
        sales_order_items = table
        return            = t_return.

    me->retorno_erros( t_return ).

  ENDMETHOD.

  METHOD picking.

    DATA: wa_vbkok TYPE vbkok,
          tl_vbpok TYPE TABLE OF vbpok,
          t_return TYPE TABLE OF bapiret2,
          tl_prot  TYPE TABLE OF prott.

    DATA: wa_hdata TYPE bapiobdlvhdrchg,
          wa_hcont TYPE bapiobdlvhdrctrlchg.

    SELECT *
      FROM lips
        INTO TABLE @DATA(it_lips)
      WHERE vbeln EQ @input.

    wa_vbkok-vbeln_vl  = input.
    wa_vbkok-vbeln     = input.
    wa_vbkok-wabuc     = abap_true.
    wa_vbkok-komue     = abap_true.
    wa_vbkok-wadat_ist = sy-datum.

    tl_vbpok = VALUE #( FOR ls IN  it_lips (
                                            vbeln_vl = input
                                            posnr_vl = ls-posnr
                                            vbeln    = input
                                            posnn    = ls-posnr
                                            matnr    = ls-matnr
                                            pikmg    = ls-lfimg
                                            charg    = ls-charg
                                            gewei    = ls-gewei
                                            lgort    = ls-lgort
                                            brgew    = ls-lfimg
                                            ntgew    = ls-lfimg
                                            )
                       ).

    CALL FUNCTION 'SD_DELIVERY_UPDATE_PICKING_1'
      EXPORTING
        vbkok_wa                 = wa_vbkok
        synchron                 = abap_true
        if_error_messages_send_1 = abap_true
      TABLES
        vbpok_tab                = tl_vbpok
        prot                     = tl_prot.

    IF tl_prot IS INITIAL.
      me->commit( ).
      return = abap_false.
    ELSE.
      me->rollback( ).

      t_return = VALUE #( FOR ln IN  tl_prot (
                          type = ln-msgty
                          message = me->message( ln )
                        ) ).

      wa_hdata-deliv_numb = input.
      wa_hcont-deliv_numb = input.
      wa_hcont-dlv_del    = abap_true.

      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
        EXPORTING
          header_data    = wa_hdata
          header_control = wa_hcont
          delivery       = input
        TABLES
          return         = t_return.

      me->commit( ).

      me->retorno_erros( t_return ).
      me->exibe_erros( ).

      return = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD get_header.
    SELECT SINGLE * FROM vbrk INTO return WHERE vbeln EQ input.
  ENDMETHOD.

  METHOD get_konv.
    SELECT * FROM konv INTO TABLE at_konv WHERE knumv EQ input AND   kschl EQ 'PR00'.
  ENDMETHOD.

  METHOD get_real.
    SELECT SINGLE centro_real FROM zsdt_depara_cen INTO return WHERE centrov_1 EQ input.
  ENDMETHOD.

  METHOD get_vbfa.
    SELECT * FROM vbfa INTO TABLE at_vbfa WHERE vbeln EQ input.
  ENDMETHOD.

  METHOD get_itens.
    SELECT * FROM vbrp INTO TABLE at_itens WHERE vbeln EQ input.
  ENDMETHOD.

  METHOD refresh.
    PERFORM selecionar_dados.
    PERFORM organizacao_dados.
  ENDMETHOD.

  METHOD get_set.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        setnr      = '0000AUART_ZSDT0066'
      TABLES
        set_values = at_set.

  ENDMETHOD.

  METHOD set_0053.

    DATA(x03) = tg_0053[].
    FREE: x03.

    IF input IS NOT INITIAL.
      x03 = VALUE #( FOR ls_0053 IN tg_0053 WHERE ( nro_sol_ov IN input AND vbeln IS NOT INITIAL ) ( ls_0053 ) ).
    ENDIF.

    DELETE tg_0053 WHERE vbeln IS NOT INITIAL.

    APPEND LINES OF x03 TO tg_0053.

  ENDMETHOD.

  METHOD set_saida.

    CHECK input1 IS INITIAL.
    CHECK input IS NOT INITIAL.

    DATA: x01 TYPE RANGE OF zsdt0053-vbeln.
    DATA(x02) = tg_saida[].
    FREE: x02.

    x01 = VALUE #( FOR ls_saida IN tg_saida WHERE ( nro_sol_ov IN input )
    LET s = 'I' o = 'EQ' IN sign = s option = o ( low = ls_saida-vbeln ) ).

    x02 = VALUE #( FOR ls_saida IN tg_saida WHERE ( nro_sol_ov IN input ) ( ls_saida ) ).

    IF  x02 IS NOT INITIAL.
      DELETE x02 WHERE nfenum_exp IS INITIAL.
    ENDIF.

    IF x01 IS NOT INITIAL .
      DELETE tg_saida WHERE vbeln IN x01.
    ENDIF.



    APPEND LINES OF x02 TO tg_saida.

  ENDMETHOD.

  METHOD get_docnum.

    DATA: refkey TYPE j_1bnflin-refkey.
    DATA: tg_vbfa TYPE TABLE OF vbfa.

    SELECT *
      FROM vbfa AS fa
      INNER JOIN vbrk AS rk ON rk~vbeln EQ fa~vbeln
      INTO CORRESPONDING FIELDS OF TABLE tg_vbfa
      WHERE fa~vbelv   EQ input
        AND fa~vbtyp_n EQ 'M'
        AND fa~vbtyp_v EQ 'J'
        AND rk~fksto   EQ abap_false.

    CHECK sy-subrc IS INITIAL.

    refkey = tg_vbfa[ 1 ]-vbeln.

    SELECT SINGLE docnum
      FROM j_1bnflin
      INTO return
      WHERE refkey EQ refkey.

  ENDMETHOD.

  METHOD set_exists.
    me->get_set( ).
    IF NOT line_exists( at_set[ from = input ] ).
      return = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD nf.
    DATA: opt    TYPE ctu_params,
          tl_bdc TYPE TABLE OF bdcdata.

    IF me->set_exists( input-auart ) IS NOT INITIAL.
      MESSAGE |Tipo de Ordem sem Autorização para esta Ação!| TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF input-docnum_rt IS INITIAL.
      MESSAGE |Não Existe Documento de Retorno!| TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF input-remessa_exp IS NOT INITIAL.
      DATA(docnum) = me->get_docnum( input-remessa_exp ).
    ENDIF.

    tl_bdc = VALUE #(
    ( dynbegin = abap_true  program = 'Z_1BNFE_MONITOR' dynpro = '1000'      fnam = abap_false               fval = abap_false      )
    ( dynbegin = abap_false program = abap_false        dynpro = abap_false  fnam = 'BDC_CURSOR'             fval = 'BUKRS-LOW'     )
    ( dynbegin = abap_false program = abap_false        dynpro = abap_false  fnam = 'BDC_OKCODE'             fval = '=%001'         )
    ( dynbegin = abap_false program = abap_false        dynpro = abap_false  fnam = 'DOCNUM-LOW'             fval = input-docnum_rt )
    ( dynbegin = abap_false program = abap_false        dynpro = abap_false  fnam = 'BUKRS-LOW'              fval = input-vkorg     )

    ( dynbegin = abap_true  program = 'SAPLALDB'        dynpro = '3000'      fnam = abap_false               fval = abap_false      )
    ( dynbegin = abap_false program = abap_false        dynpro = abap_false  fnam = 'BDC_OKCODE'             fval = '=ACPT'         )
    ( dynbegin = abap_false program = abap_false        dynpro = abap_false  fnam = 'BDC_SUBSCR'             fval = 'SAPLALDB                                3010SCREEN_HEADER' )
    ( dynbegin = abap_false program = abap_false        dynpro = abap_false  fnam = 'BDC_CURSOR'             fval = 'RSCSEL_255-SLOW_I(02)' )
    ( dynbegin = abap_false program = abap_false        dynpro = abap_false  fnam = 'RSCSEL_255-SLOW_I(01)'  fval = input-docnum_rt )
    ( dynbegin = abap_false program = abap_false        dynpro = abap_false  fnam = 'RSCSEL_255-SLOW_I(02)'  fval = docnum          )

    ( dynbegin = abap_true  program = 'Z_1BNFE_MONITOR' dynpro = '1000'      fnam = abap_false               fval = abap_false      )
    ( dynbegin = abap_false program = abap_false        dynpro = abap_false  fnam = 'BDC_CURSOR'              fval = 'BUKRS-LOW'    )
    ( dynbegin = abap_false program = abap_false        dynpro = abap_false  fnam = 'BDC_OKCODE'             fval = '=ONLI'         )
    ( dynbegin = abap_false program = abap_false        dynpro = abap_false  fnam = 'DOCNUM-LOW'             fval = input-docnum_rt )
    ( dynbegin = abap_false program = abap_false        dynpro = abap_false  fnam = 'BUKRS-LOW'              fval = input-vkorg     )
    ).

    opt-dismode  = 'E'.
    opt-defsize  = abap_false.
    opt-racommit = abap_true.

    CALL TRANSACTION 'ZNFE' USING tl_bdc OPTIONS FROM opt.

    me->refresh( ).

  ENDMETHOD.

  METHOD save_text.

    DATA: wa_header TYPE thead.

    wa_header-tdobject = 'VBBK'.
    wa_header-tdname   = input.
    wa_header-tdid     = '0002'.
    wa_header-tdspras  = sy-langu.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client          = sy-mandt
        header          = wa_header
        savemode_direct = abap_true
      TABLES
        lines           = at_text
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.

    FREE at_text.

  ENDMETHOD.

  METHOD read_text.

    DATA: name TYPE tdobname.

    FREE: at_text.

    name = input.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id        = '0002'
        language  = sy-langu
        name      = name
        object    = 'VBBK'
      TABLES
        lines     = at_text
      EXCEPTIONS
        id        = 1
        language  = 2
        name      = 3
        not_found = 4
        OTHERS    = 5.

  ENDMETHOD.

  METHOD check_del.

    DATA x01 TYPE RANGE OF auart.

    me->get_set( ).

    x01 = VALUE #( FOR ra IN at_set
                  ( option = 'EQ' sign = 'I' low = ra-from )
                 ).

    at02 = VALUE #( FOR ls_0051 IN tg_0051 WHERE ( auart IN x01 )
                  ( option = 'EQ' sign = 'I' low = ls_0051-nro_sol_ov )
                 ).

    SORT at02 BY low.
    DELETE ADJACENT DUPLICATES FROM at02 COMPARING low.

  ENDMETHOD.

  METHOD exibe_erros.

    CHECK at_return[] IS NOT INITIAL.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_structure_name      = 'BAPIRETURN1'
        i_save                = 'A'
        i_screen_start_column = 3
        i_screen_start_line   = 3
        i_screen_end_column   = 100
        i_screen_end_line     = 13
      TABLES
        t_outtab              = at_return.

  ENDMETHOD.

  METHOD commit.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = abap_true.
    WAIT UP TO 2 SECONDS.
  ENDMETHOD.

  METHOD rollback.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDMETHOD.

  METHOD message.
    MESSAGE ID input-msgid TYPE input-msgty NUMBER input-msgno WITH input-msgv1 input-msgv2 input-msgv3 input-msgv4 INTO return.
  ENDMETHOD.

  METHOD retorno_erros.
    APPEND LINES OF input TO at_return.
  ENDMETHOD.

  METHOD mensagem_acao.
    cl_progress_indicator=>progress_indicate( i_text = input ).
  ENDMETHOD.

  METHOD geraov.

    DATA: cont TYPE i.
    FREE MEMORY ID  'ZID_REDEX'.
    CASE input-auart.
      WHEN 'ZEXI' OR 'ZEXP'.

        LOOP AT tg_saida ASSIGNING FIELD-SYMBOL(<saida>) WHERE nro_sol_ov EQ input-nro_sol_ov.

          IF <saida>-mark IS NOT INITIAL.
            " CLEAR:tl_nro_sol_ov.
            APPEND VALUE #(
                            nro_sol_ov = <saida>-nro_sol_ov
                            posnr      = <saida>-posnr
                           ) TO tl_nro_sol_ov.
            "LP 84616 - CS2022000665 - Gravar ID_EXport
            DATA(lv_idexport) = <saida>-id_export.
            EXPORT id_redex FROM lv_idexport  TO MEMORY ID 'ZID_REDEX'.
            "<< END.
          ENDIF.

        ENDLOOP.

      WHEN OTHERS.

        tl_nro_sol_ov = VALUE #(
                                 FOR ls1 IN tg_saida WHERE ( nro_sol_ov EQ input-nro_sol_ov )
                                   (
                                     nro_sol_ov = ls1-nro_sol_ov
                                     posnr      = ls1-posnr
                                   )
                               ).

        LOOP AT tg_saida ASSIGNING <saida> WHERE nro_sol_ov EQ input-nro_sol_ov.
          <saida>-mark = abap_true.
        ENDLOOP.

        me->get_documento( input-auart ).
    ENDCASE.

    CHECK me->at_erro IS INITIAL.
    CHECK tl_nro_sol_ov IS NOT INITIAL.

    DATA(funcao) = me->funcao( p_form ).

    CALL FUNCTION funcao
      EXPORTING
        i_vbeln                = at_documento
      TABLES
        ti_nro_sol_ov          = tl_nro_sol_ov
      EXCEPTIONS
        ov_ja_criada           = 1
        solicitacao_nao_existe = 2
        OTHERS                 = 3.

    IF p_venda IS NOT INITIAL.

      CALL METHOD diferenca
        EXPORTING
          simulador = tl_nro_sol_ov.

    ENDIF.

    " 06.05.2022 - RAMON LIMA - RECKLIKE 76054 -->

    CALL FUNCTION 'ZSD_SAVE_ZSDT0187'
      EXPORTING
        i_commit = 'X'
      TABLES
        it_007   = tl_nro_sol_ov.
    " 06.05.2022 - RAMON LIMA - RECKLIKE 76054 --<


*    CALL METHOD DESCONTO_ABS( TL_NRO_SOL_OV ).

*    IF P_VENDA IS NOT INITIAL.
*
*      DO.
*        IF CONT EQ 10.
*          EXIT.
*        ENDIF.
*
*        CALL METHOD AJUSTE_53( TL_NRO_SOL_OV ).
*        ADD 1 TO CONT.
*      ENDDO.
*
*    ENDIF.

    me->refresh( ).

    CLEAR: at_documento.

  ENDMETHOD.

  METHOD geraped.

    DATA lt_bapiret TYPE TABLE OF bapiret2.
    DATA lv_erro.

    BREAK rblima. " #apagar #debug

    LOOP AT tg_saida ASSIGNING FIELD-SYMBOL(<saida>)
        WHERE nro_sol_ov EQ input-nro_sol_ov.

      CHECK <saida>-vbeln IS INITIAL.

      CALL FUNCTION 'ZSDMF001_GERA_PED_SOLICITACAO'
        EXPORTING
          iv_nro_sol_ov          = input-nro_sol_ov
        IMPORTING
          ev_ebeln               = <saida>-vbeln
        TABLES
          et_bapiret2            = lt_bapiret
        EXCEPTIONS
          ped_ja_existe          = 1
          solicitacao_nao_existe = 2
          OTHERS                 = 3.

      IF NOT line_exists( lt_bapiret[ type = 'E' ] ) .

        UPDATE zsdt0053 SET vbeln = <saida>-vbeln
          WHERE nro_sol_ov = input-nro_sol_ov.

        COMMIT WORK AND WAIT.

      ENDIF.

    ENDLOOP.

    me->exibe_msg( lt_bapiret ).

    me->refresh( ).


  ENDMETHOD.
  METHOD exibe_msg.

    DATA: l_lines TYPE i.

    DESCRIBE TABLE it_ret LINES l_lines.

    IF l_lines <= 1 OR sy-batch = 'X'.

      LOOP AT it_ret ASSIGNING FIELD-SYMBOL(<fs_ret2>).

        MESSAGE ID <fs_ret2>-id
              TYPE 'S'
            NUMBER <fs_ret2>-number
              WITH <fs_ret2>-message_v1
                   <fs_ret2>-message_v2
                   <fs_ret2>-message_v3
                   <fs_ret2>-message_v4 DISPLAY LIKE <fs_ret2>-type.

      ENDLOOP.

    ELSE.

      CALL FUNCTION 'MESSAGES_INITIALIZE'.

      LOOP AT it_ret ASSIGNING <fs_ret2>.

        IF <fs_ret2>-id IS INITIAL.

          <fs_ret2>-id = 'DS'. "<-classe padrao abap
          <fs_ret2>-number = '016'.
          <fs_ret2>-message_v1 = <fs_ret2>-message.

        ENDIF.

        CALL FUNCTION 'MESSAGE_STORE'
          EXPORTING
            arbgb                  = <fs_ret2>-id
            "EXCEPTION_IF_NOT_ACTIVE  = 'X'
            msgty                  = <fs_ret2>-type
            msgv1                  = <fs_ret2>-message_v1
            msgv2                  = <fs_ret2>-message_v2
            msgv3                  = <fs_ret2>-message_v3
            msgv4                  = <fs_ret2>-message_v4
            txtnr                  = <fs_ret2>-number
            "ZEILE                    = ' '
            "IMPORTING
            "ACT_SEVERITY             =
            "MAX_SEVERITY             =
          EXCEPTIONS
            message_type_not_valid = 1
            not_active             = 2
            OTHERS                 = 3.     "#EC CI_SUBRC

      ENDLOOP.

      CALL FUNCTION 'MESSAGES_STOP'
        EXCEPTIONS
          a_message = 1
          e_message = 2
          i_message = 3
          w_message = 4
          OTHERS    = 5.     "#EC CI_SUBRC

      CALL FUNCTION 'MESSAGES_SHOW'
        EXPORTING
          "CORRECTIONS_OPTION          = ' '
          "CORRECTIONS_FUNC_TEXT       = ' '
          "LINE_FROM                   = ' '
          "LINE_TO                     = ' '
          "OBJECT                      = ' '
          "SEND_IF_ONE                 = ' '
          batch_list_type     = 'B'
          show_linno          = ' '
          show_linno_text     = 'X'
          show_linno_text_len = '3'
          i_use_grid          = ' '
          i_amodal_window     = ' '
          "MSG_SELECT_FUNC             = ' '
          "MSG_SELECT_FUNC_TEXT        = ' '
          "IMPORTING
          "CORRECTIONS_WANTED          =
          "E_EXIT_COMMAND              =
          "MSG_SELECTED                =
        EXCEPTIONS
          inconsistent_range  = 1
          no_messages         = 2
          OTHERS              = 3.     "#EC CI_SUBRC

    ENDIF.

  ENDMETHOD.
  METHOD funcao.

    IF input IS NOT INITIAL.
      return  = 'ZSDMF001_GERA_OV_SOL_FORM'.
    ELSE.
      return = 'ZSDMF001_GERA_OV_SOLICITACAO'.
    ENDIF.

  ENDMETHOD.

  METHOD get_tvak.
    SELECT SINGLE bezob FROM tvak INTO return WHERE auart = input.
  ENDMETHOD.

  METHOD get_vbrk.
    SELECT SINGLE fkart FROM vbrk INTO return WHERE vbeln = input AND fksto EQ abap_false.
    CHECK sy-subrc IS NOT INITIAL.
    ADD sy-subrc TO me->at_erro.
    MESSAGE 'Documento de não existe!' TYPE 'S' DISPLAY LIKE 'E'.
  ENDMETHOD.

  METHOD get_tvcpa.
    SELECT COUNT(*) FROM tvcpa WHERE fkarv = input AND auarn EQ input1.
    return = sy-subrc.
    CHECK return IS NOT INITIAL.
    ADD sy-subrc TO me->at_erro.
    MESSAGE 'Documento de Fatura Inválido!' TYPE 'S' DISPLAY LIKE 'E'.
  ENDMETHOD.

  METHOD get_documento.

    CHECK me->get_tvak( input ) IS NOT INITIAL.
    CHECK me->set_documento( ) IS INITIAL.

    me->get_vbeln( ).
    me->get_tvcpa( input = me->get_vbrk( at_documento ) input1 = input ).

  ENDMETHOD.

  METHOD set_documento.

    DATA: lv_return.

    FREE tl_fields.

    APPEND VALUE #(
                    tabname    = 'VBFA'
                    fieldname  = 'VBELN'
                    field_obl  = 'X'
                    fieldtext  = 'Informar Doc.Fatura'
                   ) TO tl_fields.

    CALL FUNCTION 'POPUP_GET_VALUES_USER_HELP'
      EXPORTING
        popup_title     = 'Selecionar Documento de fatura'
        programname     = sy-cprog
      IMPORTING
        returncode      = return
      TABLES
        fields          = tl_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    CHECK return IS NOT INITIAL.
    ADD 4 TO me->at_erro.

  ENDMETHOD.

  METHOD get_vbeln.
    TRY .
        at_documento = tl_fields[ 1 ]-value.
      CATCH cx_sy_itab_line_not_found.
        CLEAR at_documento.
    ENDTRY.
  ENDMETHOD.

  METHOD desconto_abs.

    DATA desc_abs TYPE kbetr.

    DATA(simulador_) = simulador[ 1 ].

    SELECT SINGLE *
     FROM zsdt0053
     INTO @DATA(w_0053)
       WHERE nro_sol_ov EQ @simulador_-nro_sol_ov
         AND posnr EQ @simulador_-posnr.

    IF w_0053-desc_absoluto IS NOT INITIAL.

      CHECK w_0053-vbeln IS NOT INITIAL.

      SELECT SINGLE *
        FROM vbap
          INTO @DATA(w_vbap)
       WHERE vbeln EQ @w_0053-vbeln.

      CALL METHOD zsdmf001_atuali_ov_simulador
        EXPORTING
          i_vbeln     = w_vbap-vbeln
          i_posnr     = w_vbap-posnr
          i_matnr     = w_vbap-matnr
          i_diferenca = CONV #( w_0053-desc_absoluto ).

    ENDIF.

  ENDMETHOD.

  METHOD diferenca.

    DATA: soma_vbap   TYPE netwr_ap,
          soma_53     TYPE netwr_ap,
          soma_59     TYPE netwr_ap,
          sub_53_vbap TYPE netwr_ap,
          imposto     TYPE netwr_ap,
          diferenca   TYPE netwr_ap,
          p_2         TYPE p DECIMALS 2.

    TRY .
        DATA(simulador_) = simulador[ 1 ].
      CATCH cx_sy_itab_line_not_found.
        EXIT.
    ENDTRY.

    SELECT SINGLE *
     FROM zsdt0053
     INTO @DATA(w_0053)
       WHERE nro_sol_ov EQ @simulador_-nro_sol_ov
         AND posnr EQ @simulador_-posnr.

    CHECK w_0053-vbeln IS NOT INITIAL.

    SELECT SINGLE *
      FROM vbap
     INTO @DATA(wa_vbap)
      WHERE vbeln EQ @w_0053-vbeln.

    ADD wa_vbap-netwr TO soma_vbap.
    ADD wa_vbap-mwsbp TO soma_vbap.

    SELECT SUM( vlrtot )
      FROM zsdt0053
     INTO soma_53
      WHERE nro_sol_ov EQ w_0053-nro_sol_ov
        AND vbeln EQ w_0053-vbeln.

    SELECT *
      FROM zsdt0059
     INTO TABLE @DATA(ti_zsdt0059)
      WHERE nro_sol_ov = @w_0053-nro_sol_ov
        AND field = 'PRECO'
        AND cod_fp IN ( '0011', '0012' ). "// 0011 -> PIS E COFINS, 0012 -> ICMS

    LOOP AT ti_zsdt0059 INTO DATA(wa_0059).

      TRY.
          p_2 = wa_0059-formula2.
        CATCH cx_sy_conversion_no_number INTO DATA(obj_exc).
          DATA(exc_text) = obj_exc->get_text( ).
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH exc_text.
      ENDTRY.

      ADD p_2 TO soma_59.

    ENDLOOP.

*   "// A Diferenca da Subtração da ZSDT0053 com a VBAP
    sub_53_vbap = soma_53 - soma_vbap.

    CHECK sub_53_vbap IS NOT INITIAL.

*   "// Verifica o Imposto da SUB_53_VBAP
    imposto = ( sub_53_vbap * soma_59 ) / 100.

*   "// Remove o imposto da Diferença
    diferenca = sub_53_vbap - imposto.

    CHECK diferenca IS NOT INITIAL.

    CALL METHOD zsdmf001_atuali_ov_simulador
      EXPORTING
        i_vbeln     = wa_vbap-vbeln
        i_posnr     = wa_vbap-posnr
        i_matnr     = wa_vbap-matnr
        i_diferenca = CONV #( diferenca ).

  ENDMETHOD.

  METHOD ajuste_53.

    DATA soma TYPE dmbtr.
    DATA diferenca TYPE dmbtr.

    SELECT *
     FROM zsdt0053
     INTO TABLE @DATA(t_0053)
      FOR ALL ENTRIES IN @simulador
       WHERE nro_sol_ov EQ @simulador-nro_sol_ov
         AND posnr EQ @simulador-posnr.

    CHECK sy-subrc IS INITIAL.

    LOOP AT t_0053 INTO DATA(w_0053).

      SELECT SINGLE *
        FROM vbak
        INTO @DATA(w_vbak)
        WHERE vbeln EQ @w_0053-vbeln.

      SELECT SINGLE *
        FROM vbap
          INTO @DATA(w_vbap)
       WHERE vbeln EQ @w_vbak-vbeln.

      IF sy-subrc IS INITIAL.

        SELECT SINGLE kbetr
          FROM konv
          INTO @DATA(vl_kbetr)
          WHERE knumv EQ @w_vbak-knumv
            AND kposn EQ @w_vbap-posnr
            AND kschl EQ 'RB00'.

        soma = w_vbap-netwr + w_vbap-mwsbp.

        IF soma NE w_0053-vlrtot.

          diferenca = w_0053-vlrtot - soma.
          ADD vl_kbetr TO diferenca.

          CALL METHOD zsdmf001_atuali_ov_simulador
            EXPORTING
              i_vbeln     = w_vbap-vbeln
              i_posnr     = w_vbap-posnr
              i_matnr     = w_vbap-matnr
              i_diferenca = CONV #( diferenca ).
        ENDIF.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.

  METHOD zsdmf001_atuali_ov_simulador.
*   "// criando uma tabela para REMOVER o ("Desconto/Acrescimo")
    DATA(t_desc_abs) = VALUE zsdt0041_t( (
                                          vbeln         = i_vbeln
                                          posnr         = i_posnr
                                          matnr         = i_matnr
                                          desc_absoluto = i_diferenca
                                     ) ).
*   "// função que aplica o desconto/acrescimo na OV
    CALL FUNCTION 'ZSDMF001_ATUALI_OV_SIMULADOR_2'
      TABLES
        ti_itens_ov       = t_desc_abs
      EXCEPTIONS
        ov_nao_encontrada = 1
        OTHERS            = 2.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM block.

  IF cont IS INITIAL.
    CHECK check_tpvenda IS INITIAL.

    PERFORM iniciar_variaves.
    PERFORM selecionar_dados.
    PERFORM organizacao_dados.
    PERFORM imprimir_dados.
  ELSE.
    SHIFT concat LEFT DELETING LEADING ','.
    IF concat IS INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Sem acesso a Organização de Venda!'.
    ELSE.
      IF cont LE 1.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Acesso somente para a Organização de Venda' concat '!'.
      ELSE.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Acesso somente para as Organizações de Venda' concat '!'.
      ENDIF.
    ENDIF.

  ENDIF.
  CLEAR: cont, concat, cont_1, check_tpvenda.

  DEFINE bdc.
    APPEND VALUE #(
        program   = &1
        dynpro    = &2
        dynbegin  = &3
        fnam      = &4
        fval      = &5
                    ) TO it_dta.
  END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selecionar_dados.
  DATA: tl_0053 TYPE TABLE OF zsdt0053,
        tl_0051 TYPE TABLE OF zsdt0051.

  FREE: tg_0051,
        tg_0053,
        tl_0053,
        tg_0327,
        tg_0054,
        tg_0066,
        tg_0045,
        tg_makt,
        tg_bsad,
        tg_objek,
        tg_t001w,
        tg_kna1,
        tg_saida,
        tg_bdc,
        tg_saldo,
        tg_lfa1,
        tg_ponto_coleta,
        tg_terminal,
        tg_0051_form.

  CLEAR: wg_0051,
         wg_0053,
         wg_0054,
         wg_0066,
         wg_0045,
         wg_bsad,
         wg_objek,
         wg_makt,
         wg_t001w,
         wg_kna1,
         wg_saida,
         wg_lfa1,
         wg_ponto_coleta,
         wg_terminal,
         wg_bdc,
         wg_saldo.

  SELECT *
      FROM zsdt0051
      INTO TABLE tg_0051
       WHERE vkorg IN s_vkorg
         AND vtweg IN s_vtweg
         AND spart IN s_spart
         AND kunnr IN s_kunnr
         AND nro_sol_ov IN s_nr_sol
         AND tp_venda IN s_tpven
         AND data_venda IN s_dtven
         AND status NE 'D' . SORT tg_0051 BY nro_sol_ov ASCENDING.

  IF p_gera IS NOT INITIAL.
    tg_0051_form[] = tg_0051[].
    DELETE tg_0051 WHERE status NE 'L'.
  ELSE.
    tg_0051_form[] = tg_0051[].
  ENDIF.

  IF p_venda IS NOT INITIAL.
    IF tg_0051[] IS NOT INITIAL.

      DELETE tg_0051 WHERE inco1 EQ s_inco1-low.

      SELECT *
        FROM lfa1
        INTO TABLE tg_lfa1
         FOR ALL ENTRIES IN tg_0051
          WHERE lifnr EQ tg_0051-correto.

      SELECT *
        FROM zsdt0053
        INTO TABLE tg_0053
         FOR ALL ENTRIES IN tg_0051
          WHERE nro_sol_ov EQ tg_0051-nro_sol_ov
            AND vbeln IN s_ordem
            AND numero_ruc IN s_ruc
            AND matnr IN  p_matnr. " Rubenilson Pereira - 11.02.25 US164018

      DELETE tg_0053 WHERE status EQ 'Y'.
      DELETE tg_0053 WHERE status EQ 'W'.
      DELETE tg_0053 WHERE status EQ 'C'.

      obj_ret->check_del( ).
      IF p_gera IS NOT INITIAL.
        obj_ret->set_0053( obj_ret->at02 ).
      ELSE.
        DELETE tg_0053 WHERE vbeln IS INITIAL.
      ENDIF.


      IF tg_0053[] IS NOT INITIAL.

        SELECT *
          FROM zdoc_exp INTO TABLE tg_zdoc_exp
           FOR ALL ENTRIES IN tg_0053
          WHERE vbeln EQ tg_0053-remessa_exp.

        DATA(lit_0053_aux) = tg_0053[].
        DELETE lit_0053_aux WHERE id_nomeacao_tran IS INITIAL.

        IF lit_0053_aux[] IS NOT INITIAL.
          SELECT *
            FROM znom_transporte INTO TABLE tg_znom_transp
             FOR ALL ENTRIES IN lit_0053_aux
           WHERE id_nomeacao_tran EQ lit_0053_aux-id_nomeacao_tran.
        ENDIF.

        SELECT *
        FROM zsdt0054
        INTO TABLE tg_0054
         FOR ALL ENTRIES IN tg_0053
          WHERE nro_sol_ov EQ tg_0053-nro_sol_ov
            AND posnr      EQ tg_0053-posnr.

        IF sy-subrc IS INITIAL.
          SELECT bukrs belnr augdt augbl
            FROM bsad
            INTO TABLE tg_bsad
             FOR ALL ENTRIES IN tg_0054
             WHERE bukrs IN s_vkorg
               AND belnr EQ tg_0054-adiant.

        ENDIF.

        SELECT *
          FROM makt
          INTO TABLE tg_makt
           FOR ALL ENTRIES IN tg_0053
           WHERE matnr EQ tg_0053-matnr
             AND spras EQ sy-langu.

        SELECT *
          FROM t001w
          INTO TABLE tg_t001w
           FOR ALL ENTRIES IN tg_0053
            WHERE werks EQ tg_0053-werks.

        SELECT *
          FROM kna1
          INTO TABLE tg_kna1
           FOR ALL ENTRIES IN tg_0051
            WHERE kunnr EQ tg_0051-kunnr.

        tl_0053[] = tg_0053[].
        DELETE tl_0053 WHERE kunnr IS INITIAL.
        IF tl_0053[] IS NOT INITIAL.
          SELECT *
            FROM kna1
            APPENDING TABLE tg_kna1
             FOR ALL ENTRIES IN tl_0053
              WHERE kunnr EQ tl_0053-kunnr.
        ENDIF.

        SELECT nfenum docnum
          FROM j_1bnfdoc
          INTO TABLE it_doc
          FOR ALL ENTRIES IN tg_0053
          WHERE docnum EQ tg_0053-docnum_rt.

        SELECT fa~vbelv fa~vbeln
          INTO TABLE it_vbfa_rt
          FROM vbfa AS fa
          INNER JOIN vbrk AS rk ON rk~vbeln EQ fa~vbeln
          FOR ALL ENTRIES IN tg_0053
          WHERE fa~vbelv EQ tg_0053-remessa_exp
            AND fa~vbtyp_n EQ 'M'
            AND fa~vbtyp_v EQ 'J'
            AND rk~fksto   EQ abap_false.

*       US 152645 // MMSILVA - 11-10-2024 - Inicio
        IF s_erdat IS INITIAL.
          SELECT *
            FROM vbak
            INTO TABLE it_vbak
            FOR ALL ENTRIES IN tg_0053
            WHERE vbeln EQ tg_0053-vbeln.
        ELSE.
          IF s_erdat-high = '00000000'.
            s_erdat-high = s_erdat-low.
          ENDIF.
          SELECT *
            FROM vbak
            INTO TABLE it_vbak
            FOR ALL ENTRIES IN tg_0053
            WHERE vbeln EQ tg_0053-vbeln
            AND erdat BETWEEN s_erdat-low AND s_erdat-high.
        ENDIF.

        IF p_gera IS NOT INITIAL.
          DELETE it_vbak WHERE erdat IS INITIAL.
        ENDIF.
*       US 152645 // MMSILVA - 11-10-2024 - Fim

        IF it_vbfa_rt IS NOT INITIAL.

          LOOP AT it_vbfa_rt ASSIGNING FIELD-SYMBOL(<f_vbfa>).
            <f_vbfa>-refkey = <f_vbfa>-vbeln.
          ENDLOOP.

          SELECT docnum refkey
            FROM j_1bnflin
            INTO TABLE it_lin
            FOR ALL ENTRIES IN it_vbfa_rt
            WHERE refkey EQ it_vbfa_rt-refkey.

          IF it_lin IS NOT INITIAL.
            SELECT nfenum docnum
              FROM j_1bnfdoc
              APPENDING TABLE it_doc
              FOR ALL ENTRIES IN it_lin
              WHERE docnum EQ it_lin-docnum.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    IF tg_0051[] IS NOT INITIAL.
      SELECT *
          FROM lfa1
          INTO TABLE tg_lfa1
           FOR ALL ENTRIES IN tg_0051
            WHERE lifnr EQ tg_0051-correto.
    ENDIF.

    LOOP AT tg_0051_form INTO wg_0051.
      wg_objek-objek = wg_0051-nro_sol_ov.
      APPEND wg_objek TO tg_objek.
      CLEAR: wg_objek.
    ENDLOOP.

    IF tg_objek[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0045
        INTO TABLE tg_0045
         FOR ALL ENTRIES IN tg_objek
         WHERE objek       EQ tg_objek-objek
           AND objecttable EQ 'ZSDT0051'.

      SELECT *
        FROM zsdt0066
        INTO TABLE tg_0066
         FOR ALL ENTRIES IN tg_0051_form
         WHERE nro_sol_ov EQ tg_0051_form-nro_sol_ov
           AND vbeln      IN s_ordem
           AND inco1      IN s_inco1
           AND status     EQ 'L'
           AND matnr IN  p_matnr. " Rubenilson Pereira - 11.02.25 US164018

      IF p_gera IS NOT INITIAL.
        DELETE tg_0066 WHERE vbeln IS NOT INITIAL.
      ELSE.
        DELETE tg_0066 WHERE vbeln IS INITIAL.
      ENDIF.

      IF tg_0066[] IS NOT INITIAL.

        SELECT *
          FROM makt
          APPENDING TABLE tg_makt
           FOR ALL ENTRIES IN tg_0066
           WHERE matnr EQ tg_0066-matnr
             AND spras EQ sy-langu.

        SELECT *
          FROM t001w
          APPENDING TABLE tg_t001w
           FOR ALL ENTRIES IN tg_0066
            WHERE werks EQ tg_0066-werks.

        SELECT *
          FROM kna1
          APPENDING TABLE tg_kna1
           FOR ALL ENTRIES IN tg_0066
            WHERE kunnr EQ tg_0066-kunnr.

        SELECT *
          FROM kna1
          APPENDING TABLE tg_local_entrega
           FOR ALL ENTRIES IN tg_0066
            WHERE kunnr EQ tg_0066-lentrega.

        SORT tg_local_entrega BY kunnr.


        LOOP AT tg_0066 INTO wg_0066.
          CLEAR vl_lenght.
          vl_lenght = strlen( wg_0066-ponto_c ).

          IF vl_lenght < 10.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wg_0066-ponto_c
              IMPORTING
                output = wg_0066-ponto_c.
          ENDIF.

          CLEAR vl_lenght.
          vl_lenght = strlen( wg_0066-terminal ).

          IF  vl_lenght < 10.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wg_0066-terminal
              IMPORTING
                output = wg_0066-terminal.
          ENDIF.

          MODIFY tg_0066 FROM wg_0066 INDEX sy-tabix.
          CLEAR wg_0066.
        ENDLOOP.

        SELECT *
          FROM lfa1
          INTO TABLE tg_ponto_coleta
          FOR ALL ENTRIES IN tg_0066
          WHERE lifnr EQ tg_0066-ponto_c
          AND   land1 EQ 'BR'.

        SELECT *
          FROM lfa1
          INTO TABLE tg_terminal
          FOR ALL ENTRIES IN tg_0066
          WHERE lifnr EQ tg_0066-terminal
          AND   land1 EQ 'BR'.


      ENDIF.
    ENDIF.
  ENDIF.

*-CS2023000189-26.04.2023-#108710-JT-inicio
  IF tg_0053[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0327
      INTO TABLE tg_0327
       FOR ALL ENTRIES IN tg_0053
     WHERE nro_sol_ov  = tg_0053-nro_sol_ov
       AND posnr       = tg_0053-posnr.
  ENDIF.

  SORT tg_0327 BY nro_sol_ov posnr seq DESCENDING.
  DELETE ADJACENT DUPLICATES FROM tg_0327
                   COMPARING nro_sol_ov posnr.
*-CS2023000189-26.04.2023-#108710-JT-fim

*-CS2023000189-26.04.2023-#108710-JT-inicio
  IF tg_0066[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0327
      INTO TABLE tg_0327
       FOR ALL ENTRIES IN tg_0066
     WHERE nro_sol_ov  = tg_0066-nro_sol_ov
       AND posnr       = tg_0066-posnr.
  ENDIF.

  SORT tg_0327 BY nro_sol_ov posnr seq DESCENDING.
  DELETE ADJACENT DUPLICATES FROM tg_0327
                   COMPARING nro_sol_ov posnr.
*-CS2023000189-26.04.2023-#108710-JT-fim


ENDFORM.                    " SELECIONAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM organizacao_dados.
  SORT: tg_0051  BY nro_sol_ov,
        tg_makt  BY matnr,
        tg_t001w BY werks,
        tg_kna1  BY kunnr,
        tg_0054  BY nro_sol_ov posnr,
        tg_saldo BY nro_sol_ov vbeln,
        tg_lfa1  BY lifnr.

  PERFORM busca_saldo TABLES tg_0053.

*** Stefanini - IR252396 - 12/08/2025 - LAZAROSR - Início de Alteração
  PERFORM busca_saldo_pedido TABLES tg_0053.
*** Stefanini - IR252396 - 12/08/2025 - LAZAROSR - Fim de Alteração

  LOOP AT tg_0053 INTO wg_0053.

    TRY  .
        wg_0051 = tg_0051[ nro_sol_ov = wg_0053-nro_sol_ov ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    IF wg_0051-param_espec EQ 'M'
    AND wg_0053-status_itm IS INITIAL.
      CONTINUE.
    ENDIF.

    TRY  .
        wg_lfa1 = tg_lfa1[ lifnr = wg_0051-correto ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    READ TABLE tg_makt INTO wg_makt
       WITH KEY matnr = wg_0053-matnr
                BINARY SEARCH.

    READ TABLE tg_t001w INTO wg_t001w
        WITH KEY werks = wg_0053-werks
                 BINARY SEARCH.

    IF wg_0053-kunnr IS NOT INITIAL.
      wg_0051-kunnr = wg_0053-kunnr.
    ENDIF.

    READ TABLE tg_kna1 INTO wg_kna1
        WITH KEY kunnr = wg_0051-kunnr
                 BINARY SEARCH.

    CLEAR: wg_saldo.
    IF p_gera IS INITIAL.
      READ TABLE tg_saldo INTO wg_saldo
            WITH KEY nro_sol_ov = wg_0053-nro_sol_ov
                     vbeln      = wg_0053-vbeln.

*** Stefanini - IR252396 - 12/08/2025 - LAZAROSR - Início de Alteração
      READ TABLE tg_saldo_pedido INTO wg_saldo_pedido
                                 WITH KEY ebeln = wg_0053-vbeln
                                          menge = wg_0053-zmeng
                                                  BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        wg_saldo-saldo = wg_saldo_pedido-saldo.
        wg_saldo-total = wg_saldo_pedido-total.

      ENDIF.
*** Stefanini - IR252396 - 12/08/2025 - LAZAROSR - Fim de Alteração

    ENDIF.


    MOVE-CORRESPONDING: wg_0051  TO wg_saida,
                        wg_0053  TO wg_saida.

    wg_saida-auart = COND #( WHEN wg_0053-auart IS NOT INITIAL THEN wg_0053-auart ELSE wg_0051-auart ).

*    WSB
    TRY .
        wg_saida-nfenum_rt = it_doc[ docnum =  wg_0053-docnum_rt ]-nfenum.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    TRY .
        wg_saida-doc_fat = it_vbfa_rt[ vbelv = wg_0053-remessa_exp ]-vbeln.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    TRY .
        wg_saida-nfenum_exp = it_doc[ docnum =  it_lin[ refkey = it_vbfa_rt[ vbelv = wg_0053-remessa_exp ]-refkey ]-docnum ]-nfenum.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    READ TABLE tg_0054 INTO wg_0054
          WITH KEY nro_sol_ov = wg_0053-nro_sol_ov
                        posnr = wg_0053-posnr
                        BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      LOOP AT tg_0054 INTO wg_0054
         WHERE nro_sol_ov EQ wg_0053-nro_sol_ov
           AND posnr EQ wg_0053-posnr
           AND adiant IS INITIAL.

      ENDLOOP.
      IF sy-subrc IS INITIAL.
        wg_saida-adiant = icon_warning.

      ELSE.
        wg_saida-adiant = icon_status_best.
      ENDIF.

    ENDIF.
**********************************************************************
* 102756 CS2023000064 Criar opção a anexo de contrato nas linhas da ZSDT0066 pelo numero da solicitação - PSA
**********************************************************************
    PERFORM f_verifiva_icone_arquivo.


    IF wg_0053-id_nomeacao_tran IS NOT INITIAL.
      READ TABLE tg_znom_transp INTO DATA(lwa_znom_transp) WITH KEY id_nomeacao_tran = wg_0053-id_nomeacao_tran.

      IF sy-subrc IS INITIAL.
        wg_saida-ds_nome_transpor = lwa_znom_transp-ds_nome_transpor.
      ENDIF.
    ENDIF.

    MOVE: wg_t001w-name1 TO wg_saida-w_name1,
          wg_0053-werks  TO wg_saida-werks,
          wg_kna1-kunnr  TO wg_saida-kunnr,
          wg_kna1-name1  TO wg_saida-name1,
          wg_kna1-ort01  TO wg_saida-ort01,
          wg_kna1-regio  TO wg_saida-regio,
*          wg_kna1-stcd1  TO wg_saida-stcd1,
          wg_makt-maktx  TO wg_saida-maktx,
          wg_saldo-total TO wg_saida-qtd_remessa,
          wg_saldo-saldo TO wg_saida-saldo_ov,
          wg_lfa1-lifnr  TO wg_saida-correto,
          wg_lfa1-name1  TO wg_saida-name1_c.

* Ini - rjf - ajuste da coluna cpf/cnpj - cs2024000002 incluir o cnpj do cliente no alv da zsdt0066
    IF wg_kna1-stkzn IS INITIAL.
      MOVE wg_kna1-stcd1  TO wg_saida-stcd1.
    ELSE.
      MOVE wg_kna1-stcd2  TO wg_saida-stcd1.
    ENDIF.
* Fim - rjf - ajuste da coluna cpf/cnpj - cs2024000002 incluir o cnpj do cliente no alv da zsdt0066

    MOVE: wg_0053-id_export TO wg_saida-id_export. "84616 - CS2022000665 - Gravar ID_EXport



*    IF WG_0051-STATUS EQ 'L'
*    AND WG_0053-VBELN IS INITIAL.
*      WG_SAIDA-STATUS = ICON_MESSAGE_WARNING.
*    ELSEIF WG_0051-STATUS EQ 'L'
*     AND WG_0053-VBELN IS NOT INITIAL.
*      WG_SAIDA-STATUS = ICON_CHECKED.
**     ELSEIF TL_0053-STATUS EQ 'B'.
**      TG_ITENS-STATUS = ICON_LOCKED.
**
**    ELSEIF TL_0053-STATUS EQ 'E'.
**      TG_ITENS-STATUS = ICON_COMPLETE.
*    ENDIF.

    IF wg_0053-vbeln IS NOT INITIAL
    AND ( wg_0053-status IS INITIAL
      OR wg_0053-status EQ 'D' ).
      wg_saida-status = icon_checked.

    ELSEIF wg_0053-vbeln IS INITIAL
      AND wg_0053-status IS INITIAL.
      wg_saida-status = icon_message_warning.

    ELSEIF wg_0053-status EQ 'B'.
      READ TABLE tg_0054 TRANSPORTING NO FIELDS
       WITH KEY nro_sol_ov = wg_0053-nro_sol_ov
                posnr      = wg_0053-posnr.
      IF sy-subrc IS INITIAL.
        wg_saida-status = icon_cost_components.
      ELSE.
        wg_saida-status = icon_locked.
      ENDIF.


    ELSEIF wg_0053-status EQ 'E'.
      wg_saida-status = icon_complete.
*    ELSEIF WG_0053-STATUS EQ 'D'.
*      WG_SAIDA-STATUS = icon_checked.
    ENDIF.

    IF wg_0053-contrato IS NOT INITIAL.
      wg_saida-contrato = icon_wri.
    ELSE.
      CLEAR: wg_saida-contrato.
    ENDIF.

    IF wg_saida-remessa_exp IS NOT INITIAL.
      READ TABLE tg_zdoc_exp WITH KEY vbeln = wg_saida-remessa_exp.
      IF sy-subrc = 0.
        wg_saida-id_due             = tg_zdoc_exp-id_due.
        wg_saida-numero_due         = tg_zdoc_exp-numero_due.
        wg_saida-id_registro_expo   = tg_zdoc_exp-id_registro_expo.
        wg_saida-nr_registro_expo   = tg_zdoc_exp-nr_registro_expo.
      ENDIF.
    ENDIF.

    wg_saida-instrucao  = wg_0053-instrucao.

*-CS2023000189-26.04.2023-#108710-JT-inicio
    READ TABLE tg_0327 INTO wg_0327 WITH KEY nro_sol_ov  = wg_0053-nro_sol_ov
                                             posnr       = wg_0053-posnr.
    IF sy-subrc <> 0.
      wg_saida-status_trace = icon_dummy.

    ELSEIF wg_0327-tipo_msg = 'E'.
      wg_saida-status_trace = icon_alert. "ICON_FAILURE
    ELSEIF wg_0327-tipo_msg = 'S'.
      wg_saida-status_trace = icon_checked.
    ELSE.
      wg_saida-status_trace = icon_dummy.
    ENDIF.
*-CS2023000189-26.04.2023-#108710-JT-fim

*   US 152645 // MMSILVA - 11-10-2024
    READ TABLE it_vbak INTO wg_vbak WITH KEY vbeln = wg_0053-vbeln.

    IF s_erdat-low <> '00000000'.
      IF s_erdat-high = '00000000'.
        s_erdat-high = s_erdat-low.
      ENDIF.
      IF wg_vbak-vbeln IS NOT INITIAL.
        wg_saida-erdat = wg_vbak-erdat.
      ELSE.
        CONTINUE.
      ENDIF.
    ELSE.
      wg_saida-erdat = wg_vbak-erdat.
    ENDIF.
*   US 152645 // MMSILVA - 11-10-2024

    APPEND wg_saida TO tg_saida.
    CLEAR: wg_0051, wg_saida, wg_0053, wg_0054, wg_makt, wg_t001w, wg_kna1, wg_saldo, wg_lfa1, wg_0066, wg_vbak.
  ENDLOOP.

  IF p_gera IS NOT INITIAL.
    DELETE tg_saida WHERE nfenum_exp IS NOT INITIAL.
  ENDIF.

  PERFORM busca_saldo_form TABLES tg_0066.
  SORT: tg_0051_form BY nro_sol_ov.

*------------------------------------------------------------
  LOOP AT tg_0066 INTO wg_0066.

    wg_saida-safra = wg_0066-charg. "102915 CS2023000054 INCLUIR COLUNA "SAFRA" NA TRANSAÇÃO ZSDT0066 - SCABANA

    READ TABLE tg_0051_form INTO wg_0051
      WITH KEY nro_sol_ov = wg_0066-nro_sol_ov
                    BINARY SEARCH.


    READ TABLE tg_lfa1 INTO wg_lfa1
       WITH KEY lifnr = wg_0051-correto
                BINARY SEARCH.

    READ TABLE tg_makt INTO wg_makt
       WITH KEY matnr = wg_0066-matnr
                BINARY SEARCH.

    READ TABLE tg_t001w INTO wg_t001w
        WITH KEY werks = wg_0066-werks
                 BINARY SEARCH.

    IF wg_0066-kunnr IS NOT INITIAL.
      wg_0051-kunnr = wg_0066-kunnr.
    ENDIF.

    READ TABLE tg_local_entrega INTO DATA(wa_local_entrega)
     WITH KEY kunnr = wg_0066-lentrega
        BINARY SEARCH.

    IF sy-subrc IS NOT INITIAL.
      CLEAR: wa_local_entrega.
    ENDIF.

    READ TABLE tg_kna1 INTO wg_kna1
        WITH KEY kunnr = wg_0051-kunnr
                 BINARY SEARCH.

    CLEAR: wg_saldo.
    IF p_gera IS INITIAL.
      READ TABLE tg_saldo INTO wg_saldo
            WITH KEY nro_sol_ov = wg_0066-nro_sol_ov
                     vbeln      = wg_0066-vbeln
                          BINARY SEARCH.
    ENDIF.


    MOVE-CORRESPONDING: wg_0051  TO wg_saida,
                        wg_0066  TO wg_saida.
*                        WG_MAKT  TO WG_SAIDA,
*                        WG_T001W TO WG_SAIDA,
*                        WG_KNA1 TO WG_SAIDA.

*-CS2023000189-26.04.2023-#108710-JT-inicio
    READ TABLE tg_0327 INTO wg_0327 WITH KEY nro_sol_ov  = wg_0066-nro_sol_ov
                                             posnr       = wg_0066-posnr.
    IF sy-subrc <> 0.
      wg_saida-status_trace = icon_dummy.

    ELSEIF wg_0327-tipo_msg = 'E'.
      wg_saida-status_trace = icon_alert. "ICON_FAILURE
    ELSEIF wg_0327-tipo_msg = 'S'.
      wg_saida-status_trace = icon_checked.
    ELSE.
      wg_saida-status_trace = icon_dummy.
    ENDIF.
*-CS2023000189-26.04.2023-#108710-JT-fim

    wg_saida-vtweg = '10'.

    " 05.07.2022 - RAMON - 76636 ->

    wg_saida-auart = wg_0066-auart.


*    IF wg_0066-industrializacao = 'S'.
*      wg_saida-auart = wg_0066-auart.
*    ELSE.
*
*      IF ( wg_0066-aviso IS INITIAL ) AND ( wg_0066-dco IS INITIAL ).
*
*        " 18.07.2022 - RAMON - 83453 --->
*
*        wg_saida-auart = wg_0066-auart.
*
*        "wg_saida-auart = 'ZRFL'.
*
*        " 18.07.2022 - RAMON - 83453 ---<
*
*
*      ELSE.
*        wg_saida-auart = 'ZRDC'.
*      ENDIF.
*
*    ENDIF.
*    IF ( wg_0066-aviso IS INITIAL ) AND ( wg_0066-dco IS INITIAL ).
*      wg_saida-auart = 'ZRFL'.
*    ELSE.
*      wg_saida-auart = 'ZRDC'.
*    ENDIF.
    " 05.07.2022 - RAMON - 76636 -<

    READ TABLE tg_0054 INTO wg_0054
          WITH KEY nro_sol_ov = wg_0066-nro_sol_ov
                        posnr = wg_0066-posnr
                        BINARY SEARCH.

    IF sy-subrc IS INITIAL.
*      LOOP AT tg_0054 INTO wg_0054
*         WHERE nro_sol_ov EQ wg_0066-nro_sol_ov
*           AND posnr EQ wg_0053-posnr
*           AND adiant IS INITIAL.
*
*      ENDLOOP.
*      IF sy-subrc IS INITIAL.
*        wg_saida-adiant = icon_warning.
*
*      ELSE.
      wg_saida-adiant = icon_status_best.
*      ENDIF.

    ENDIF.

    MOVE: wg_t001w-name1 TO wg_saida-w_name1,
          wg_0066-werks  TO wg_saida-werks,
          wg_kna1-kunnr  TO wg_saida-kunnr,
          wg_kna1-name1  TO wg_saida-name1,
          wg_kna1-ort01  TO wg_saida-ort01,
          wg_kna1-regio  TO wg_saida-regio,
          wg_makt-maktx  TO wg_saida-maktx,
          wg_lfa1-lifnr  TO wg_saida-correto,
          wg_0066-waerk  TO wg_saida-waerk,
          wg_lfa1-name1  TO wg_saida-name1_c,
          wg_saldo-total TO wg_saida-qtd_remessa,
          wg_saldo-saldo TO wg_saida-saldo_ov,
          wa_local_entrega-kunnr TO wg_saida-id_local_descarga,
          wa_local_entrega-name1 TO wg_saida-ds_local_descarga.
*          wg_saldo-total TO wg_saida-qtd_remessa,
*          WG_0066-ZMENG TO WG_SAIDA-SALDO_OV.


    IF wg_0066-vbeln IS NOT INITIAL
    AND ( wg_0066-status_form IS INITIAL
      OR wg_0066-status_form EQ 'D' )
    AND wg_0066-status EQ 'L'.
      wg_saida-status = icon_checked.

    ELSEIF wg_0066-vbeln IS INITIAL
      AND wg_0066-status_form IS INITIAL
      AND wg_0066-status EQ 'L'.
      wg_saida-status = icon_message_warning.

    ELSEIF wg_0066-status_form EQ 'B'.
      READ TABLE tg_0054 TRANSPORTING NO FIELDS
       WITH KEY nro_sol_ov = wg_0066-nro_sol_ov
                posnr      = wg_0066-posnr.
      IF sy-subrc IS INITIAL.
        wg_saida-status = icon_cost_components.
      ELSE.
        wg_saida-status = icon_locked.
      ENDIF.


    ELSEIF wg_0066-status_form EQ 'E'.
      wg_saida-status = icon_complete.
*    ELSEIF WG_0053-STATUS EQ 'D'.
*      WG_SAIDA-STATUS = icon_checked.
    ENDIF.

    wg_saida-ponto_coleta  = wg_0066-ponto_c.

    READ TABLE tg_ponto_coleta INTO wg_ponto_coleta
                              WITH KEY lifnr = wg_0066-ponto_c.
    IF sy-subrc EQ 0.
      wg_saida-name_pc = wg_ponto_coleta-name1.
    ENDIF.

    wg_saida-terminal_z1  = wg_0066-terminal.

    READ TABLE tg_terminal INTO wg_terminal
                              WITH KEY lifnr = wg_0066-terminal.
    IF sy-subrc EQ 0.
      wg_saida-name_terminal = wg_terminal-name1.
    ENDIF.




*    IF WG_0066-STATUS EQ 'L'
*    AND WG_0066-VBELN IS INITIAL.
*      WG_SAIDA-STATUS = ICON_MESSAGE_WARNING.
*    ELSEIF WG_0066-STATUS EQ 'L'
*     AND WG_0066-VBELN IS NOT INITIAL.
*      WG_SAIDA-STATUS = ICON_CHECKED.
*    ELSEIF WG_0066-STATUS EQ 'B'.
*      WG_SAIDA-STATUS = ICON_LOCKED.
***
*    ELSEIF WG_0066-STATUS EQ 'E'.
*      WG_SAIDA-STATUS = ICON_COMPLETE.
*    ENDIF.

*    IF wg_0066-vbeln IS NOT INITIAL
*    AND ( wg_0066-status IS INITIAL
*      OR  wg_0066-status EQ 'D' ).
*      wg_saida-status = icon_checked.
*
*    ELSEIF wg_0066-vbeln IS INITIAL
*      AND wg_0066-status IS INITIAL.
*      wg_saida-status = icon_message_warning.
*
*    ELSEIF wg_0066-status EQ 'B'.
*      READ TABLE tg_0054 TRANSPORTING NO FIELDS
*       WITH KEY nro_sol_ov = wg_0053-nro_sol_ov
*                posnr      = wg_0053-posnr.
*      IF sy-subrc IS INITIAL.
*        wg_saida-status = icon_cost_components.
*      ELSE.
*        wg_saida-status = icon_locked.
*      ENDIF.
*
*
*    ELSEIF wg_0053-status EQ 'E'.
*      wg_saida-status = icon_complete.
**    ELSEIF WG_0053-STATUS EQ 'D'.
**      WG_SAIDA-STATUS = icon_checked.
*    ENDIF.

*    IF wg_0053-contrato IS NOT INITIAL.
*      wg_saida-contrato = icon_wri.
*    ELSE.
    CLEAR: wg_saida-contrato.
*    ENDIF.


    APPEND wg_saida TO tg_saida.



    CLEAR: wg_0051, wg_saida, wg_0053, wg_0054, wg_makt, wg_t001w, wg_kna1, wg_saldo, wg_lfa1, wg_ponto_coleta, wg_0066, wg_vbak.
  ENDLOOP.

  obj_ret->set_saida( input  = obj_ret->at02
                      input1 = p_gera ).

ENDFORM.                    " ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprimir_dados.
  DATA: layout          TYPE slis_layout_alv.
  PERFORM definir_eventos.

  PERFORM montar_layout USING 'TG_SAIDA'.
  layout-box_fieldname = 'MARK'.
  layout-box_tabname  = 'TG_SAIDA'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = v_report
*     I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
      it_fieldcat        = estrutura[]
*     IT_SORT            = T_SORT[]
      i_save             = 'A'
      it_events          = events
      is_layout          = layout
      is_print           = t_print
    TABLES
      t_outtab           = tg_saida.

ENDFORM.                    " IMPRIMIR_DADOS
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
                                 slis_ev_user_command  'XUSER_COMMAND',
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
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout USING p_alv.
  REFRESH: estrutura.
  IF p_alv EQ 'TG_SAIDA'.
    PERFORM montar_estrutura USING:
  1   ''           ''                 'TG_SAIDA' 'STATUS_TRACE' 'Trace Cotton'        '10',  "*-CS2023000189-26.04.2023-#108710-JT
  1   ''           ''                 'TG_SAIDA' 'STATUS'      'Status'               '',
  2   'ZSDT0051'   'VKORG'            'TG_SAIDA' 'VKORG'       ''                     '',
  3   'ZSDT0051'   'VTWEG'            'TG_SAIDA' 'VTWEG'       ''                     '',
  4   'ZSDT0051'   'SPART'            'TG_SAIDA' 'SPART'       ''                     '',
  5   'ZSDT0051'   'VKBUR'            'TG_SAIDA' 'VKBUR'       ''                     '',
  6   'ZSDT0051'   'VKGRP'            'TG_SAIDA' 'VKGRP'       ''                     '',
  7   ''           'ANEXOS'           'TG_SAIDA' 'ANEXOS'      'Anexos'               '10',
  7   'ZSDT0051'   'NRO_SOL_OV'       'TG_SAIDA' 'NRO_SOL_OV'  ''                     '',
  7   'ZSDT0051'   'POSNR'            'TG_SAIDA' 'POSNR'       'Posição'              '',   "*-CS2023000189-26.04.2023-#108710-JT
  8   'ZSDT0051'   'TP_VENDA'         'TG_SAIDA' 'TP_VENDA'    ''                     '',
  9   'ZSDT0051'   'AUART'            'TG_SAIDA' 'AUART'       ''                     '',
  10  'ZSDT0053'   'CONTRATO'         'TG_SAIDA' 'CONTRATO'    ''                     '',
  11  'ZSDT0053'   'DOCNUM_RT'        'TG_SAIDA' 'DOCNUM_RT'   'Retorno'              '',
  12  'J_1BNFDOC'  'NFENUM'           'TG_SAIDA' 'NFENUM_RT'   'NFe Retorno'          '',
  13  'ZSDT0053'   'VBELN'            'TG_SAIDA' 'VBELN'       ''                     '',
  14  'ZSDT0053'   'REMESSA_EXP'      'TG_SAIDA' 'REMESSA_EXP' 'Remessa'              '',
  15  'VBRK'       'VBELN'            'TG_SAIDA' 'DOC_FAT'     'Doc. Fatura'          '',
  16  'J_1BNFDOC'  'NFENUM'           'TG_SAIDA' 'NFENUM_EXP'  'NFe'                  '',
  17  ''           ''                 'TG_SAIDA' 'ADIANT'      'Adiantamento'         '',
  18  'ZSDT0051'   'KUNNR'            'TG_SAIDA' 'KUNNR'       ''                     '',
  19  'KNA1'       'STCD1'            'TG_SAIDA' 'STCD1'       'CNPJ/CPF'             '',
  19  'KNA1'       'NAME1'            'TG_SAIDA' 'NAME1'       'Nome Emissor'         '',
  20  'KNA1'       'ORT01'            'TG_SAIDA' 'ORT01'       'Município Emissor'    '',
  21  'KNA1'       'REGIO'            'TG_SAIDA' 'REGIO'       'UF Emissor'           '',
  22  'ZSDT0053'   'MATNR'            'TG_SAIDA' 'MATNR'       ''                     '',
  23  'MAKT'       'MAKTX'            'TG_SAIDA' 'MAKTX'       ''                     '',
  24  'ZSDT0053'   'WERKS'            'TG_SAIDA' 'WERKS'       'Centro Forn.'         '',
  25  'T001W'      'NAME1'            'TG_SAIDA' 'W_NAME1'     'Fornecedor'           '',
  26  'ZSDT0053'   'ZMENG'            'TG_SAIDA' 'ZMENG'       ''                     '',
  27  'ZSDT0053'   'ZMENG'            'TG_SAIDA' 'QTD_REMESSA' 'Qtd.Remessa'          '',
  28  'ZSDT0053'   'ZMENG'            'TG_SAIDA' 'SALDO_OV'    'Sdo.Solicitação'      '',
  29  'ZSDT0053'   'ZIEME'            'TG_SAIDA' 'ZIEME'       ''                     '',
  30  'ZSDT0053'   'DMBTR'            'TG_SAIDA' 'DMBTR'       'Preço'                '',
  31  'ZSDT0053'   'VLRTOT'           'TG_SAIDA' 'VLRTOT'      'Valor Total'          '',
  32  'ZSDT0051'   'WAERK'            'TG_SAIDA' 'WAERK'       ''                     '',
  33  'ZSDT0051'   'INCO1'            'TG_SAIDA' 'INCO1'       'Incoterms'            '',
  34  'ZSDT0051'   'BSTKD'            'TG_SAIDA' 'BSTKD'       ''                     '',
  35  'ZSDT0051'   'CORRETO'          'TG_SAIDA' 'CORRETO'     'Corretor'             '',
  36  'LFA1'       'NAME1'            'TG_SAIDA' 'NAME1_C'     'Desc. Corretor'       '',
  37  'ZSDT0066'   'DCO'              'TG_SAIDA' 'DCO'         'DCO'                  '',
  38  'ZSDT0066'   'AVISO'            'TG_SAIDA' 'AVISO'       'AVISO'                '',
  39  'ZSDT0170'         'ID_DUE'            'TG_SAIDA' 'ID_DUE'                 'Id.DU-e'         '',
  40  'ZSDT0170'         'NUMERO_DUE'        'TG_SAIDA' 'NUMERO_DUE'             'Numero DU-e'     '',
  41  'ZREG_EXPORTACAO'  'ID_REGISTRO_EXPO'  'TG_SAIDA' 'ID_REGISTRO_EXPO'       'Id.RE'           '',
  42  'ZREG_EXPORTACAO'  'NR_REGISTRO_EXPO'  'TG_SAIDA' 'NR_REGISTRO_EXPO'       'Numero RE'       '',
  43  'ZSDT0053'         'INSTRUCAO'         'TG_SAIDA' 'INSTRUCAO'              'Instrução'       '',

  44  'ZSDT0053'         'NUMERO_RUC'        'TG_SAIDA' 'NUMERO_RUC'             'RUC'             '35',
  45  'ZSDT0053'         'ID_NOMEACAO_TRAN'  'TG_SAIDA' 'ID_NOMEACAO_TRAN'       'Id.Nomeação'     '',
  46  'ZNOM_TRANSPORTE'  'DS_NOME_TRANSPOR'  'TG_SAIDA' 'DS_NOME_TRANSPOR'       'Ds.Transporte'   '25',
  47  'ZSDT0051'         'DATA_VENDA      '  'TG_SAIDA' 'DATA_VENDA      '       'Data da venda'   '10',
  54  'VBAK'             'ERDAT'             'TG_SAIDA' 'ERDAT'                  'Data de Criação OV' ''.

    "102915 CS2023000054 INCLUIR COLUNA "SAFRA" NA TRANSAÇÃO ZSDT0066 - SCABANA
    IF p_form IS NOT INITIAL.
      PERFORM montar_estrutura USING:
      49  'ZSDT0066'       'CHARG'            'TG_SAIDA' 'SAFRA'                          'Safra'       '10'.
    ENDIF.
    "102915 CS2023000054 INCLUIR COLUNA "SAFRA" NA TRANSAÇÃO ZSDT0066 - SCABANA

    IF p_form IS INITIAL.
      PERFORM montar_estrutura USING:
      48  'ZSDT0053'   'NAVIO'            'TG_SAIDA' 'NAVIO'            'Navio'                   ''.

    ELSE.
      PERFORM montar_estrutura USING:
      47  'KNA1'             'KUNNR'      'TG_SAIDA' 'ID_LOCAL_DESCARGA'    'Id.Local Descarga'     '',
      48  'KNA1'             'NAME1'      'TG_SAIDA' 'DS_LOCAL_DESCARGA'    'Ds.Local Descarga'     '25',
      49  'ZSDT0066'         'PONTO_C'    'TG_SAIDA' 'PONTO_COLETA'         'Ponto Coleta(PC)'      '',
      50  'LFA1'             'NAME1'      'TG_SAIDA' 'NAME_PC'              'Ds.Ponto Coleta(PC)'   '25',
      51  'ZSDT0066'         'TERMINAL'   'TG_SAIDA' 'TERMINAL_Z1'          'Terminal(Z1)'          '',
      52  'LFA1'             'NAME1'      'TG_SAIDA' 'NAME_TERMINAL'        'Ds.Terminal(Z1)'       '25'.

    ENDIF.

    PERFORM montar_estrutura USING:
    38  'ZSDT0051'   'OBSERVACAO'       'TG_SAIDA' 'OBSERVACAO'  'Observações'     '',
"84616 - CS2022000665 - Gravar ID_EXport LP
    53  'ZSDT0053'   'ID_EXPORT'        'TG_SAIDA' 'ID_EXPORT'   'ID Redex'        ''.

    IF p_gera IS NOT INITIAL.
      DELETE estrutura WHERE fieldname EQ 'QTD_REMESSA'
                          OR fieldname EQ 'SALDO_OV'.
    ENDIF.

    IF p_form IS NOT INITIAL.
      DELETE estrutura WHERE fieldname EQ 'INSTRUCAO'.
    ENDIF.



    IF p_form IS INITIAL.
      DELETE estrutura WHERE fieldname EQ 'DCO'
                          OR fieldname EQ 'AVISO'.
    ENDIF.


  ELSEIF p_alv EQ 'TG_FLUXOSD'. "RJF - Ini

    PERFORM montar_estrutura USING:
  1   'VBAK'             'VBELN_VA'           'TG_FLUXOSD' 'VBELN_VA'       'Numero OV'                          '15',
  2   'VBRK'             'WERKS'              'TG_FLUXOSD' 'WERKS'          'Filial Cliente'                     '10',
  3   'J_1BNFDOC'        'NFENUM'             'TG_FLUXOSD' 'NFE_NUM'        'Número NFE'                         '',
  4   'J_1BNFLIN'        'DOCNUM'             'TG_FLUXOSD' 'DOCNUM_NFE'     'DOCNUM Nfe'                         '',
  5   'ZSDT_RETLOTE_TER' 'CHAVE_NFE'          'TG_FLUXOSD' 'CHAVE_NF'       'Chave Nfe'                          '',
  6   'J_1BNFDOC'        'DOCDAT'             'TG_FLUXOSD' 'DATA_NF'        'Data Nfe'                           '',
  7   'J_1BNFLIN'        'MENGE'              'TG_FLUXOSD' 'QUANTIDADE_NF'  'Quantidade Nfe'                     '',
  8   'J_1BNFLIN'        'NETPR'              'TG_FLUXOSD' 'PRECO_NF'       'Preço Nfe'                          '',
  9   'J_1BNFLIN'        'NETWR'              'TG_FLUXOSD' 'VALOR_NF'       'Valor Nfe'                          '',
  10  'TDS_DOCFLOW'      'MATNR'              'TG_FLUXOSD' 'MATNR'          'Produto'                            '',
  11  'TDS_DOCFLOW'      'MAKTX'              'TG_FLUXOSD' 'MAKTX'          'Descrição Produto'                  '50',
  12  'TDS_DOCFLOW'      'RFMNG'              'TG_FLUXOSD' 'RFMNG_F'        'Quantidade'                         '',
  13  'VBRK'             'KURRF'              'TG_FLUXOSD' 'KURRF'          'Taxa de câmbio'                     '',
  14  'VBRK'             'FKART'              'TG_FLUXOSD' 'FKART'          'Tipo documento Fatura'              '',
  15  'J_1BNFDOC'        'INCO1'              'TG_FLUXOSD' 'INCO1'          'Incoterms'                          '',

  16   'J_1BNFDOC'        'NFENUM'             'TG_FLUXOSD' 'NFE_NUM_R'        'Número NFE Recusa'               '',
  17   'J_1BNFLIN'        'DOCNUM'             'TG_FLUXOSD' 'DOCNUM_NFE_R'     'DOCNUM Nfe Recusa'               '',
  18   'ZSDT_RETLOTE_TER' 'CHAVE_NFE'          'TG_FLUXOSD' 'CHAVE_NF_R'       'Chave Nfe Recusa'                '',
  19   'J_1BNFDOC'        'DOCDAT'             'TG_FLUXOSD' 'DATA_NF_R'        'Data Nfe Recusa'                 '',
  20   'J_1BNFLIN'        'MENGE'              'TG_FLUXOSD' 'QUANTIDADE_NF_R'  'Quantidade Nfe Recusa'           '',
  21   'J_1BNFLIN'        'NETPR'              'TG_FLUXOSD' 'PRECO_NF_R'       'Preço Nfe Recusa'                '',
  22   'J_1BNFLIN'        'NETWR'              'TG_FLUXOSD' 'VALOR_NF_R'       'Valor Nfe Recusa'                '',
  22   'TDS_DOCFLOW'      'RFMNG'              'TG_FLUXOSD' 'RFMNG_R'          'Quantidade Recusa'               '',

  23   'J_1BNFDOC'        'NFENUM'             'TG_FLUXOSD' 'NFE_NUM_D'        'Número NFE Devolução'            '',
  24   'J_1BNFLIN'        'DOCNUM'             'TG_FLUXOSD' 'DOCNUM_NFE_D'     'DOCNUM Nfe Devolução'            '',
  25   'ZSDT_RETLOTE_TER' 'CHAVE_NFE'          'TG_FLUXOSD' 'CHAVE_NF_D'       'Chave Nfe Devolução'             '',
  26   'J_1BNFDOC'        'DOCDAT'             'TG_FLUXOSD' 'DATA_NF_D'        'Data Nfe Devolução'              '',
  27   'J_1BNFLIN'        'MENGE'              'TG_FLUXOSD' 'QUANTIDADE_NF_D'  'Quantidade Nfe Devolução'        '',
  28   'J_1BNFLIN'        'NETPR'              'TG_FLUXOSD' 'PRECO_NF_D'       'Preço Nfe Devolução'             '',
  29   'J_1BNFLIN'        'NETWR'              'TG_FLUXOSD' 'VALOR_NF_D'       'Valor Nfe Devolução'             '',
  29   'TDS_DOCFLOW'      'RFMNG'              'TG_FLUXOSD' 'RFMNG_D'          'Quantidade Devolução'            '',

  30   'J_1BNFDOC'        'NFENUM'             'TG_FLUXOSD' 'NFE_NUM_C'        'Número NFE Complemento'          '',
  31   'J_1BNFLIN'        'DOCNUM'             'TG_FLUXOSD' 'DOCNUM_NFE_C'     'DOCNUM Nfe Complemento'          '',
  32   'ZSDT_RETLOTE_TER' 'CHAVE_NFE'          'TG_FLUXOSD' 'CHAVE_NF_C'       'Chave Nfe Complemento'           '',
  33   'J_1BNFDOC'        'DOCDAT'             'TG_FLUXOSD' 'DATA_NF_C'        'Data Nfe Complemento'            '',
  34   'J_1BNFLIN'        'MENGE'              'TG_FLUXOSD' 'QUANTIDADE_NF_C'  'Quantidade Nfe Complemento'      '',
  35   'J_1BNFLIN'        'NETPR'              'TG_FLUXOSD' 'PRECO_NF_C'       'Preço Nfe Complemento'           '',
  36   'J_1BNFLIN'        'NETWR'              'TG_FLUXOSD' 'VALOR_NF_C'       'Valor Nfe Complemento'           '',
  37   'TDS_DOCFLOW'      'RFMNG'              'TG_FLUXOSD' 'RFMNG_C'          'Quantidade Complemento'          '',
  38   'ZPFE_LOTE_ITEM'   'PESO_CHEGADA'       'TG_FLUXOSD' 'PESO_CHEGADA'     'Peso Descarga'                   '',
  39   'ZPFE_LOTE_ITEM'   'DT_CHEGADA'         'TG_FLUXOSD' 'DT_CHEGADA'       'Data Descarga'                   ''.

* RJF - Fim
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

  wa_estrutura-fieldname      = p_field.
  wa_estrutura-tabname        = p_tabname.
  wa_estrutura-ref_tabname    = p_ref_tabname.
  wa_estrutura-ref_fieldname  = p_ref_fieldname.
  wa_estrutura-key            = ' '.
  wa_estrutura-key_sel        = 'X'.
  wa_estrutura-col_pos        = p_col_pos.
  wa_estrutura-no_out         = ' '.
  wa_estrutura-seltext_s      = p_scrtext_l.
  wa_estrutura-seltext_m      = p_scrtext_l.
  wa_estrutura-seltext_l      = p_scrtext_l.
  wa_estrutura-reptext_ddic   = p_scrtext_l.


  IF p_outputlen IS INITIAL.
    wa_estrutura-outputlen     = x_contador.
  ELSE.
    wa_estrutura-outputlen     =  p_outputlen.
  ENDIF.

  CASE p_field.
    WHEN 'VBELN'  OR 'NRO_SOL_OV'.
      wa_estrutura-hotspot = 'X'.
      wa_estrutura-key = 'X'.
    WHEN 'ADIANT' OR 'STATUS'.
      wa_estrutura-just = 'C'.
    WHEN 'ANEXOS' OR 'STATUS'.
      wa_estrutura-hotspot = 'X'.
      wa_estrutura-key = 'X'.
      wa_estrutura-just = 'C'.
    WHEN 'STATUS_TRACE'.
      wa_estrutura-hotspot = 'X'.
      wa_estrutura-just = 'C'.
      wa_estrutura-key = 'X'.
  ENDCASE.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " montar_estrutura

*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top.
*            I_LOGO             = 'CLARO_50'.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM iniciar_variaves.

  v_report = sy-repid.

  PERFORM f_construir_cabecalho.

ENDFORM.                    " INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0510   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho.
*  DATA: WL_DATA_LOW(30),
*        WL_DATA_HIGHT(30).
*
*
  DATA: ls_line TYPE slis_listheader.
  CLEAR: ls_line.
*  SELECT SINGLE BUTXT
*      FROM T001
*      INTO LS_LINE-INFO
*       WHERE BUKRS EQ S_BUKRS-LOW.
*  LS_LINE-KEY = 'Empresa:'.
*  CONCATENATE 'Empresa: ' LS_LINE-INFO INTO LS_LINE-INFO SEPARATED BY SPACE.
*  LS_LINE-TYP =  'A'.
**  CONCATENATE 'EMPRESA:' S_BUKRS-LOW INTO LS_LINE-INFO SEPARATED BY SPACE.
*  APPEND LS_LINE TO T_TOP.
*
*  CLEAR LS_LINE.
*  IF S_BUDAT-HIGH IS INITIAL.
*    S_BUDAT-HIGH = S_BUDAT-LOW.
*  ENDIF.
*  CONCATENATE S_BUDAT-LOW+6(2) S_BUDAT-LOW+4(2) S_BUDAT-LOW(4) INTO WL_DATA_LOW SEPARATED BY '/'.
*  CONCATENATE S_BUDAT-HIGH+6(2) S_BUDAT-HIGH+4(2) S_BUDAT-HIGH(4) INTO WL_DATA_HIGHT SEPARATED BY '/'.
*  CONDENSE WL_DATA_LOW NO-GAPS.
*  CONDENSE WL_DATA_HIGHT NO-GAPS.
*  CONCATENATE 'Período:' WL_DATA_LOW 'á' WL_DATA_HIGHT INTO LS_LINE-INFO SEPARATED BY SPACE.
*  LS_LINE-TYP  = 'A'.
*  APPEND LS_LINE TO T_TOP.
*
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-key = 'QUEBRA'.
  ls_line-info = 'COCKIPT de Geração de Ordem de Vendas'.
  APPEND ls_line TO t_top.
*
*  CLEAR LS_LINE.
*  LS_LINE-TYP  = 'A'.
*  LS_LINE-KEY = 'QUEBRA'.
*  LS_LINE-INFO = ' '.
*  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xuser_command USING ucomm LIKE sy-ucomm
                         ls_selfield  TYPE slis_selfield.

  TYPES: BEGIN OF ty_bsid,
           bukrs TYPE bsid-bukrs,
           kunnr TYPE bsid-kunnr,
           vbel2 TYPE bsid-vbel2,
         END OF ty_bsid,

         BEGIN OF ty_bsad,
           bukrs TYPE bsad-bukrs,
           kunnr TYPE bsad-kunnr,
           vbel2 TYPE bsad-vbel2,
         END OF ty_bsad.

  DATA: tl_nro_sol_ov TYPE TABLE OF zsds007 WITH HEADER LINE,
        tl_saida      TYPE TABLE OF ty_saida,
        it_bsid       TYPE TABLE OF ty_bsid,
        it_bsad       TYPE TABLE OF ty_bsad,
        tl_bdc        TYPE TABLE OF bdcdata WITH HEADER LINE,
        tl_msg        TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
        wl_par        TYPE ctu_params,
        wl_shdbnr     TYPE zshdbt0001-shdbnr,
        wl_data(10),
        wl_valor(20),
        vxreversal    TYPE bkpf-xreversal,
        wa_0001       TYPE zsdt0001.

  DATA: p_resp.

  DATA: vl_vbeln   TYPE p,
        vl_vbeln_c TYPE string.

  DATA: lw_0051 TYPE zsdt0051.

  DATA: var_msg                       TYPE string.
  DATA: cx_exception                  TYPE REF TO zcx_webservice.
  DATA: obj_zcl_webservice_taxa_curva TYPE REF TO zcl_webservice_tx_curva.

* Início - Sara Oikawa - CS2020000143 - 30.11.2020
  DATA: tl_texto         TYPE catsxt_longtext_itab,
        wl_texto         LIKE LINE OF tl_texto,
        "WL_TEXTO         TYPE LINE OF CATSXT_LONGTEXT_ITAB,
        wl_0158_mensagem TYPE string.
* Fim - Sara Oikawa - CS2020000143 - 30.11.2020

  REFRESH: tl_nro_sol_ov, tl_saida.
  DATA opt TYPE ctu_params.
  DATA: s_0053 TYPE zsdt0053.
  REFRESH: tg_bdc.

  LOOP AT tg_saida INTO wg_saida WHERE mark EQ 'X'.
    SELECT SINGLE * FROM zsdt0053 INTO s_0053
      WHERE nro_sol_ov EQ wg_saida-nro_sol_ov
        AND posnr      EQ wg_saida-posnr.
  ENDLOOP.





* #########################  ENCERRAR OVs ####################################
* #########################    INICIO     ####################################
  IF ucomm EQ 'ENCERRA'.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question         = 'Deseja Encerrar esta Ordem de Venda?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        display_cancel_button = ' '
      IMPORTING
        answer                = p_resp.

    IF p_resp EQ 1.

      CASE s_0053-status.
        WHEN 'E'.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Ordem de venda já foi encerrada anteriormente!'.
        WHEN OTHERS.

          LOOP AT tg_saida INTO wg_saida WHERE mark IS NOT INITIAL.
            MOVE: wg_saida-nro_sol_ov TO tl_nro_sol_ov-nro_sol_ov,
                  wg_saida-posnr      TO tl_nro_sol_ov-posnr.
            APPEND tl_nro_sol_ov.
            CLEAR: tl_nro_sol_ov.
          ENDLOOP.

          IF tl_nro_sol_ov[] IS NOT INITIAL.
            IF lines( tl_nro_sol_ov[] ) EQ 1." Obrigatório ter apenas uma solicitação para ser Encerrada.

*             Função para Atualizar o documento e o disparo do Hedge
              IF p_venda IS NOT INITIAL.
                CALL FUNCTION 'ZSDMF005_MODIF_STATUS_ITEM_SOL'
                  EXPORTING
                    i_status               = 'E'
                    i_syucomm              = ucomm
                  TABLES
                    ti_nro_sol_ov          = tl_nro_sol_ov
                  EXCEPTIONS
                    ov_ja_criada           = 1
                    solicitacao_nao_existe = 2
                    OTHERS                 = 3.
              ELSE.
                CALL FUNCTION 'ZSDMF005_MODIF_STATUS_ITEM_SOL'
                  EXPORTING
                    i_status               = 'E'
                    i_tipo_sol             = 'FL'
                    i_syucomm              = ucomm
                  TABLES
                    ti_nro_sol_ov          = tl_nro_sol_ov
                  EXCEPTIONS
                    ov_ja_criada           = 1
                    solicitacao_nao_existe = 2
                    OTHERS                 = 3.
              ENDIF.

              obj_ret->refresh( ).
            ELSE.
              MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Selecione somente uma Linha para ser Encerrada!'.
            ENDIF.
          ENDIF.
      ENDCASE.
    ELSE.
      CLEAR ucomm.
    ENDIF.
  ENDIF.

* #########################  ENCERRAR OVs ####################################
* #########################      FIM      ####################################

* Início - Sara Oikawa - CS2020000143 - 30.11.2020
* RETORNAR RFL

  IF ucomm = 'RETORN_RFL'.
    LOOP AT tg_saida INTO wg_saida WHERE mark IS NOT INITIAL.
      MOVE: wg_saida-nro_sol_ov TO tl_nro_sol_ov-nro_sol_ov,
            wg_saida-posnr      TO tl_nro_sol_ov-posnr.
      APPEND tl_nro_sol_ov.
      CLEAR: tl_nro_sol_ov.
    ENDLOOP.

    IF tl_nro_sol_ov[] IS NOT INITIAL.
      IF lines( tl_nro_sol_ov[] ) EQ 1." Obrigatório ter apenas uma solicitação para Retornar RFL.

*                SELECT * FROM ZSDT0158 INTO WA_0158
*                  UP TO 1 ROWS
*                 WHERE NRO_SOL_OV EQ WG_SAIDA-NRO_SOL_OV.
*                ENDSELECT.
        SELECT SINGLE * FROM zsdt0158 INTO wa_0158
           WHERE nro_sol_ov = wg_saida-nro_sol_ov.
        IF sy-subrc IS INITIAL.

          "Altera Campos MENSAGEM e Status na ZSDT0158 - Tabela Solicitação Formação Lote e Pedido Transferência
          REFRESH tl_texto.
          CLEAR: wl_texto, wl_0158_mensagem.

          CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
            EXPORTING
              im_title = 'Texto de Observação'
            CHANGING
              ch_text  = tl_texto.

          LOOP AT tl_texto INTO wl_texto.
            CONCATENATE wl_0158_mensagem wl_texto INTO wl_0158_mensagem SEPARATED BY space.
          ENDLOOP.


          UPDATE zsdt0158 SET status = ' '
                             mensagem = wl_0158_mensagem
                          WHERE sequencial = wa_0158-sequencial.

          UPDATE zsdt0051 SET status     = 'D'
                              job        = abap_false
                              usnam      = sy-uname
                              data_atual = sy-datum
                              hora_atual = sy-uzeit
                          WHERE nro_sol_ov EQ wg_saida-nro_sol_ov.
        ENDIF.

        obj_ret->refresh( ).
      ELSE.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Selecione somente uma Linha para Retornar RFL'.
      ENDIF.
    ENDIF.
*** BUG 4932 - Camila Brand
*  ELSE.
*    CLEAR UCOMM.
  ENDIF.

* Fim - Sara Oikawa - CS2020000143 - 30.11.2020

  IF ucomm EQ 'GERA'
  OR ucomm EQ 'GERA2'.

    " Início - DEVK9A1TTI - SD - CS2023000611 - Bloqueio OV -Tributação ICMS #128315 RSA
    READ TABLE tg_saida INTO wg_saida INDEX ls_selfield-tabindex.
    IF sy-subrc EQ 0.
      IF NOT wg_saida-matnr IS INITIAL.
        SELECT SINGLE matkl, extwg
               FROM mara
               INTO @DATA(wa_data_mara)
               WHERE matnr EQ @wg_saida-matnr.
        IF NOT wa_data_mara-matkl IS INITIAL.
          SELECT SINGLE matkl
                 FROM zsdt0338
                 INTO @DATA(vl_matkl)
                 WHERE matkl EQ @wa_data_mara-matkl.
          IF NOT vl_matkl IS INITIAL.
            IF wa_data_mara-extwg IS INITIAL.
              MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Grp. de Mercadoria não habilitado a gerar OV'.
              RETURN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    " Fim - DEVK9A1TTI - SD - CS2023000611 - Bloqueio OV -Tributação ICMS #128315 RSA

*    TL_SAIDA[] = TG_SAIDA[].
*    DELETE TL_SAIDA WHERE MARK IS INITIAL.
*
*    LOOP AT TG_SAIDA INTO WG_SAIDA.
*      READ TABLE TL_SAIDA TRANSPORTING NO FIELDS WITH KEY NRO_SOL_OV = WG_SAIDA-NRO_SOL_OV.
*      IF SY-SUBRC IS INITIAL.
*        MOVE: WG_SAIDA-NRO_SOL_OV TO TL_NRO_SOL_OV-NRO_SOL_OV,
*              WG_SAIDA-POSNR      TO TL_NRO_SOL_OV-POSNR.
*
*        APPEND TL_NRO_SOL_OV.
*
*        WG_SAIDA-MARK = 'X'.
*
*        MODIFY TG_SAIDA FROM WG_SAIDA.
*      ENDIF.
*      CLEAR: TL_NRO_SOL_OV.
*    ENDLOOP.
*
*    IF TL_NRO_SOL_OV[] IS NOT INITIAL.
*      IF P_FORM IS NOT INITIAL.
*        CALL FUNCTION 'ZSDMF001_GERA_OV_SOL_FORM'
*          TABLES
*            TI_NRO_SOL_OV          = TL_NRO_SOL_OV
*          EXCEPTIONS
*            OV_JA_CRIADA           = 1
*            SOLICITACAO_NAO_EXISTE = 2
*            OTHERS                 = 3.
*      ELSE.
*        CALL FUNCTION 'ZSDMF001_GERA_OV_SOLICITACAO'
*          TABLES
*            TI_NRO_SOL_OV          = TL_NRO_SOL_OV
*          EXCEPTIONS
*            OV_JA_CRIADA           = 1
*            SOLICITACAO_NAO_EXISTE = 2
*            OTHERS                 = 3.
*      ENDIF.
*      NEW ZCL_RETORNO( )->REFRESH( ).
*    ENDIF.

  ELSEIF ucomm EQ '&IC1'.
    READ TABLE tg_saida INTO wg_saida INDEX ls_selfield-tabindex.

    IF sy-subrc EQ 0.

      IF ls_selfield-fieldname EQ 'ANEXOS'.
        PERFORM f_anexa_visualiza_arquivo.
      ENDIF.

      IF ls_selfield-fieldname EQ 'STATUS_TRACE'.  "*-CS2023000189-26.04.2023-#108710-JT
        CALL FUNCTION 'ZSD_LOG_ENVIO_TRACE_COTTON'
          EXPORTING
            i_nro_sol_ov   = wg_saida-nro_sol_ov
            i_posnr        = wg_saida-posnr
            i_tipo_integra = 'OV'.

      ELSEIF ls_selfield-fieldname EQ 'VBELN'.
** Se foi clicado na coluna EBELN.

** Passa o valor clicado na coluna como parâmetro para a transação que
**se quer chamar.
** Passa o id do campo Pedido na transação VA03.

        " 12.09.2024 - 145583 -- RAMON -->
        IF wg_saida-vbeln IS NOT INITIAL AND wg_saida-auart NE 'ZUB'.
          " 12.09.2024 - 145583 -- RAMON --<
          SET PARAMETER ID 'AUN' FIELD wg_saida-vbeln.

** Chamo a transação
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

          " 12.09.2024 - 145583 -- RAMON -->
        ELSEIF wg_saida-vbeln IS NOT INITIAL.

          SET PARAMETER ID 'BES' FIELD wg_saida-vbeln.

          CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
            EXPORTING
              i_ebeln              = wg_saida-vbeln
            EXCEPTIONS
              not_found            = 1
              no_authority         = 2
              invalid_call         = 3
              preview_not_possible = 4
              OTHERS               = 5.

          IF sy-subrc <> 0.
          ENDIF.

          " 12.09.2024 - 145583 -- RAMON --<

        ENDIF.

      ELSEIF ls_selfield-fieldname EQ 'NRO_SOL_OV'.
        PERFORM f_preencher_dynpro USING:
       'X' 'ZSDR0022'                      '0050',
       ' ' 'WG_HEADER-NRO_SOL_OV'          wg_saida-nro_sol_ov,
       ' ' 'BDC_OKCODE'                    'ATUAL'.
        opt-dismode = 'E'.
        opt-defsize = ' '.

        CALL TRANSACTION 'ZSDT0062' USING tg_bdc OPTIONS FROM opt.
      ENDIF.
    ENDIF.

  ELSEIF ucomm EQ 'BLOQUEAR'.
    LOOP AT tg_saida INTO wg_saida
            WHERE mark IS NOT INITIAL.

      MOVE: wg_saida-nro_sol_ov TO tl_nro_sol_ov-nro_sol_ov,
            wg_saida-posnr      TO tl_nro_sol_ov-posnr.
      APPEND tl_nro_sol_ov.

      CLEAR: tl_nro_sol_ov.
    ENDLOOP.
    IF tl_nro_sol_ov[] IS NOT INITIAL.
      IF p_venda IS NOT INITIAL.
        CALL FUNCTION 'ZSDMF005_MODIF_STATUS_ITEM_SOL'
          EXPORTING
            i_status               = 'B'
            i_syucomm              = ucomm
          TABLES
            ti_nro_sol_ov          = tl_nro_sol_ov
          EXCEPTIONS
            ov_ja_criada           = 1
            solicitacao_nao_existe = 2
            OTHERS                 = 3.
      ELSE.
        CALL FUNCTION 'ZSDMF005_MODIF_STATUS_ITEM_SOL'
          EXPORTING
            i_status               = 'B'
            i_tipo_sol             = 'FL'
            i_syucomm              = ucomm
          TABLES
            ti_nro_sol_ov          = tl_nro_sol_ov
          EXCEPTIONS
            ov_ja_criada           = 1
            solicitacao_nao_existe = 2
            OTHERS                 = 3.
      ENDIF.
      IF sy-subrc <> 0.
      ENDIF.
      obj_ret->refresh( ).

    ENDIF.
  ELSEIF ucomm EQ 'DESBLOQ'.
    LOOP AT tg_saida INTO wg_saida
        WHERE mark IS NOT INITIAL.

      MOVE: wg_saida-nro_sol_ov TO tl_nro_sol_ov-nro_sol_ov,
            wg_saida-posnr      TO tl_nro_sol_ov-posnr.
      APPEND tl_nro_sol_ov.

      CLEAR: tl_nro_sol_ov.
    ENDLOOP.
    IF tl_nro_sol_ov[] IS NOT INITIAL.
      IF p_venda IS NOT INITIAL.
        CALL FUNCTION 'ZSDMF005_MODIF_STATUS_ITEM_SOL'
          EXPORTING
            i_status               = 'D'
            i_syucomm              = ucomm
          TABLES
            ti_nro_sol_ov          = tl_nro_sol_ov
          EXCEPTIONS
            ov_ja_criada           = 1
            solicitacao_nao_existe = 2
            OTHERS                 = 3.
      ELSE.
        CALL FUNCTION 'ZSDMF005_MODIF_STATUS_ITEM_SOL'
          EXPORTING
            i_status               = 'D'
            i_tipo_sol             = 'FL'
            i_syucomm              = ucomm
          TABLES
            ti_nro_sol_ov          = tl_nro_sol_ov
          EXCEPTIONS
            ov_ja_criada           = 1
            solicitacao_nao_existe = 2
            OTHERS                 = 3.
      ENDIF.
      IF sy-subrc <> 0.

      ENDIF.

      obj_ret->refresh( ).

    ENDIF.
  ELSEIF ucomm EQ 'DEL'.

    obj_ret->get_set( ).

    tl_saida[] = tg_saida[].

    DELETE tl_saida WHERE mark IS INITIAL.

    LOOP AT tl_saida INTO wg_saida.

      CHECK wg_saida-vbeln IS NOT INITIAL.

      IF p_gera IS NOT INITIAL.

        IF NOT line_exists( obj_ret->at_set[ from =  wg_saida-auart ] ).
          CONTINUE.
        ENDIF.

      ENDIF.

      CALL FUNCTION 'MOVE_CHAR_TO_NUM'
        EXPORTING
          chr = wg_saida-vbeln
        IMPORTING
          num = vl_vbeln.

      vl_vbeln_c = vl_vbeln.

      SELECT SINGLE * FROM zsdt0001 INTO wa_0001 WHERE vbeln EQ wg_saida-vbeln.
      IF sy-subrc IS INITIAL.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Ordem possui Romaneio' wa_0001-nr_romaneio 'Vinculado!'.
        RETURN.
      ENDIF.

      "Verificação Vinculação Contabil x Ordem Venda
      REFRESH: it_bsid, it_bsad.
      SELECT a~bukrs a~kunnr a~vbel2
        INTO TABLE it_bsid
        FROM bsid AS a
        INNER JOIN bkpf AS b ON a~bukrs = b~bukrs
                            AND a~belnr = b~belnr
                            AND a~gjahr = b~gjahr
       WHERE a~bukrs EQ wg_saida-vkorg
         AND a~kunnr EQ wg_saida-kunnr
         AND b~xreversal EQ ''
         AND ( ( a~vbel2 EQ wg_saida-vbeln ) OR
               ( a~xblnr EQ wg_saida-vbeln ) OR
               ( a~vbel2 EQ vl_vbeln_c     ) OR
               ( a~xblnr EQ vl_vbeln_c     )  ).

      SELECT a~bukrs a~kunnr a~vbel2
        INTO TABLE it_bsad
        FROM bsad AS a
        INNER JOIN bkpf AS b ON a~bukrs = b~bukrs
                            AND a~belnr = b~belnr
                            AND a~gjahr = b~gjahr
       WHERE a~bukrs EQ wg_saida-vkorg
         AND a~kunnr EQ wg_saida-kunnr
         AND b~xreversal EQ ''
         AND ( ( a~vbel2 EQ wg_saida-vbeln ) OR
               ( a~xblnr EQ wg_saida-vbeln ) OR
               ( a~vbel2 EQ vl_vbeln_c     ) OR
               ( a~xblnr EQ vl_vbeln_c     )  ).

      IF ( it_bsid[] IS NOT INITIAL ) OR ( it_bsad[] IS NOT INITIAL ).
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Ordem de venda ' vl_vbeln_c 'não pode ser eliminada,' 'existem Doc. Contábeis Vinculados!'.
        RETURN.
      ENDIF.

      MOVE: wg_saida-nro_sol_ov TO tl_nro_sol_ov-nro_sol_ov,
            wg_saida-posnr      TO tl_nro_sol_ov-posnr.

      APPEND tl_nro_sol_ov.
      CLEAR: tl_nro_sol_ov.
    ENDLOOP.

    IF tl_nro_sol_ov[] IS NOT INITIAL.
      ""84616 - CS2022000665 - Gravar ID_EXport LP

      EXPORT id_redex FROM wg_saida-id_export  TO MEMORY ID 'ZID_REDEX'.
      "<< END
      IF p_venda IS NOT INITIAL.
        CALL FUNCTION 'ZSDMF005_MODIF_STATUS_ITEM_SOL'
          EXPORTING
            i_status               = 'X'
            i_tipo_sol             = 'VN'
            i_syucomm              = ucomm
          TABLES
            ti_nro_sol_ov          = tl_nro_sol_ov
          EXCEPTIONS
            ov_ja_criada           = 1
            solicitacao_nao_existe = 2
            OTHERS                 = 3.
      ELSE.
        CALL FUNCTION 'ZSDMF005_MODIF_STATUS_ITEM_SOL'
          EXPORTING
            i_status               = 'X'
            i_tipo_sol             = 'FL'
            i_syucomm              = ucomm
          TABLES
            ti_nro_sol_ov          = tl_nro_sol_ov
          EXCEPTIONS
            ov_ja_criada           = 1
            solicitacao_nao_existe = 2
            OTHERS                 = 3.
      ENDIF.
      IF sy-subrc <> 0.
      ENDIF.

      obj_ret->refresh( ).

    ENDIF.

  ELSEIF ucomm EQ 'ESTOR_ADI'.
    wl_par-dismode = 'N'.
    wl_par-updmode = 'A'.
    wl_par-defsize = 'X'.
    wl_par-racommit = 'X'.
    wl_par-nobinpt = 'X'.

    LOOP AT tg_saida INTO wg_saida
          WHERE mark IS NOT INITIAL.


      READ TABLE tg_0051 INTO wg_0051
        WITH KEY nro_sol_ov = wg_saida-nro_sol_ov.

      LOOP AT tg_0054 INTO wg_0054
         WHERE nro_sol_ov EQ wg_saida-nro_sol_ov
           AND posnr      EQ wg_saida-posnr.

        CLEAR vxreversal.
        SELECT SINGLE xreversal
           INTO vxreversal
           FROM bkpf
           WHERE bukrs = wg_0051-vkorg
           AND   belnr = wg_0054-adiant
           AND   gjahr = wg_0051-data_venda(4).

        IF vxreversal = 1.
          UPDATE zsdt0054 SET adiant = ''
          WHERE nro_sol_ov = wg_saida-nro_sol_ov
          AND   posnr      = wg_saida-posnr
          AND   valdt      = wg_0054-valdt.
        ELSE.
          READ TABLE tg_bsad INTO wg_bsad
            WITH KEY bukrs = wg_0051-vkorg
                     belnr = wg_0054-adiant.
          IF wg_bsad-augbl IS INITIAL.
            REFRESH: tl_bdc.
            WRITE sy-datum TO wl_data.

            f_preencher_dynpro:
             'X' 'SAPMF05A'             '0105',
             ' ' 'RF05A-BELNS'          wg_0054-adiant,
             ' ' 'BKPF-BUKRS'           wg_0051-vkorg,
             ' ' 'RF05A-GJAHS'          wg_0051-data_venda(4),
             ' ' 'UF05A-STGRD'          '02',
             ' ' 'BSIS-BUDAT'           wl_data,
             ' ' 'BDC_OKCODE'           '=BU'.


            CALL FUNCTION 'ZSHDB_CRIA_EXECUTA'
              EXPORTING
                tcode               = 'FB08'
                params              = wl_par
                i_callback_program  = sy-repid
                i_callback_formname = 'X_ESTORNA_ADIANT'
              IMPORTING
                shdbnr              = wl_shdbnr
              TABLES
                t_bdc               = tl_bdc.

          ELSE.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Doc.'
                                                wg_0054-adiant
                                                'já foi compensado e não pode ser estornado.'.
          ENDIF.
        ENDIF.
        CLEAR: wg_bsad.
      ENDLOOP.
    ENDLOOP.

    WAIT UP TO 1 SECONDS.
    obj_ret->refresh( ).

  ELSEIF ucomm EQ 'CONTRATO'.

    LOOP AT tg_saida INTO wg_saida
       WHERE mark IS NOT INITIAL.
      UPDATE zsdt0053 SET contrato = 'X'
           WHERE nro_sol_ov EQ wg_saida-nro_sol_ov
             AND posnr      EQ wg_saida-posnr.
    ENDLOOP.
    obj_ret->refresh( ).
  ELSEIF ucomm EQ 'ATUALIZA'.
    obj_ret->refresh( ).
  ELSEIF ucomm EQ 'VIN_RE_DUE'.

    DATA: lit_rem_ov   TYPE TABLE OF lips,
          lit_zdoc_exp TYPE TABLE OF zdoc_exp.

    FREE tg_bdc.

    LOOP AT tg_saida INTO wg_saida WHERE mark        IS NOT INITIAL
                                     AND remessa_exp IS NOT INITIAL.
      CLEAR: wa_vc_doc_exp.

      SELECT SINGLE * FROM mara INTO @DATA(wa_mara) WHERE matnr EQ @wg_saida-matnr.

      IF wa_mara-matkl = '700140'.

        wa_vc_doc_exp-vbeln = wg_saida-remessa_exp.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_vc_doc_exp-vbeln
          IMPORTING
            output = wa_vc_doc_exp-vbeln.


        IF wg_saida-numero_ruc IS NOT INITIAL. "Checar possibilidade de vinculo automatico
          DATA(lva_continue) = abap_true.
          PERFORM f_vinc_aut_due_rem USING wa_vc_doc_exp-vbeln
                                           wg_saida-numero_ruc
                                  CHANGING lva_continue.

          CHECK lva_continue EQ abap_true.
        ENDIF.

        PERFORM z_verifica_rem.

        EXPORT v_tcode FROM sy-tcode TO MEMORY ID 'P_TCODE'.

*        PERFORM F_PREENCHER_DYNPRO USING:
*        'X' 'ZSDI0007'                      '0100',
*        ' ' 'V_VBELN'          WA_VC_DOC_EXP-VBELN,
*        ' ' 'BDC_OKCODE'                    '/00'.
*
*        OPT-DISMODE = 'E'.
*        OPT-DEFSIZE = ' '.

        SUBMIT zsdi0007 WITH p_tcode = sy-tcode WITH v_vbeln = wa_vc_doc_exp-vbeln AND RETURN.
*        CALL TRANSACTION 'ZSDT0014' USING TG_BDC OPTIONS FROM OPT.

      ELSE.
        MESSAGE 'Vinculação não disponível para o material!' TYPE 'E'.
        EXIT.
      ENDIF.

*      CALL SCREEN 0100 STARTING AT 10 10 ENDING AT 140 16.
    ENDLOOP.

  ELSEIF  ucomm EQ 'NFS'.

    DATA: it_rsparams TYPE TABLE OF rsparams,
          wa_rsparams TYPE  rsparams.

    READ TABLE tg_saida INTO wg_saida INDEX ls_selfield-tabindex.
    IF sy-subrc = 0.
      IF wg_saida-docnum_rt IS INITIAL.
        MESSAGE 'Doc Retorno não existe !' TYPE 'I'.
        EXIT.
      ELSE.
        CLEAR: it_rsparams[].

        wa_rsparams-selname = 'P_DOCNUM'.
        wa_rsparams-kind    = 'S'.
        wa_rsparams-sign    = 'I'.
        wa_rsparams-option  = 'EQ'.
        wa_rsparams-low     = wg_saida-docnum_rt.
        APPEND wa_rsparams  TO it_rsparams.

        wa_rsparams-selname = 'GEXCEL'.
        wa_rsparams-kind    = 'P'.
        wa_rsparams-sign    = 'I'.
        wa_rsparams-option  = 'EQ'.
        wa_rsparams-low     = 'X'.
        APPEND wa_rsparams TO it_rsparams.

        SUBMIT zsdr0001  WITH SELECTION-TABLE it_rsparams AND RETURN.

      ENDIF.
    ENDIF.


  ELSEIF ucomm EQ 'NOMEACAO'.

    DATA(lit_saida_sol_ov) = tg_saida[].

    DELETE lit_saida_sol_ov WHERE mark IS INITIAL.

    IF lines( lit_saida_sol_ov ) EQ 0.
      MESSAGE 'Nenhuma linha foi selecionada!' TYPE 'I'.
      EXIT.
    ENDIF.

    SORT lit_saida_sol_ov BY nro_sol_ov.
    DELETE ADJACENT DUPLICATES FROM lit_saida_sol_ov COMPARING nro_sol_ov.

    IF lines( lit_saida_sol_ov ) > 1.
      MESSAGE 'Selecione somente uma solicitação de Ordem de Venda!' TYPE 'I'.
      EXIT.
    ENDIF.

    READ TABLE lit_saida_sol_ov INTO DATA(wl_saida_ov) INDEX 1.
    IF sy-subrc NE 0.
      MESSAGE 'Não foi possivel selecionar a solicitação de Ordem de Venda!' TYPE 'I'.
      EXIT.
    ENDIF.

    SELECT SINGLE * FROM mara INTO wa_mara WHERE matnr EQ wl_saida_ov-matnr.

    IF NOT ( ( sy-subrc EQ 0 ) AND ( wa_mara-matkl EQ '700140' ) AND ( wl_saida_ov-matnr IS NOT INITIAL ) ).
      MESSAGE 'Função disponível apenas para Materiais do grupo de Algodão!' TYPE 'I'.
      EXIT.
    ENDIF.

    IF wl_saida_ov-nro_sol_ov IS INITIAL.
      MESSAGE 'Nro. Sol. OV não informado!' TYPE 'I'.
      EXIT.
    ENDIF.

    IF wl_saida_ov-numero_ruc IS INITIAL.
      MESSAGE 'Solicitação O.V sem Numero RUC!' TYPE 'I'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'ZDUE_NOMEACAO_SOL_OV'
      EXPORTING
        i_nro_sol_ov = wl_saida_ov-nro_sol_ov
        i_numero_ruc = wl_saida_ov-numero_ruc.

  ELSEIF ucomm EQ 'IMP_DUE'.

    DATA: lwa_zsdt0170     TYPE zsdt0170,
          lit_zsdt0172     TYPE zsdt0172_t,
          lit_zsdt0173     TYPE zsdt0173_t,
          lva_id_doc_exp   TYPE zdoc_exp-id_doc_exp,
          lwa_zdoc_nf_prod TYPE zdoc_nf_produtor.

    lit_saida_sol_ov = tg_saida[].

    DELETE lit_saida_sol_ov WHERE mark IS INITIAL.

    IF lines( lit_saida_sol_ov ) EQ 0.
      MESSAGE 'Nenhuma linha foi selecionada!' TYPE 'I'.
      EXIT.
    ENDIF.

    SORT lit_saida_sol_ov BY nro_sol_ov.
    DELETE ADJACENT DUPLICATES FROM lit_saida_sol_ov COMPARING nro_sol_ov.

    IF lines( lit_saida_sol_ov ) > 1.
      MESSAGE 'Selecione somente uma solicitação de Ordem de Venda!' TYPE 'I'.
      EXIT.
    ENDIF.

    READ TABLE lit_saida_sol_ov INTO wl_saida_ov INDEX 1.
    IF sy-subrc NE 0.
      MESSAGE 'Não foi possivel selecionar a solicitação de Ordem de Venda!' TYPE 'I'.
      EXIT.
    ENDIF.

    SELECT SINGLE * FROM mara INTO wa_mara WHERE matnr EQ wl_saida_ov-matnr.

    IF NOT ( ( sy-subrc EQ 0 ) AND ( wa_mara-matkl EQ '700140' ) AND ( wl_saida_ov-matnr IS NOT INITIAL ) ).
      MESSAGE 'Função disponível apenas para Materiais do grupo de Algodão!' TYPE 'I'.
      EXIT.
    ENDIF.

    IF wl_saida_ov-nro_sol_ov IS INITIAL.
      MESSAGE 'Nro. Sol. OV não informado!' TYPE 'I'.
      EXIT.
    ENDIF.

    IF wl_saida_ov-numero_ruc IS INITIAL.
      MESSAGE 'Solicitação O.V sem Numero RUC!' TYPE 'I'.
      EXIT.
    ENDIF.

    IF wl_saida_ov-id_nomeacao_tran IS INITIAL.
      MESSAGE 'Nomeação não foi gerada para a Solicitação de O.V!' TYPE 'I'.
      EXIT.
    ENDIF.


    TRY.
        CALL FUNCTION 'ZDUE_IMPORTAR_DUE_SISCOMEX'
          EXPORTING
            i_numero_ruc         = wl_saida_ov-numero_ruc
            i_bukrs_auth         = wl_saida_ov-vkorg
            i_id_nomeacao_tran   = wl_saida_ov-id_nomeacao_tran
            i_importador         = wl_saida_ov-kunnr
            i_emb_container      = abap_true
            i_lcto_avulso        = abap_true
            i_fatura_mot_disp_nf = '3004'
            i_check_nf_cct_man   = abap_true
          IMPORTING
            e_zsdt0170           = lwa_zsdt0170
            e_zsdt0172           = lit_zsdt0172
            e_zsdt0173           = lit_zsdt0173.
      CATCH zcx_due INTO DATA(ex_due).
        ex_due->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        RETURN.
    ENDTRY.

    MESSAGE |DU-e { lwa_zsdt0170-numero_due } foi importada com sucesso!| TYPE 'I'.

    COMMIT WORK.


  ENDIF.

  CASE ucomm.
    WHEN 'RETORNO'.
      obj_ret->retorno( obj_ret->get_line( ) ).
    WHEN 'REMFAT'.
      obj_ret->remfat( obj_ret->get_line( ) ).
    WHEN 'NF'.
      obj_ret->nf( obj_ret->get_line( ) ).
    WHEN 'GERA' OR 'GERA2'.

      " 12.09.2024 - 145583 -- RAMON -->
      IF obj_ret->get_line( )-auart EQ 'ZUB'.
        obj_ret->geraped( obj_ret->get_line( ) ).
      ELSE.
        obj_ret->geraov( obj_ret->get_line( ) ).
      ENDIF.
      " 12.09.2024 - 145583 -- RAMON --<

    WHEN 'TRACE'.
      obj_ret->envia_trace( obj_ret->get_line( ) ). "*-CS2023000189-26.04.2023-#108710-JT
*   WHEN 'STATUS_TRACE'.
*     obj_ret->log_trace( obj_ret->get_line( ) ). "*-CS2023000189-26.04.2023-#108710-JT

* RJF - Ini - CS2022000859 Criar relatorio com peso de descarga para as vendas CIF do mercado interno #89618 RJF
    WHEN '&VENDACIF'.
      TYPES: BEGIN OF ty_vbeln,
               vbeln TYPE vbeln,
             END OF ty_vbeln,
             BEGIN OF ty_vbtyp,
               vbtyp TYPE vbtyp,
             END OF ty_vbtyp.

      DATA: lo_util  TYPE REF TO zcl_util_sd,
            it_vbeln TYPE TABLE OF ty_vbeln,
            it_vbtyp TYPE TABLE OF ty_vbtyp.

      READ TABLE tg_saida INTO wg_saida WITH KEY mark = abap_true.

      IF sy-subrc EQ 0.

        APPEND: wg_saida-vbeln TO it_vbeln.

*        lo_util = NEW #( ).
        CREATE OBJECT lo_util.
        lo_util->return_nf_fluxosd( EXPORTING it_vbelv       = it_vbeln
                                              it_vbtyp       = it_vbtyp
                                    IMPORTING et_fluxosd     = tg_fluxosd_t
                                              et_fluxosd_alv = tg_fluxosd
                                              ev_msg_ret     = DATA(ev_msg_ret) ).


        IF tg_fluxosd IS NOT INITIAL AND ev_msg_ret IS INITIAL .
          PERFORM imprimir_fluxo_sd.
        ELSE.
          MESSAGE ev_msg_ret TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
      ELSE.
        MESSAGE TEXT-016 TYPE 'I'.
        EXIT.
      ENDIF.
* RJF - Fim - CS2022000859 Criar relatorio com peso de descarga para as vendas CIF do mercado interno #89618 RJF
  ENDCASE.

  ls_selfield-refresh    = abap_true.
  ls_selfield-col_stable = abap_true.
  ls_selfield-row_stable = abap_true.

ENDFORM. "XUSER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  Z_PF_STATUS_SET
*&---------------------------------------------------------------------*
* Especificar barra de status
*----------------------------------------------------------------------*
FORM xpf_status_set USING pt_extab TYPE slis_t_extab.

  DATA: wl_ucomm TYPE sy-ucomm.

* Início - Sara Oikawa - CS2020000143 - 30.11.2020
* O Botão só será exibido na opção “Formação de Lote” e “Gerar OV”
  IF p_gera IS INITIAL OR p_form IS INITIAL.
    APPEND VALUE #( fcode = 'RETORN_RFL' ) TO pt_extab[].
  ENDIF.
* Fim - Sara Oikawa - CS2020000143 - 30.11.2020

  IF p_gera IS NOT INITIAL.
    SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING pt_extab.
  ELSE.
    SET PF-STATUS 'STANDARD_FULLSCREEN2' EXCLUDING pt_extab.
  ENDIF.
ENDFORM.                    "z_pf_status_set
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
*&      Form  BUSCA_SALDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_saldo TABLES t_0053 STRUCTURE zsdt0053.
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
    SELECT *
      FROM zsdt0053
      INTO TABLE tl_0053
        FOR ALL ENTRIES IN t_0053
        WHERE nro_sol_ov EQ t_0053-nro_sol_ov
          AND vbeln NE space.

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

*** Stefanini - IR252396 - 12/08/2025 - LAZAROSR - Início de Alteração
FORM busca_saldo_pedido TABLES t_0053 STRUCTURE zsdt0053.

  DATA:
    lv_bwart_862 TYPE ekbe-bwart VALUE '862',
    lv_bewtp_u   TYPE ekbe-bewtp VALUE 'U'.

  IF t_0053[] IS NOT INITIAL.

    SELECT ebeln,
           menge
     FROM ekbe
     INTO TABLE @DATA(lt_ekbe)
      FOR ALL ENTRIES IN @t_0053
        WHERE ebeln = @t_0053-vbeln
          AND bwart = @lv_bwart_862
          AND bewtp = @lv_bewtp_u.

    IF sy-subrc IS INITIAL.

      SORT lt_ekbe BY ebeln menge.

      LOOP AT t_0053 INTO DATA(ls_0053).

        CLEAR wg_saldo_pedido.

        READ TABLE lt_ekbe INTO DATA(ls_ekbe)
                           WITH KEY ebeln = ls_0053-vbeln
                                    menge = ls_0053-zmeng
                                            BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          wg_saldo_pedido-ebeln = ls_ekbe-ebeln.
          wg_saldo_pedido-menge = ls_ekbe-menge.

          wg_saldo_pedido-total = ls_ekbe-menge.
          CLEAR wg_saldo_pedido-saldo.

          APPEND wg_saldo_pedido TO tg_saldo_pedido.

        ENDIF.

      ENDLOOP.

      SORT tg_saldo_pedido BY ebeln menge.

    ENDIF.

  ENDIF.

ENDFORM.
*** Stefanini - IR252396 - 12/08/2025 - LAZAROSR - Fim de Alteração

*&---------------------------------------------------------------------*
*&      Form  BUSCA_SALDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_saldo_form TABLES t_0066 STRUCTURE zsdt0066.
  DATA: tl_0066  LIKE TABLE OF zsdt0066 WITH HEADER LINE,
        tl_vbfa  TYPE TABLE OF vbfa WITH HEADER LINE,
        wl_matnr TYPE mara-matnr,
        wl_zieme TYPE mara-meins,
        wl_pmein TYPE mara-meins,
        wl_menge TYPE ekpo-menge,
        wl_tabix TYPE sy-tabix.

  REFRESH: tg_saldo.
*  TL_ITENS[] = TG_ITENS[].
*
*  DELETE TL_ITENS WHERE VBELN IS INITIAL.

  IF t_0066[] IS NOT INITIAL.

    tl_0066[] = t_0066[].

*    SELECT *
*      FROM ZSDT0066
*      INTO TABLE TL_0066
*        FOR ALL ENTRIES IN T_0066
*        WHERE NRO_SOL_OV EQ T_0066-NRO_SOL_OV
*          AND VBELN NE SPACE.



    IF sy-subrc IS INITIAL.
      SELECT *
        FROM vbfa
        INTO TABLE tl_vbfa
         FOR ALL ENTRIES IN tl_0066
         WHERE vbelv EQ tl_0066-vbeln
           AND vbtyp_n EQ 'J'
           AND vbtyp_v EQ 'C'.

      SORT tl_vbfa BY vbelv.
      REFRESH: tg_saldo.
      LOOP AT tl_0066.
        CLEAR: wg_saldo.
        LOOP AT tl_vbfa WHERE vbelv EQ tl_0066-vbeln.

          wg_saldo-vbeln = tl_vbfa-vbelv.
          wg_saldo-werks = tl_0066-werks.
          wg_saldo-total = tl_vbfa-rfmng.

          COLLECT wg_saldo INTO tg_saldo .
          wl_tabix = sy-tabix.
*        CLEAR TG_SALDO.
        ENDLOOP.
        IF sy-subrc IS INITIAL.
          READ TABLE tg_saldo INTO wg_saldo INDEX wl_tabix.
        ENDIF.

        wl_matnr = tl_0066-matnr.
        wl_zieme = tl_0066-zieme.
        wl_pmein = tl_vbfa-meins.
        wl_menge = tl_0066-zmeng.

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
        wg_saldo-nro_sol_ov = tl_0066-nro_sol_ov.
        wg_saldo-saldo = wg_saldo-zmeng - wg_saldo-total.

        IF wl_tabix IS NOT INITIAL.
          MODIFY tg_saldo FROM wg_saldo INDEX wl_tabix.
        ELSE.
          wg_saldo-vbeln = tl_0066-vbeln.
          wg_saldo-werks = tl_0066-werks.
          APPEND wg_saldo TO tg_saldo.
        ENDIF.
        CLEAR: wl_tabix.
*      ENDIF.
      ENDLOOP.

    ENDIF.
  ENDIF.
ENDFORM.                    " BUSCA_SALDO_FORM
*&---------------------------------------------------------------------*
*&      Form  BUSCA_SALDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM x_estorna_adiant  TABLES tl_zshdb_msg STRUCTURE zshdbt0002
                              tl_zshdb     STRUCTURE zshdbt0001.

  DATA: wl_zshdb   TYPE TABLE OF zshdbt0001,
        wl_obj_key TYPE zfit0036-obj_key,
        tl_0054    TYPE TABLE OF zsdt0054.

  IF tl_zshdb_msg[] IS INITIAL.
    READ TABLE tl_zshdb
      WITH KEY fnam = 'RF05A-BELNS'.

    CONDENSE tl_zshdb-fval NO-GAPS.

*    SELECT *
*      FROM ZSDT0054
*      INTO TABLE TL_0054
*       WHERE ADIANT EQ TL_ZSHDB-FVAL.
    UPDATE zsdt0054 SET   adiant = space
                    WHERE adiant EQ tl_zshdb-fval.

    CONCATENATE 'PB' tl_zshdb-fval INTO  wl_obj_key.
    DELETE FROM zfit0036 WHERE obj_key EQ wl_obj_key.


  ENDIF.
ENDFORM.                    " X_ESTORNA_ADIANT
*&---------------------------------------------------------------------*
*&      Form  BUSCA_SALDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM x_estorna_adiant_ext  TABLES tl_zshdb_msg STRUCTURE zshdbt0002
                                  tl_zshdb     STRUCTURE zshdbt0001.

  DATA: wl_zshdb      TYPE TABLE OF zshdbt0001,
        wl_0063       TYPE  zsdt0063,
        wl_obj_key    TYPE zfit0036-obj_key,
        wl_nro_sol_ov TYPE zsdt0063-nro_sol_ov.

  IF tl_zshdb_msg[] IS INITIAL.
    READ TABLE tl_zshdb
      WITH KEY fnam = 'RF05A-BELNS'.

    CONDENSE tl_zshdb-fval NO-GAPS.

    SELECT SINGLE *
      FROM zsdt0063
       INTO wl_0063
       WHERE adiant EQ tl_zshdb-fval.


    UPDATE zsdt0063 SET   adiant = space
                    WHERE adiant EQ tl_zshdb-fval.

    wl_nro_sol_ov = wl_0063-nro_sol_ov.
    SHIFT wl_nro_sol_ov LEFT DELETING LEADING '0'.
    CONCATENATE 'P' wl_0063-adiant INTO wl_obj_key.
    DELETE FROM zfit0036 WHERE bukrs EQ wl_0063-bukrs
                          AND invoice EQ wl_nro_sol_ov
                          AND moeda_pgto EQ wl_0063-waers
                          AND obj_key    EQ wl_obj_key.

  ENDIF.
ENDFORM.                    " X_ESTORNA_ADIANT
*&---------------------------------------------------------------------*
*&      Form  BLOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM block .

  LOOP AT s_vkorg.

    IF NOT s_vkorg-high IS INITIAL.

      cont_1 = s_vkorg-low.
      WHILE s_vkorg-high GE cont_1.

        AUTHORITY-CHECK OBJECT 'Z_SD_T0066' ID 'ZVKORG' FIELD cont_1.

        IF sy-subrc IS INITIAL.
          wa_check-bukrs = cont_1.
          wa_check-livre = 'X'.
          APPEND wa_check TO it_check.
        ELSE.
          wa_check-bukrs = cont_1.
          wa_check-block = 'X'.
          APPEND wa_check TO it_check.
        ENDIF.
        cont_1 = cont_1 + 1.

        CLEAR wa_check.
      ENDWHILE.
    ENDIF.

    AUTHORITY-CHECK OBJECT 'Z_SD_T0066' ID 'ZVKORG' FIELD s_vkorg-low.

    IF sy-subrc IS INITIAL.
      wa_check-bukrs = s_vkorg-low.
      wa_check-livre = 'X'.
      APPEND wa_check TO it_check.
    ELSE.
      wa_check-bukrs = s_vkorg-low.
      wa_check-block = 'X'.
      APPEND wa_check TO it_check.
    ENDIF.

    CLEAR wa_check.
  ENDLOOP.

  SORT it_check BY bukrs.
  DELETE ADJACENT DUPLICATES FROM it_check COMPARING bukrs.

  LOOP AT it_check INTO wa_check.

    IF NOT wa_check-block IS INITIAL.
      cont = cont + 1.
    ELSE.
      CONCATENATE concat wa_check-bukrs INTO concat SEPARATED BY ', '.
    ENDIF.

  ENDLOOP.

  CASE 'X'.
    WHEN p_form.
      MOVE 'FL' TO p_tpvenda.
      MOVE TEXT-004 TO msg_venda.
    WHEN p_venda.
      MOVE 'VN' TO p_tpvenda.
      MOVE TEXT-003 TO msg_venda.
  ENDCASE.

  CLEAR check_tpvenda.
  AUTHORITY-CHECK OBJECT 'Z_SD_T0066' ID 'TPVENDA' FIELD p_tpvenda.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH msg_venda.
    check_tpvenda = abap_true.
  ENDIF.


ENDFORM.                    " BLOCK
*&---------------------------------------------------------------------*
*&      Form  RETORNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM retorno .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REMFAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM remfat .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  NF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM nf .

ENDFORM.

FORM f_control_field_0100.
  LOOP AT SCREEN.
    IF ( screen-name EQ 'WA_VC_DOC_EXP-NR_REGISTRO_EXPO' ) OR
       ( screen-name EQ 'WA_VC_DOC_EXP-NUMERO_DUE'       ).
      IF wa_vc_doc_exp-vbeln IS INITIAL.
        screen-input = 0.
      ELSEIF ( wa_vc_doc_exp-nr_registro_expo IS NOT INITIAL ) OR ( wa_vc_doc_exp-numero_due IS NOT INITIAL ).
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'T0100'.

  PERFORM f_control_field_0100.

ENDMODULE.

MODULE zm_vbeln INPUT.

* Verifica Remessa
  PERFORM z_verifica_rem.

ENDMODULE.                 " ZM_VBELN  INPUT


FORM z_verifica_rem.

  REFRESH: t_conhec_a,
           t_conhec_v,
           t_full    .

  CLEAR: wa_vc_doc_exp-bukrs,
         wa_vc_doc_exp-butxt,
         wa_vc_doc_exp-werks,
         wa_vc_doc_exp-name1,
         wa_vc_doc_exp-lgort,
         wa_vc_doc_exp-lgobe,
         wa_vc_doc_exp-numero_due,
         wa_vc_doc_exp-nr_registro_expo,
         wa_vc_doc_exp-dde,
         wa_vc_doc_exp-id_nomeacao_tran.

  CHECK wa_vc_doc_exp-vbeln IS NOT INITIAL.

  SELECT SINGLE vbeln
    FROM likp INTO wa_vc_doc_exp-vbeln
   WHERE vbeln EQ wa_vc_doc_exp-vbeln.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE e836(sd) WITH TEXT-005.
  ELSE.
*   Preenche Dados Empresa/Centro/Depósito
    PERFORM z_preenche_emp.
*   Preenche Table Controls
    PERFORM z_preenche_controls.
*   Prencher o número da Re para Remessa que já estajam vinculadas.
    PERFORM z_preenche_re_due.
  ENDIF.
ENDFORM.                    " Z_VERIFICA_REM

FORM z_preenche_emp.

  DATA vl_vgbel TYPE lips-vgbel.

  SELECT SINGLE vgbel
    FROM lips INTO vl_vgbel
   WHERE vbeln EQ wa_vc_doc_exp-vbeln.

  CHECK vl_vgbel IS NOT INITIAL.

  SELECT SINGLE werks gsber lgort
    FROM vbap INTO (wa_vc_doc_exp-werks, wa_vc_doc_exp-gsber, wa_vc_doc_exp-lgort)
   WHERE vbeln EQ vl_vgbel.

  SELECT SINGLE bukrs_vf
    FROM vbak INTO wa_vc_doc_exp-bukrs
   WHERE vbeln EQ vl_vgbel.

  IF wa_vc_doc_exp-werks IS NOT INITIAL.
    SELECT SINGLE name1
      FROM t001w INTO wa_vc_doc_exp-name1
     WHERE werks EQ wa_vc_doc_exp-werks.
  ENDIF.

  IF wa_vc_doc_exp-lgort IS NOT INITIAL.
    SELECT SINGLE lgobe
      FROM t001l INTO wa_vc_doc_exp-lgobe
     WHERE lgort EQ wa_vc_doc_exp-lgort.
  ENDIF.

  IF wa_vc_doc_exp-bukrs IS NOT INITIAL.
    SELECT SINGLE butxt
      FROM t001 INTO wa_vc_doc_exp-butxt
    WHERE bukrs EQ wa_vc_doc_exp-bukrs.
  ENDIF.

ENDFORM.                    " Z_PREENCHE_EMP

FORM z_preenche_controls.

  TYPES: BEGIN OF ty_pais,
           sign   TYPE char1,
           option TYPE  char2,
           low    TYPE land1,
           high   TYPE land1,
         END OF ty_pais.

  DATA: tl_rem_bl TYPE TABLE OF zdoc_rem_bl,
        tl_exp    TYPE TABLE OF zdoc_exp,
        sl_conhec LIKE LINE OF  t_conhec_a,
        sl_rem_bl TYPE zdoc_rem_bl,
        it_pais   TYPE TABLE OF ty_pais WITH HEADER LINE,
        it_t005t  TYPE TABLE OF t005t INITIAL SIZE 0 WITH HEADER LINE,
        wa_t005t  TYPE t005t,
        vg_tabix  TYPE sy-tabix.

  REFRESH: t_conhec_a,
           t_conhec_v,
           t_full,
           tl_exp[].

  IF wa_vc_doc_exp-id_nomeacao_tran IS NOT INITIAL.

    SELECT *
      FROM znom_conhec INTO CORRESPONDING FIELDS OF TABLE t_conhec_a
     WHERE id_nomeacao_tran EQ wa_vc_doc_exp-id_nomeacao_tran.

    SORT t_conhec_a BY id_conhec        ASCENDING
                       id_nomeacao_tran ASCENDING.

    t_full[] = t_conhec_a[].

    LOOP AT t_conhec_a INTO sl_conhec.
      it_pais-sign   = 'I'.
      it_pais-option = 'EQ'.
      it_pais-low    = sl_conhec-sg_pais_destino.
      it_pais-high   = sl_conhec-sg_pais_destino.
      APPEND it_pais.
    ENDLOOP.

    SORT it_pais BY low.
    DELETE ADJACENT DUPLICATES FROM it_pais COMPARING low.

    IF it_pais[] IS NOT INITIAL.
      SELECT * INTO TABLE it_t005t
        FROM t005t
       WHERE spras EQ sy-langu
         AND land1 IN it_pais.
    ENDIF.

    LOOP AT t_conhec_a INTO sl_conhec.
      vg_tabix = sy-tabix.
      it_pais-low = sl_conhec-sg_pais_destino.
      READ TABLE it_t005t INTO wa_t005t WITH KEY land1 = it_pais-low.
      IF sy-subrc IS INITIAL.
        sl_conhec-landx = wa_t005t-landx.
        MODIFY t_conhec_a INDEX vg_tabix FROM sl_conhec TRANSPORTING landx.
      ENDIF.
    ENDLOOP.

  ENDIF.

  CLEAR: tl_exp[].

  IF ( wa_vc_doc_exp-vbeln            IS NOT INITIAL ) AND
     ( wa_vc_doc_exp-id_registro_expo IS NOT INITIAL ).
    SELECT *
      FROM zdoc_exp INTO TABLE tl_exp
     WHERE vbeln            EQ wa_vc_doc_exp-vbeln
       AND id_registro_expo EQ wa_vc_doc_exp-id_registro_expo.
  ENDIF.

  IF ( wa_vc_doc_exp-vbeln            IS NOT INITIAL ) AND
     ( wa_vc_doc_exp-id_due IS NOT INITIAL ).
    SELECT *
      FROM zdoc_exp INTO TABLE tl_exp
     WHERE vbeln  EQ wa_vc_doc_exp-vbeln
       AND id_due EQ wa_vc_doc_exp-id_due.
  ENDIF.

  IF tl_exp[] IS NOT INITIAL.
    SELECT *
      FROM zdoc_rem_bl INTO TABLE tl_rem_bl
       FOR ALL ENTRIES IN tl_exp
     WHERE id_doc_exp EQ tl_exp-id_doc_exp.
  ENDIF.

  LOOP AT tl_rem_bl INTO sl_rem_bl.
    sl_conhec-id_conhec        = sl_rem_bl-id_conhec.
    sl_conhec-id_nomeacao_tran = sl_rem_bl-id_nomeacao_tran.
    sl_conhec-dt_data          = sl_rem_bl-dt_data.
    sl_conhec-nr_qtde          = sl_rem_bl-nr_qtde.
    sl_conhec-nr_conhec        = sl_rem_bl-nr_conhec.
    sl_conhec-id               = sl_rem_bl-id_doc_exp.
    sl_conhec-item             = sl_rem_bl-id_doc_item.

    APPEND sl_conhec TO t_conhec_v.

    CLEAR: sl_rem_bl,
           sl_conhec.
  ENDLOOP.

  IF t_conhec_v[] IS NOT INITIAL.
    LOOP AT t_conhec_v INTO sl_conhec.
      READ TABLE t_conhec_a
        WITH KEY id_conhec        = sl_conhec-id_conhec
                 id_nomeacao_tran = sl_conhec-id_nomeacao_tran
        BINARY SEARCH
        TRANSPORTING NO FIELDS.
      IF sy-subrc IS INITIAL.
        DELETE t_conhec_a INDEX sy-tabix.
      ENDIF.
      CLEAR sl_conhec.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " Z_PREENCHE_CONTROLS

FORM z_preenche_re_due.

*  Buscar RE/DU-e vinculada a Rmessa informada.
  SELECT SINGLE nr_registro_expo numero_due
    FROM zdoc_exp INTO (wa_vc_doc_exp-nr_registro_expo, wa_vc_doc_exp-numero_due)
   WHERE vbeln = wa_vc_doc_exp-vbeln.

  IF ( sy-subrc IS INITIAL ) AND ( wa_vc_doc_exp-vbeln IS NOT INITIAL ).
    PERFORM z_verifica_re.
    PERFORM z_verifica_due.
  ENDIF.
ENDFORM.                    " Z_PREENCHE_RE

FORM z_verifica_re .

  REFRESH: t_conhec_a,
           t_conhec_v,
           t_full    .

  CHECK wa_vc_doc_exp-nr_registro_expo IS NOT INITIAL.

  SELECT SINGLE nr_registro_expo
    FROM zreg_exportacao INTO wa_vc_doc_exp-nr_registro_expo
   WHERE nr_registro_expo EQ wa_vc_doc_exp-nr_registro_expo
     AND in_status_comex  NE 'X'.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE e836(sd) WITH TEXT-006.
  ELSE.
    PERFORM z_valida_materiais.
*   Preenche Dados DDE/Navio
    PERFORM z_preenche_dde.
*   Preenche Table Controls
    PERFORM z_preenche_controls.
  ENDIF.
ENDFORM.                    " Z_VERFICA_RE

FORM z_verifica_due.

  REFRESH: t_conhec_a,
           t_conhec_v,
           t_full.

  CHECK wa_vc_doc_exp-numero_due IS NOT INITIAL.

  SELECT SINGLE numero_due
    FROM zsdt0170 INTO wa_vc_doc_exp-numero_due
   WHERE numero_due  EQ wa_vc_doc_exp-numero_due
     AND status      EQ '1'
     AND lcto_avulso EQ abap_true.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE e836(sd) WITH TEXT-012.
  ELSE.
    "PERFORM Z_VALIDA_MATERIAIS_DUE.
*   Preenche Dados DDE/Navio
    PERFORM z_preenche_dde.
*   Preenche Table Controls
    PERFORM z_preenche_controls.
  ENDIF.
ENDFORM.                    " Z_VERFICA_RE

FORM z_valida_materiais.

  DATA: BEGIN OF tl_exportacao OCCURS 0,
          cd_material TYPE zreg_exportacao-cd_material,
        END OF tl_exportacao,

        BEGIN OF tl_lips OCCURS 0,
          matnr TYPE lips-matnr,
        END OF tl_lips.

  DATA: BEGIN OF tl_0093 OCCURS 0,
          matnr TYPE mara-matnr,
        END OF tl_0093.

  DATA: lv_cont     TYPE i,
        lv_material TYPE zreg_exportacao-cd_material.

  CLEAR: lv_cont.

* Encontra os materiais da remessa
  SELECT *
    FROM lips
    INTO CORRESPONDING FIELDS OF TABLE tl_lips
    WHERE vbeln EQ wa_vc_doc_exp-vbeln.

* Encontra os materiais do RE
  SELECT *
    FROM zreg_exportacao
    INTO CORRESPONDING FIELDS OF TABLE tl_exportacao
    WHERE nr_registro_expo EQ wa_vc_doc_exp-nr_registro_expo.

* Verifica se a remessa e RE possuem os mesmo materiais
  LOOP AT tl_lips.
    SHIFT tl_lips-matnr LEFT DELETING LEADING '0'.

    READ TABLE tl_exportacao WITH KEY cd_material = tl_lips-matnr.

    IF sy-subrc IS INITIAL.
      ADD 1 TO lv_cont.
    ENDIF.
  ENDLOOP.

  IF lv_cont IS INITIAL.
*   Verifica se os materiais RE tem vinculo na tabela de De/Para zsdt0093
    SELECT *
      FROM zsdt0093
      JOIN mara ON mara~matkl EQ zsdt0093~matkl
      INTO CORRESPONDING FIELDS OF TABLE tl_0093
      FOR ALL ENTRIES IN tl_exportacao
      WHERE matnr_com EQ tl_exportacao-cd_material.

    IF tl_0093[] IS NOT INITIAL.
      LOOP AT tl_lips.
        READ TABLE tl_0093 WITH KEY matnr = tl_lips-matnr.
        IF sy-subrc IS INITIAL.
          ADD 1 TO lv_cont.
        ENDIF.
      ENDLOOP.
    ENDIF.

*   Interrompe o processo caso não haja vinculo entre os materiais
    IF lv_cont IS INITIAL.
      MESSAGE TEXT-007 TYPE 'E'.
      CLEAR: wa_vc_doc_exp-vbeln, wa_vc_doc_exp-nr_registro_expo.
    ENDIF.
  ENDIF.

ENDFORM.                    " Z_VALIDA_MATERIAIS


FORM z_valida_materiais_due.

  DATA: BEGIN OF tl_exportacao OCCURS 0,
          cd_material TYPE zreg_exportacao-cd_material,
        END OF tl_exportacao,

        BEGIN OF tl_lips OCCURS 0,
          matnr TYPE lips-matnr,
        END OF tl_lips.

  DATA: BEGIN OF tl_0093 OCCURS 0,
          matnr TYPE mara-matnr,
        END OF tl_0093.

  DATA: lv_cont     TYPE i,
        lv_material TYPE zreg_exportacao-cd_material.

  CLEAR: lv_cont.

* Encontra os materiais da remessa
  SELECT *
    FROM lips
    INTO CORRESPONDING FIELDS OF TABLE tl_lips
    WHERE vbeln EQ wa_vc_doc_exp-vbeln.

* Encontra os materiais do RE
  SELECT *
    FROM zreg_exportacao
    INTO CORRESPONDING FIELDS OF TABLE tl_exportacao
    WHERE nr_registro_expo EQ wa_vc_doc_exp-nr_registro_expo.

* Verifica se a remessa e RE possuem os mesmo materiais
  LOOP AT tl_lips.
    SHIFT tl_lips-matnr LEFT DELETING LEADING '0'.

    READ TABLE tl_exportacao WITH KEY cd_material = tl_lips-matnr.

    IF sy-subrc IS INITIAL.
      ADD 1 TO lv_cont.
    ENDIF.
  ENDLOOP.

  IF lv_cont IS INITIAL.
*   Verifica se os materiais RE tem vinculo na tabela de De/Para zsdt0093
    SELECT *
      FROM zsdt0093
      JOIN mara ON mara~matkl EQ zsdt0093~matkl
      INTO CORRESPONDING FIELDS OF TABLE tl_0093
      FOR ALL ENTRIES IN tl_exportacao
      WHERE matnr_com EQ tl_exportacao-cd_material.

    IF tl_0093[] IS NOT INITIAL.
      LOOP AT tl_lips.
        READ TABLE tl_0093 WITH KEY matnr = tl_lips-matnr.
        IF sy-subrc IS INITIAL.
          ADD 1 TO lv_cont.
        ENDIF.
      ENDLOOP.
    ENDIF.

*   Interrompe o processo caso não haja vinculo entre os materiais
    IF lv_cont IS INITIAL.
      MESSAGE TEXT-007 TYPE 'E'.
      CLEAR: wa_vc_doc_exp-vbeln, wa_vc_doc_exp-nr_registro_expo.
    ENDIF.
  ENDIF.

ENDFORM.                    " Z_VALIDA_MATERIAIS

FORM z_preenche_dde.

  IF wa_vc_doc_exp-nr_registro_expo IS NOT INITIAL.
    SELECT SINGLE id_registro_expo id_nomeacao_tran cd_material
      INTO (wa_vc_doc_exp-id_registro_expo, wa_vc_doc_exp-id_nomeacao_tran, wa_vc_doc_exp-cd_material)
      FROM zreg_exportacao
     WHERE nr_registro_expo EQ wa_vc_doc_exp-nr_registro_expo
       AND in_status_comex  NE 'X'.
  ENDIF.

  IF wa_vc_doc_exp-numero_due IS NOT INITIAL.
    SELECT SINGLE id_due id_nomeacao_tran
      INTO (wa_vc_doc_exp-id_due, wa_vc_doc_exp-id_nomeacao_tran)
      FROM zsdt0170
     WHERE numero_due EQ wa_vc_doc_exp-numero_due
       AND status      EQ '1'
       AND lcto_avulso EQ abap_true.
  ENDIF.

  IF wa_vc_doc_exp-id_registro_expo IS NOT INITIAL.
    SELECT SINGLE id_dde
      FROM zdde_aplicacao INTO wa_vc_doc_exp-id_dde
    WHERE  id_registro_expo EQ wa_vc_doc_exp-id_registro_expo.
  ENDIF.

  IF wa_vc_doc_exp-id_nomeacao_tran IS NOT INITIAL.
    SELECT SINGLE ds_nome_transpor
      FROM znom_transporte INTO wa_vc_doc_exp-desc
    WHERE  id_nomeacao_tran EQ wa_vc_doc_exp-id_nomeacao_tran.
  ENDIF.

  IF wa_vc_doc_exp-id_dde IS NOT INITIAL.
    SELECT SINGLE nr_dde
      FROM zdde INTO wa_vc_doc_exp-dde
    WHERE  id_dde EQ wa_vc_doc_exp-id_dde.
  ENDIF.

ENDFORM.                    " Z_PREENCHE_DDE

MODULE zm_re INPUT.

  PERFORM z_verifica_re.

ENDMODULE.                 " ZM_RE  INPUT

MODULE zm_due INPUT.

  PERFORM z_verifica_due.

ENDMODULE.                 " ZM_RE  INPUT


MODULE zm_user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'DESVINC'.
      PERFORM z_desvinc_re_due.
    WHEN 'SAVE'.
      PERFORM z_save.
  ENDCASE.

  CLEAR: sy-ucomm.


ENDMODULE.                 " ZM_USER_COMMAND  INPUT

MODULE zm_exit_command_0100 INPUT.
  DATA: vl_answer TYPE c LENGTH 1.

  CASE sy-dynnr.
    WHEN '0100'.
      CASE sy-ucomm.
        WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_EXIT_COMMAND  INPUT

FORM z_save.

  DATA vl_answer TYPE char1.

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

  CHECK vl_answer EQ '1'.

  IF wa_vc_doc_exp-id_registro_expo IS NOT INITIAL AND wa_vc_doc_exp-id_due IS NOT INITIAL.
    MESSAGE 'Informe a DU-e ou o R.E' TYPE 'I'.
    EXIT.
  ENDIF.

  IF wa_vc_doc_exp-id_registro_expo IS INITIAL AND wa_vc_doc_exp-id_due IS INITIAL.
    MESSAGE 'Informe a DU-e ou o R.E' TYPE 'I'.
    EXIT.
  ENDIF.

  IF wa_vc_doc_exp-nr_registro_expo IS NOT INITIAL AND wa_vc_doc_exp-numero_due IS NOT INITIAL.
    MESSAGE 'Informe a DU-e ou o R.E' TYPE 'I'.
    EXIT.
  ENDIF.

  IF wa_vc_doc_exp-nr_registro_expo IS INITIAL AND wa_vc_doc_exp-numero_due IS INITIAL.
    MESSAGE 'Informe a DU-e ou o R.E' TYPE 'I'.
    EXIT.
  ENDIF.

* Desvincular das Tabelas
  PERFORM: z_desvincular_tab,
* Vincular Tabelas
           z_vincular_tab.

*   Preenche Table Controls
*  PERFORM z_preenche_controls.

  REFRESH: t_conhec_a,
           t_conhec_v,
           t_full.

  CLEAR: wa_vc_doc_exp.

  MESSAGE i836(sd) WITH TEXT-011.

  LEAVE TO SCREEN 0.

ENDFORM.                    " Z_SAVE

FORM z_desvincular_tab.

  DATA: tl_conhec    LIKE TABLE OF t_conhec_a,
        sl_conhec    LIKE LINE OF  t_conhec_a,
        vl_id_doc    TYPE zid_doc,
        wl_reme_nota TYPE znom_reme_notas,
        wl_remet     TYPE znom_remetente.

  tl_conhec[] = t_conhec_a[].
  DELETE tl_conhec WHERE id EQ 0.

  CHECK NOT tl_conhec[] IS INITIAL.

  LOOP AT tl_conhec INTO sl_conhec.
    DELETE FROM zdoc_rem_bl
      WHERE id_doc_exp  EQ sl_conhec-id
        AND id_doc_item EQ sl_conhec-item.
    CLEAR sl_conhec.
  ENDLOOP.

  SORT tl_conhec BY id ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_conhec COMPARING id.

  MESSAGE i836(sd) WITH TEXT-010.

ENDFORM.                    " Z_DESVINCULAR_TAB

FORM z_vincular_tab.

  DATA: tl_conhec  LIKE TABLE OF t_conhec_a,
        sl_conhec  LIKE LINE OF  t_conhec_a,
        sl_doc     TYPE zdoc_exp,
        sl_rem     TYPE zdoc_rem_bl,
        vl_id_item TYPE zid_item,
        vl_id_doc  TYPE zid_doc.

  CLEAR: vl_id_doc, vl_id_item.

  IF ( wa_vc_doc_exp-vbeln            IS NOT INITIAL ) AND
     ( wa_vc_doc_exp-id_registro_expo IS NOT INITIAL ).

    SELECT SINGLE id_doc_exp
      FROM zdoc_exp INTO vl_id_doc
     WHERE vbeln            EQ wa_vc_doc_exp-vbeln
       AND id_registro_expo EQ wa_vc_doc_exp-id_registro_expo.

  ELSEIF ( wa_vc_doc_exp-vbeln  IS NOT INITIAL ) AND
         ( wa_vc_doc_exp-id_due IS NOT INITIAL ).

    SELECT SINGLE id_doc_exp
      FROM zdoc_exp INTO vl_id_doc
     WHERE vbeln   EQ wa_vc_doc_exp-vbeln
       AND id_due  EQ wa_vc_doc_exp-id_due.

  ELSE.
    RETURN.
  ENDIF.

  IF sy-subrc NE 0.
    SELECT MAX( id_doc_exp ) INTO vl_id_doc
      FROM zdoc_exp.
    IF vl_id_doc IS INITIAL.
      vl_id_doc = 1.
    ELSE.
      ADD 1 TO vl_id_doc.
    ENDIF.
    sl_doc-id_doc_exp       = vl_id_doc.
    sl_doc-vbeln            = wa_vc_doc_exp-vbeln.
    sl_doc-id_registro_expo = wa_vc_doc_exp-id_registro_expo.
    sl_doc-nr_registro_expo = wa_vc_doc_exp-nr_registro_expo.
    sl_doc-id_dde           = wa_vc_doc_exp-id_dde.
    sl_doc-nr_dde           = wa_vc_doc_exp-dde.
    sl_doc-id_due           = wa_vc_doc_exp-id_due.
    sl_doc-numero_due       = wa_vc_doc_exp-numero_due.
    sl_doc-id_nomeacao_tran = wa_vc_doc_exp-id_nomeacao_tran.
    INSERT zdoc_exp FROM sl_doc.
    vl_id_item = 0.
  ELSE.
    SELECT MAX( id_doc_item )
      FROM zdoc_rem_bl
      INTO vl_id_item
    WHERE  id_doc_exp EQ vl_id_doc.
  ENDIF.

  tl_conhec[] = t_conhec_v[].
  DELETE tl_conhec WHERE id NE 0.

  CHECK tl_conhec[] IS NOT INITIAL.

  LOOP AT tl_conhec INTO sl_conhec.

    ADD 1 TO vl_id_item.
    sl_rem-id_doc_exp       = vl_id_doc.
    sl_rem-id_doc_item      = vl_id_item.
    sl_rem-id_conhec        = sl_conhec-id_conhec.
    sl_rem-id_nomeacao_tran = sl_conhec-id_nomeacao_tran.
    sl_rem-dt_data          = sl_conhec-dt_data.
    sl_rem-nr_qtde          = sl_conhec-nr_qtde.
    sl_rem-nr_conhec        = sl_conhec-nr_conhec.

    INSERT zdoc_rem_bl FROM sl_rem.

    CLEAR: sl_conhec,
           sl_doc   ,
           sl_rem   .

  ENDLOOP.

  MESSAGE i836(sd) WITH TEXT-011.


ENDFORM.                    " Z_VINCULAR_TAB

FORM z_desvinc_re_due.

  DATA: vl_id_doc  TYPE zid_doc.

  SELECT SINGLE id_doc_exp
    FROM zdoc_exp INTO vl_id_doc
   WHERE vbeln EQ wa_vc_doc_exp-vbeln.

  CHECK ( sy-subrc EQ 0 ) AND ( vl_id_doc IS NOT INITIAL ).

  DELETE FROM zdoc_exp    WHERE id_doc_exp = vl_id_doc.
  DELETE FROM zdoc_rem_bl WHERE id_doc_exp = vl_id_doc.

  MESSAGE i836(sd) WITH TEXT-013.

  REFRESH: t_conhec_a,
           t_conhec_v,
           t_full.

  CLEAR: wa_vc_doc_exp.

  LEAVE TO SCREEN 0.

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
      WHERE arbgb = it_msg-msgid AND
            msgnr = it_msg-msgnr AND
            sprsl = sy-langu.

    LOOP AT it_msgtext.
      TRANSLATE it_msgtext-texto USING '& '.
      CONDENSE it_msgtext-texto.
      MODIFY it_msgtext.
    ENDLOOP.
  ENDIF.

  CLEAR: it_dta[].
  REFRESH: it_dta.

ENDFORM.                    "F_CALL_TRANSACTION

FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.
  CLEAR wa_dta.
  wa_dta-program   = p_program.
  wa_dta-dynpro    = p_dynpro.
  wa_dta-dynbegin  = p_start.
  wa_dta-fnam      = p_fnam.
  wa_dta-fval      = p_fval.
  APPEND wa_dta TO it_dta.
ENDFORM.                    "F_BDC_DATA

FORM f_vinc_aut_due_rem  USING p_vbeln
                               p_numero_ruc
                      CHANGING p_continue.

  CHECK ( p_vbeln IS NOT INITIAL ) AND ( p_numero_ruc IS NOT INITIAL ).

  SELECT SINGLE *
    FROM lips INTO @DATA(lwa_lips)
   WHERE vbeln EQ @p_vbeln.

  IF sy-subrc NE 0.
    p_continue = abap_false.
    MESSAGE |Remessa { p_vbeln } não encontrada!| TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM zdoc_exp INTO @DATA(lwa_zdoc_exp)
   WHERE vbeln EQ @lwa_lips-vbeln.

  CHECK ( sy-subrc EQ 0 ) AND ( lwa_zdoc_exp-id_due IS INITIAL ).

  SELECT SINGLE *
    FROM vbrp INTO @DATA(lwa_vbrp)
   WHERE vgbel EQ @p_vbeln.

  IF sy-subrc NE 0.
    "MESSAGE |Fatura remessa { P_VBELN } não encontrada!| TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM j_1bnflin INTO @DATA(lwa_lin_exp)
   WHERE refkey EQ @lwa_vbrp-vbeln.

  IF sy-subrc NE 0.
    "MESSAGE |Documento Fiscal Fatura { LWA_VBRP-VBELN } não encontrado!| TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM j_1bnfe_active INTO @DATA(lwa_act_exp)
   WHERE docnum EQ @lwa_lin_exp-docnum.

  IF sy-subrc NE 0.
    "MESSAGE |Documento Fiscal Fatura { LWA_VBRP-VBELN } não encontrado!| TYPE 'I'.
    EXIT.
  ENDIF.

  CHECK ( lwa_act_exp-docsta EQ '1' ) AND ( lwa_act_exp-scssta NE '2' ).

  p_continue = abap_false.

  SELECT SINGLE *
    FROM zsdt0170 INTO @DATA(lwa_zsdt0170)
   WHERE numero_ruc EQ @p_numero_ruc
     AND loekz      EQ @abap_false.

  IF sy-subrc NE 0.
    MESSAGE |Não foi importada a DU-e para a RUC { p_numero_ruc } !| TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM zsdt0172 INTO @DATA(lwa_zsdt0172)
   WHERE id_due   EQ @lwa_zsdt0170-id_due
     AND docnum   EQ @lwa_lin_exp-docnum.

  IF sy-subrc NE 0.
    MESSAGE |DU-e Id: { lwa_zsdt0170-id_due } e RUC { p_numero_ruc }, não possui o Doc. Fiscal { lwa_lin_exp-docnum } !| TYPE 'I'.
    EXIT.
  ENDIF.

  UPDATE zdoc_exp SET id_due      = lwa_zsdt0170-id_due
                      numero_due  = lwa_zsdt0170-numero_due
                      id_nomeacao_tran = lwa_zsdt0170-id_nomeacao_tran
   WHERE vbeln EQ p_vbeln.

  UPDATE zdoc_nf_produtor SET id_nomeacao_tran = lwa_zsdt0170-id_nomeacao_tran
    WHERE vbeln EQ p_vbeln.

  UPDATE zsdt0053 SET id_nomeacao_tran = lwa_zsdt0170-id_nomeacao_tran
   WHERE vbeln EQ lwa_lips-vgbel.

  MESSAGE |Remessa { p_vbeln } vinculada a DU-e ID { lwa_zsdt0170-id_due } !| TYPE 'S'.

ENDFORM.

**********************************************************************
* 102756 CS2023000064 Criar opção a anexo de contrato nas linhas da ZSDT0066 pelo numero da solicitação - PSA
**********************************************************************
FORM f_anexa_visualiza_arquivo.

  DATA: ls_object    TYPE sibflporb,
        save_request TYPE sgs_flag.

  ls_object-instid = wg_saida-nro_sol_ov.
  ls_object-typeid = sy-cprog.
  ls_object-catid = 'BO'.

  CALL FUNCTION 'GOS_ATTACHMENT_LIST_POPUP'
    EXPORTING
      is_object       = ls_object
      ip_mode         = 'E' " Edit mode
    IMPORTING
      ep_save_request = save_request.
  IF save_request = 'X'.
    COMMIT WORK.
  ENDIF.

  "Atualiza Tela da ALV
  obj_ret->refresh( ).


ENDFORM.
**********************************************************************
* 102756 CS2023000064 Criar opção a anexo de contrato nas linhas da ZSDT0066 pelo numero da solicitação - PSA
**********************************************************************
FORM f_verifiva_icone_arquivo.

  DATA:
    lv_logical_system	    TYPE bapibds01-log_system, "
    lt_gos_connections    TYPE STANDARD TABLE OF bdn_con, "
    lv_no_objects_found	  TYPE bdn_con, "
    lv_classname          TYPE bapibds01-classname, "
    lv_internal_error	    TYPE bapibds01, "
    lv_objkey	            TYPE swotobjid-objkey, "
    lv_internal_gos_error	TYPE swotobjid, "
    lv_client	            TYPE sy-mandt. "   SY-MANDT

  lv_classname = sy-cprog.
  lv_objkey = wg_saida-nro_sol_ov.
  lv_client = sy-mandt.

  CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'  "
    EXPORTING
      classname          = lv_classname
      objkey             = lv_objkey
      client             = lv_client
    TABLES
      gos_connections    = lt_gos_connections
    EXCEPTIONS
      no_objects_found   = 1
      internal_error     = 2
      internal_gos_error = 3.

  IF lines( lt_gos_connections ) > 0.
    wg_saida-anexos = icon_attachment.
  ELSE.
    wg_saida-anexos = icon_aggregate.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_FLUXO_SD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprimir_fluxo_sd .

  DATA: layout          TYPE slis_layout_alv.
  PERFORM definir_eventos.

  PERFORM montar_layout USING 'TG_FLUXOSD'.
  layout-box_fieldname = 'MARK'.
  layout-box_tabname  = 'TG_FLUXOSD'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = v_report
      i_callback_user_command = 'XUSER_COMMAND'
      it_fieldcat             = estrutura[]
      it_sort                 = t_sort[]
      i_save                  = 'A'
      it_events               = events
      is_layout               = layout
      is_print                = t_print
    TABLES
      t_outtab                = tg_fluxosd.

ENDFORM.

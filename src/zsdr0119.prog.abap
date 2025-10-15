**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**|  Funcional:                                                               |*
**|    + Paulo Quevedo ( paulo.quebdo@amaggi.com.br )                         |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| JOB de envio de e-mail diferença de Hedge                                 |*
**| ZSDT0062, ZSDT0044, ZMM0149, ZLES0077                                     |*
**/===========================================================================\*

REPORT zsdr0119 MESSAGE-ID zjob.

TABLES: zsdt0051, sscrfields." 28.03.2025 - RAMON - 170634

*PARAMETERS r_new RADIOBUTTON GROUP rad2 USER-COMMAND act DEFAULT 'X' .
*PARAMETERS r_old RADIOBUTTON GROUP rad2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS r_01 RADIOBUTTON GROUP rad2 USER-COMMAND act.
    SELECTION-SCREEN COMMENT 03(79) TEXT-i01.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS r_02 RADIOBUTTON GROUP rad2.
    SELECTION-SCREEN COMMENT 03(79) TEXT-i02.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS r_03 RADIOBUTTON GROUP rad2.
    SELECTION-SCREEN COMMENT 03(79) TEXT-i03.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS r_04 RADIOBUTTON GROUP rad2.
    SELECTION-SCREEN COMMENT 03(79) TEXT-i04.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS r_05 RADIOBUTTON GROUP rad2 DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 03(79) TEXT-i05.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs FOR zsdt0051-vkorg NO-EXTENSION NO INTERVALS,
                  s_solov FOR zsdt0051-nro_sol_ov NO-EXTENSION NO INTERVALS,
                  s_data  FOR zsdt0051-data_atual.
SELECTION-SCREEN: END OF BLOCK b1.

PARAMETERS p_ant AS CHECKBOX." #debug ramon

SELECTION-SCREEN: FUNCTION KEY 1. " 28.03.2025 - RAMON -170634

TYPE-POOLS slis.

TYPES: BEGIN OF ty_0053,
         nro_sol_ov TYPE zsdt0053-nro_sol_ov,
         fixacao    TYPE zsdt0053-fixacao,
         dmbtr      TYPE zsdt0053-dmbtr,
         vlrtot     TYPE zsdt0053-vlrtot,
         status     TYPE zsdt0053-status,
         status_itm TYPE zsdt0053-status_itm,
         pmein      TYPE zsdt0053-pmein,
         zmeng      TYPE zsdt0053-zmeng,
       END OF ty_0053,

       BEGIN OF ty_0055,
         nro_sol_ov   TYPE zsdt0055-nro_sol_ov,
         fixacao      TYPE zsdt0055-fixacao,
         cadencia_qte TYPE zsdt0055-cadencia_qte,
         zieme        TYPE zsdt0055-zieme,
       END OF ty_0055,

       BEGIN OF ty_0051,
         nro_sol_ov   TYPE zsdt0051-nro_sol_ov,
         " 19.01.2024 - 131275 - RBL -->
         matnr        TYPE zsdt0051-matnr,
         matkl        TYPE matkl,
         " 19.01.2024 - 131275 - RBL --<
         tp_venda     TYPE zsdt0051-tp_venda,
         status       TYPE zsdt0051-status,
         param_espec  TYPE zsdt0051-param_espec,
         inco1        TYPE zsdt0051-inco1,
         risco_sacado TYPE zsdt0051-risco_sacado,
         waerk        TYPE zsdt0051-waerk,
         auart        TYPE zsdt0051-auart,
       END OF ty_0051,

       BEGIN OF ty_0059,
         nro_sol_ov TYPE zsdt0059-nro_sol_ov,
         bezei      TYPE zsdt0059-bezei,
         cod_fp     TYPE zsdt0059-cod_fp,
         posnr      TYPE zsdt0059-posnr,
         formula2   TYPE zsdt0059-formula2,
         field      TYPE zsdt0059-field,
       END OF ty_0059,

       BEGIN OF ty_0094,
         nro_sol_ov    TYPE zsdt0094-nro_sol_ov,
         fixacao       TYPE zsdt0094-fixacao,
         total_proporc TYPE zsdt0094-total_proporc,
         taxa_cambio   TYPE zsdt0094-taxa_cambio,
         tipo          TYPE zsdt0094-tipo,
         bezei         TYPE zsdt0094-bezei,
         tipo_taxa     TYPE zsdt0094-tipo_taxa,
       END OF ty_0094,

       BEGIN OF ty_0037,
         bukrs          TYPE zsdt0037-bukrs,
         matkl          TYPE zsdt0037-matkl,
         filial_origem  TYPE zsdt0037-filial_origem,
         meins          TYPE zsdt0037-meins,
         filial_destino TYPE zsdt0037-filial_destino,
         vlr_frete      TYPE zsdt0037-vlr_frete,
       END OF ty_0037,

       BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         matkl TYPE mara-matkl,
         brgew TYPE mara-brgew,
       END OF ty_mara,

       BEGIN OF ty_vbak,
         vbeln   TYPE vbak-vbeln,
         vbeln_o TYPE vbak-vbeln,
         knumv   TYPE vbak-knumv,
         doc_sim TYPE zsded003,
       END OF ty_vbak,

       BEGIN OF ty_vbap,
         vbeln  TYPE vbap-vbeln,
         posnr  TYPE vbap-posnr,
         matnr  TYPE vbap-matnr,
         spart  TYPE vbap-spart,
         netwr  TYPE vbap-netwr,
         brgew  TYPE vbap-brgew,
         mwsbp  TYPE vbap-mwsbp,
         kwmeng TYPE vbap-kwmeng,
         lifsp  TYPE vbep-lifsp,
       END OF ty_vbap,

       BEGIN OF ty_konv,
         knumv TYPE konv-knumv,
         kposn TYPE konv-kposn,
         kschl TYPE konv-kschl,
         kbetr TYPE konv-kbetr,
       END OF ty_konv,

       BEGIN OF ty_0117,
         bukrs TYPE zsdt0117-bukrs,
         kursk TYPE zsdt0117-kursk,
       END OF ty_0117,

       BEGIN OF ty_log,
         seq     TYPE int4,
         message TYPE char255,
       END OF ty_log,

       BEGIN OF ty_90,
         docsi TYPE zsdt0090-doc_simulacao,
         vbeln TYPE zsdt0090-vbelv,
         matnr TYPE zsdt0090-matnrv,
         matkl TYPE zsdt0090-matklv,
         werks TYPE zsdt0090-werksv,
         kmein TYPE zsdt0090-kmeinv,
         zieme TYPE zsdt0090-ziemev,
         zmeng TYPE zsdt0090-zmengv,
         netpr TYPE zsdt0090-netprv,
         inco1 TYPE zsdt0090-inco1,
         spart TYPE zsdt0090-spart,
         netwr TYPE zsdt0090-netwr,
       END OF ty_90,

       BEGIN OF ty_saida,
         area       TYPE char10,
         nro_sol_ov TYPE zsdt0094-nro_sol_ov,
         fixacao    TYPE posnr,
         tp_venda   TYPE zsded012,
         spart      TYPE spart, " ramon #debug
         tpsim      TYPE char2,
         waerk      TYPE zsdt0051-waerk,
         status	    TYPE zsded021,
         bezei      TYPE char10,
         tipo       TYPE char3,
         vl94       TYPE zdmbtr,
         vl59       TYPE zdmbtr,
         diferenca  TYPE zdmbtr,

         " 26.03.2025 - 170634 - RAMON -->
         sem_email  TYPE flag,
         " 26.03.2025 - 170634 - RAMON --<

       END OF ty_saida.

TYPES BEGIN OF ty_0041.
INCLUDE TYPE zsdt0041.
TYPES: matkl     TYPE matkl,
       brgew     TYPE brgew,
       dtpgtcult TYPE sy-datum,
       kursk     TYPE kursk,
       END OF ty_0041.

DATA: r_nro_sol_ov    TYPE zrsdsselopts,
      at_datainicial  TYPE sy-datum,

      it_0040         TYPE TABLE OF zsdt0040,
      it_0041         TYPE TABLE OF zsdt0041,
      it_41           TYPE TABLE OF ty_0041,
      it_41_aux       TYPE TABLE OF ty_0041,
      it_0090         TYPE TABLE OF zsdt0090,
      it_vbrk         TYPE TABLE OF vbrk,
      it_ekko         TYPE TABLE OF ekko,
      it_ekpo         TYPE TABLE OF ekpo,
      it_ekpo_aux     TYPE TABLE OF ekpo,
      it_zmmt0035     TYPE TABLE OF zmmt0035,
      wa_zmmt0035     TYPE zmmt0035,
      it_zmmt0037     TYPE TABLE OF zmmt0037,
      wa_zmmt0037     TYPE zmmt0037,
      it_zsdt0094     TYPE TABLE OF zsdt0094,
      it_t052         TYPE TABLE OF t052,
      it_kna1         TYPE TABLE OF kna1,
      it_vbrp         TYPE TABLE OF vbrp,
      l_0090          TYPE ty_90,
      it_mara         TYPE TABLE OF ty_mara,
      it_vbak         TYPE TABLE OF ty_vbak,
      it_vbap         TYPE TABLE OF ty_vbap,
      it_konv         TYPE TABLE OF ty_konv,
      it_0051         TYPE TABLE OF ty_0051,
      it_0053         TYPE TABLE OF ty_0053,
      it_0055         TYPE TABLE OF ty_0055,
      it_0059         TYPE TABLE OF ty_0059,
      it_0037         TYPE TABLE OF ty_0037,
      it_0308         TYPE TABLE OF zsdt0308,
      wa_0037         TYPE ty_0037,
      wa_0037_aux     TYPE ty_0037,
      it_0094         TYPE TABLE OF ty_0094,
      it_0117         TYPE TABLE OF ty_0117,
      it_saida        TYPE TABLE OF ty_saida,
      it_fcat         TYPE slis_t_fieldcat_alv,
      it_log          TYPE TABLE OF ty_log,
      it_bkk          TYPE bkk_tab_mesg,

      sum_a           TYPE netpr,
      calc_m          TYPE netpr,
      sum_m           TYPE netpr,
      sum_            TYPE netpr,
      sum_c           TYPE netpr,
      sum_r           TYPE netpr,
      sum_e           TYPE netpr,
      sum_y           TYPE netpr,
      sum_f           TYPE netpr,
      l_vl59a         TYPE zdmbtr,
      l_vl59b         TYPE zdmbtr,

      container1      TYPE REF TO cl_gui_custom_container,
      grid1           TYPE REF TO cl_gui_alv_grid,
      _stable         TYPE lvc_s_stbl VALUE 'XX',
      _index          TYPE lvc_t_row,
      _layout         TYPE lvc_s_layo,
      t_fcat          TYPE lvc_t_fcat,
      _saida          TYPE ty_saida,
      lv_sub          TYPE so_obj_des,
      vg_job          TYPE i,
      job_ok          TYPE c,
      seq             TYPE int4,
      txt_log         TYPE string,
      t_html          TYPE zstring,
      porcentagem_fre TYPE p DECIMALS 5,
      vl_kursk        TYPE zsdt0117-kursk,
      l_valor         TYPE komp-netwr,
      l_wmwst         TYPE komp-netwr.

" 26.03.2025 - 170634 - RAMON -->
DATA gt_zsdt0373 TYPE TABLE OF zsdt0373.
" 26.03.2025 - 170634 - RAMON <--

DATA var_ckeck TYPE c.

" 03.04.2025 - 170634 - RAMON -->
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION .
    METHODS:
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command_itens FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.

CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD on_toolbar.
    PERFORM f_toolbar USING e_object.
  ENDMETHOD.

  METHOD handle_user_command_itens.
    PERFORM f_marcar_linha USING e_ucomm.
  ENDMETHOD.


ENDCLASS.
" 03.04.2025 - 170634 - RAMON --<

CLASS lcl_event_handler DEFINITION .
  PUBLIC SECTION .
    METHODS:

*      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
*        IMPORTING e_row e_column es_row_no.
*
*    "handle_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid.

      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    "handle_click_changed for EVENT

ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD  handle_data_changed.
    PERFORM f_data_changed USING er_data_changed.
  ENDMETHOD.                    "on_user_command

*  METHOD handle_top_of_page.
*    PERFORM f_top_of_page.
*  ENDMETHOD.

*  METHOD handle_data_changed.
*    PERFORM f_on_data_changed USING er_data_changed.
*  ENDMETHOD.

ENDCLASS.                    "lcl_handle_events DEFINITION

" 03.04.2025 - 170634 - RAMON -->
DATA lo_toolbar TYPE REF TO lcl_alv_toolbar.
" 03.04.2025 - 170634 - RAMON -->

INITIALIZATION.
  PERFORM f_botao_function. " 28.03.2025 - RAMON - 170634


  DATA(obj_tx_curva) = NEW zcl_taxa_curva( ).
  DATA(obj)          = NEW zcl_taxa_curva_db( ).

  at_datainicial = sy-datum - 730.

  IF at_datainicial <= '20200101'.
    at_datainicial = '20200101'.
  ENDIF.

  seq = 0.
  job_ok = abap_false.

  " 09.07.2024 - RAMON -->
  PERFORM f_screen_modify.

AT SELECTION-SCREEN OUTPUT.
  PERFORM f_screen_modify.
  " 09.07.2024 - RAMON <--

  " 28.03.2025 - RAMON - 170634 -->

AT SELECTION-SCREEN.
  PERFORM f_botao_command.
  " 28.03.2025 - RAMON - 170634 --<

START-OF-SELECTION.

  IF r_02 = abap_true AND p_ant = abap_false.

    SUBMIT zsdr0119_in
            WITH rb_in = abap_true
            WITH rb_mi = abap_false
            WITH rb_mm = abap_false
            WITH rb_sa = abap_false
            WITH rb_todos = abap_false
            WITH so_vkorg IN s_bukrs
            WITH so_data IN s_data
            WITH so_sim IN s_solov AND RETURN.

    EXIT.

  ELSE.

    var_ckeck = abap_true.

  ENDIF.

  IF s_data IS INITIAL.
    APPEND VALUE #( sign = 'I' option = 'GT' low = at_datainicial high = sy-datum ) TO s_data.
  ENDIF.

  IF sy-batch EQ abap_true.

    job_ok = abap_true.

    ADD 1 TO seq.
    APPEND VALUE #( seq = seq message = |Executado Pelo JOB "ZSDR0119_DIF_HEDGE_JOB".| ) TO it_log.

    SELECT SINGLE COUNT( * ) INTO vg_job
      FROM tbtco
     WHERE jobname EQ 'ZSDR0119_DIF_HEDGE_JOB'
    AND status EQ 'R'.

    IF vg_job EQ 1.
*  Processo do Mercado Interno
      FREE it_saida.
      PERFORM selecionar_dados_mi.
      PERFORM tratar_dados_mi.
      PERFORM montar_htm USING 'MI'.

* Processo do INSUMOS
      FREE it_saida.
      PERFORM selecionar_dados_in.
      PERFORM tratar_dados_in.
*      PERFORM vencimento_in.
      PERFORM montar_htm  USING 'IN'.

* Processo do AQUAVIARIO, SERVIÇOS PORTUARIOS, TRANSBORDO.
      FREE it_saida.
      PERFORM seleciona_dados_vbrk.
      PERFORM trata_dados_vbrk.
      PERFORM montar_htm  USING 'AQ'.

* Processo de Pedido ZMM0149 ME22N ME29N ME21N
      FREE it_saida.
      PERFORM seleciona_dados_ekko.
      PERFORM trata_dados_ekko.
      PERFORM montar_htm  USING 'PD'.

    ENDIF.

    SORT it_log BY seq.
    LOOP AT it_log INTO DATA(w_log).
      WRITE: w_log-message.
      MESSAGE s000 WITH w_log-message.
    ENDLOOP.

  ELSE.

    PERFORM progress USING 1 'Executando Transação!'.
    PERFORM busca_dados.
    PERFORM imprimir_dados_mi.

  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selecionar_dados_mi.

  DATA(r_tpvenda) = obj_tx_curva->get_auart( 'MAGGI_ZSDT0062_HEDGE' ).  "// Tipo de Venda aceito para Disparar o Hedge

  IF r_tpvenda IS INITIAL.
    IF job_ok IS INITIAL.
      MESSAGE s000 WITH |TP VDA não Param no Set "MAGGI_ZSDT0062_HEDGE"!|.
    ELSE.
      ADD 1 TO seq.
      APPEND VALUE #( seq = seq message = |Tipo de Venda não Parametrizado no Set "MAGGI_ZSDT0062_HEDGE"!| ) TO it_log.
    ENDIF.
    EXIT.
  ENDIF.

  PERFORM progress USING 5 'Seleção de Dados!!'.

  SELECT
     'I' AS sign,
     'EQ' AS option,
      nro_sol_ov AS low
    FROM zsdt0051
   INTO TABLE @r_nro_sol_ov
      WHERE "data_atual >= @at_datainicial
             data_atual IN @s_data
         AND tp_venda IN @r_tpvenda
         AND vkorg    IN @s_bukrs
  AND nro_sol_ov IN @s_solov.

*    AND TP_VENDA EQ '00153'
*    AND NRO_SOL_OV = '0000567469'

  "// '00002, 00003, 00023, 00027, 00061, 00091, 00153, 00163'

  SORT r_nro_sol_ov BY low.
  DELETE ADJACENT DUPLICATES FROM r_nro_sol_ov COMPARING low.

  IF r_nro_sol_ov IS INITIAL.
    IF job_ok IS INITIAL.
      MESSAGE s000 WITH |Não existe Solicitação de Venda! |.
    ELSE.
      ADD 1 TO seq.
      APPEND VALUE #( seq = seq message = |Não existe Solicitação de Venda! | ) TO it_log.
    ENDIF.
    EXIT.
  ENDIF.

  " 19.01.2024 - 131275 - RBL -->>
*  SELECT *
*    FROM zsdt0051
*     INTO CORRESPONDING FIELDS OF TABLE @it_0051
*      WHERE nro_sol_ov IN @r_nro_sol_ov.

  SELECT nro_sol_ov zsdt0051~matnr matkl
         tp_venda status param_espec
         inco1 risco_sacado waerk auart
    FROM zsdt0051
    INNER JOIN mara ON zsdt0051~matnr = mara~matnr
     INTO CORRESPONDING FIELDS OF TABLE it_0051
  WHERE nro_sol_ov IN r_nro_sol_ov.
  " 19.01.2024 - 131275 - RBL --<<



  PERFORM progress USING 10 'Seleção de Dados!!'.

  SELECT *
    FROM zsdt0053 AS a
    INTO CORRESPONDING FIELDS OF TABLE @it_0053
  WHERE a~nro_sol_ov IN @r_nro_sol_ov.

  DATA(it_0053_aux) = it_0053.

  SORT it_0053_aux BY nro_sol_ov fixacao dmbtr.
  DELETE ADJACENT DUPLICATES FROM it_0053_aux COMPARING nro_sol_ov fixacao dmbtr.

  LOOP AT it_0053_aux ASSIGNING FIELD-SYMBOL(<l_0053>).

    <l_0053>-vlrtot = REDUCE #(
                                INIT x TYPE dmbtr
                                  FOR ls2 IN it_0053
                                   WHERE ( nro_sol_ov EQ  <l_0053>-nro_sol_ov
                                    AND fixacao EQ <l_0053>-fixacao
                                    AND status NE 'C' )
                                  NEXT x = x + ls2-vlrtot
                              ).

  ENDLOOP.

  it_0053 = it_0053_aux.

  IF it_0053 IS INITIAL.
    IF job_ok IS INITIAL.
      MESSAGE s000 WITH |Não existe Item para a Solicitação de Venda! |.
    ELSE.
      ADD 1 TO seq.
      APPEND VALUE #( seq = seq message = |Não existe Item para a Solicitação de Venda! | ) TO it_log.
    ENDIF.
    EXIT.
  ENDIF.

  PERFORM progress USING 15 'Seleção de Dados!!'.

  SELECT a~nro_sol_ov, a~fixacao, a~cadencia_qte, a~zieme
  FROM zsdt0055 AS a
  INTO TABLE @it_0055
  WHERE a~nro_sol_ov IN @r_nro_sol_ov.

  DATA(it_0055_aux) = it_0055.

  SORT it_0055_aux BY nro_sol_ov fixacao.
  DELETE ADJACENT DUPLICATES FROM it_0055_aux COMPARING nro_sol_ov fixacao.

  LOOP AT it_0055_aux ASSIGNING FIELD-SYMBOL(<l_0055>).
    <l_0055>-cadencia_qte = REDUCE #(
                                INIT y TYPE zsded016
                                  FOR ls3 IN it_0055
                                   WHERE ( nro_sol_ov EQ  <l_0055>-nro_sol_ov
                                    AND fixacao EQ <l_0055>-fixacao )
                                  NEXT y = y + ls3-cadencia_qte
                              ).
  ENDLOOP.

  PERFORM progress USING 20 'Seleção de Dados!!'.

  it_0055 = it_0055_aux.

  SELECT nro_sol_ov, bezei, cod_fp, posnr, formula2, field
    FROM zsdt0059
    INTO TABLE @it_0059
   WHERE nro_sol_ov IN @r_nro_sol_ov
     AND field = 'QTDFIXADA'
     AND bezei LIKE 'T%'
  AND tipo_calc = 'V'.

  SELECT nro_sol_ov, bezei, cod_fp, posnr, formula2, field
    FROM zsdt0059
    APPENDING TABLE @it_0059
    WHERE nro_sol_ov IN @r_nro_sol_ov
     AND field = 'PRECO'
     " 19.01.2024 - 131275 - RBL -->
     " AND cod_fp IN ('0016', '0009', '0008'). "// 0016 - FRETE CIF, 0009 - FRETE PORTO,  0008 - FOBS
  AND cod_fp IN ( '0016', '0009', '0008', '0241' ).
  " 19.01.2024 - 131275 - RBL --<

  PERFORM progress USING 30 'Seleção de Dados!!'.

  SELECT nro_sol_ov, fixacao, total_proporc, taxa_cambio, tipo
    FROM zsdt0094
    INTO TABLE @it_0094
       WHERE nro_sol_ov IN @r_nro_sol_ov
         AND tipo IN ( 'VDA', 'FRE' )
  AND estorno = '0000000000'.

  " 26.03.2025 - 170634 - RAMON -->
  PERFORM f_seleciona_dados_email.
  " 26.03.2025 - 170634 - RAMON <--


  PERFORM progress USING 50 'Seleção de Dados!!'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TRATAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tratar_dados_mi.

  DATA lv_valor TYPE zdmbtr.

  FREE it_saida.

  IF job_ok IS NOT INITIAL.
    ADD 1 TO seq.
    APPEND VALUE #( seq = seq message = |Tratando Dados para Envio!| ) TO it_log.
  ENDIF.

  PERFORM progress USING 60 'Tratando Dados!'.

  LOOP AT it_0053 INTO DATA(_0053).

    DATA(_0051) = it_0051[ nro_sol_ov = _0053-nro_sol_ov ].

    _saida-area = 'MI'.
    _saida-nro_sol_ov = _0053-nro_sol_ov.
    _saida-fixacao    = |{ _0053-fixacao ALPHA = IN }|.
    _saida-tp_venda   = _0051-tp_venda.
    _saida-waerk      = _0051-waerk.
    _saida-status     = _0051-status.
    _saida-tipo       = 'VDA'.

    _saida-vl94       = REDUCE #(
                                  INIT x TYPE zdmbtr
                                    FOR ls IN it_0094
                                  WHERE ( nro_sol_ov EQ  _0053-nro_sol_ov
                                      AND fixacao EQ _0053-fixacao
                                      AND tipo EQ _saida-tipo )
                                   NEXT x = x + ls-total_proporc
                                ).

    IF line_exists( it_0059[ nro_sol_ov = _0053-nro_sol_ov field = 'QTDFIXADA' ] ).


      CLEAR _saida-vl59.

      _saida-vl59       = REDUCE #(
                                   INIT x TYPE zdmbtr
                                     FOR ls1 IN it_0059
                                   WHERE ( nro_sol_ov EQ  _0053-nro_sol_ov
                                     AND posnr EQ _0053-fixacao
                                     AND field EQ 'QTDFIXADA'
                                      )
                                   NEXT x = x + ls1-formula2 ).

      _saida-vl59 = ( _saida-vl59 * _0053-dmbtr ) / 1000.

    ELSE.
      IF _0051-status EQ 'L'.
        _saida-vl59 = _0053-vlrtot.
      ELSE.
        IF _saida-vl94 IS INITIAL.
          _saida-vl59 = 0.
        ELSE.
          _saida-vl59 = _0053-vlrtot.
        ENDIF.
      ENDIF.
    ENDIF.

    IF obj_tx_curva->check_auart(
                                 tipo   = _saida-tipo
                                 numero = _0053-nro_sol_ov
                               ) IS INITIAL AND _saida-vl94 IS INITIAL.
      _saida-vl59 = 0.
    ENDIF.

    _saida-diferenca = _saida-vl59 - _saida-vl94.

    IF ( _saida-diferenca NE 0 AND _saida-waerk NE 'USD' ) OR
       ( _saida-waerk EQ 'USD' AND _saida-vl94 IS NOT INITIAL ).
      APPEND _saida TO it_saida.
    ENDIF.

    CLEAR _saida.

    CHECK NOT line_exists( it_0055[ nro_sol_ov = _0053-nro_sol_ov ] ).

    _saida-area = 'MI'.
    _saida-nro_sol_ov = _0053-nro_sol_ov.
    _saida-fixacao    = |{ _0053-fixacao ALPHA = IN }|.
    _saida-tp_venda   = _0051-tp_venda.
    _saida-waerk      = _0051-waerk.
    _saida-status     = _0051-status.
    _saida-tipo       = 'FRE'.

    _saida-vl94       = REDUCE #(
                                  INIT x TYPE zdmbtr
                                    FOR ls IN it_0094
                                  WHERE ( nro_sol_ov EQ _0053-nro_sol_ov
                                      AND fixacao EQ _0053-fixacao
                                      AND tipo EQ _saida-tipo )
                                   NEXT x = x + ls-total_proporc
                                ).

    IF line_exists( it_0059[ nro_sol_ov = _0053-nro_sol_ov ] ).
      PERFORM calcula_cadencia USING _0053-nro_sol_ov _0053-fixacao CHANGING _saida-vl59.
    ENDIF.

    IF _0051-status NE 'L' AND _saida-vl94 IS INITIAL.
      _saida-vl59 = 0.
    ENDIF.

    IF _0051-param_espec EQ 'M' AND _0053-status_itm IS INITIAL.
      _saida-vl59 = 0.
    ENDIF.

    IF obj_tx_curva->check_auart(
                                  tipo   = _saida-tipo
                                  numero = _0053-nro_sol_ov
                                ) IS INITIAL AND _saida-vl94 IS INITIAL.
      _saida-vl59 = 0.
    ENDIF.

    _saida-diferenca = _saida-vl59 - _saida-vl94.

    IF _saida-diferenca NE 0.
      APPEND _saida TO it_saida.
    ENDIF.

  ENDLOOP.

  PERFORM progress USING 80 'Tratando Dados!'.

  LOOP AT it_0055 INTO DATA(_0055).

    _0051 = it_0051[ nro_sol_ov = _0055-nro_sol_ov ].


    " 08.07.2024 - RAMON -->
    TRY .
        _0053 = it_0053[ nro_sol_ov = _0055-nro_sol_ov fixacao = _0055-fixacao ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
    " 08.07.2024 - RAMON --<

    IF _0051-param_espec EQ 'M' AND _0053-status_itm NE 'F'.
      CONTINUE.
    ENDIF.

    IF _0051-inco1 EQ 'FOB' AND _0051-risco_sacado EQ 'S'.
      CONTINUE.
    ENDIF.

    IF _0051-param_espec NE 'M' .
      CONTINUE.
    ENDIF.

    _saida-area = 'MI'.
    _saida-nro_sol_ov = _0055-nro_sol_ov.
    _saida-fixacao    = |{ _0055-fixacao ALPHA = IN }|.
    _saida-tp_venda   = _0051-tp_venda.
    _saida-status     = _0051-status.
    _saida-tipo       = 'FRE'.

    _saida-vl94       = REDUCE #(
                                  INIT x TYPE zdmbtr
                                    FOR ls IN it_0094
                                  WHERE ( nro_sol_ov EQ  _0055-nro_sol_ov
                                      AND fixacao EQ _0055-fixacao
                                      AND tipo EQ _saida-tipo )
                                   NEXT x = x + ls-total_proporc
                                ).

    IF line_exists( it_0059[ nro_sol_ov = _0055-nro_sol_ov ] ).
      PERFORM calcula_cadencia USING _0055-nro_sol_ov _0055-fixacao CHANGING _saida-vl59.
    ENDIF.

    IF _0051-status NE 'L' AND _saida-vl94 IS INITIAL.
      _saida-vl59 = 0.
    ENDIF.

    IF obj_tx_curva->check_auart(
                                  tipo   = _saida-tipo
                                  numero = _0053-nro_sol_ov
                                ) IS INITIAL AND _saida-vl94 IS INITIAL.
      _saida-vl59 = 0.
    ENDIF.

    _saida-diferenca = _saida-vl59 - _saida-vl94.

    IF _saida-diferenca NE 0.
      APPEND _saida TO it_saida.
    ENDIF.

  ENDLOOP.

  SORT it_saida BY nro_sol_ov fixacao.

  " 04.04.2025 - 170634 - RAMON -->
  PERFORM f_info_email.
  " 04.04.2025 - 170634 - RAMON --<

  IF it_saida IS INITIAL.
    IF job_ok IS INITIAL.
      MESSAGE s000 WITH |'Nenhuma Solicitação foi Encontrada!|.
    ELSE.
      ADD 1 TO seq.
      APPEND VALUE #( seq = seq message = |Nenhuma Solicitação foi Encontrada!| ) TO it_log.
    ENDIF.
  ENDIF.

  PERFORM progress USING 90 'Tratando Dados!'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS_MI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprimir_dados_mi.

  PERFORM f_teste_dev.

  PERFORM progress USING 100 'Exibindo na ALV!'.

  FREE it_fcat.

  PERFORM fcat_mi USING:
      'IT_SAIDA' 'NRO_SOL_OV' '10' 'Solicitação'  ' ' '' '',
      'IT_SAIDA' 'FIXACAO'    '08' 'Fixação'      'X' '' '',
      'IT_SAIDA' 'TIPO'       '04' 'Tipo'         ' ' '' '',
      'IT_SAIDA' 'SPART'      '02' 'Atividade'    ' ' '' '', "#debug
      'IT_SAIDA' 'TPSIM'      '03' 'Cond. PG'     ' ' '' '',
      'IT_SAIDA' 'STATUS'     '10' 'Status'       ' ' '' '',
      'IT_SAIDA' 'WAERK'      '03' 'Moeda'        ' ' '' '',
      'IT_SAIDA' 'TP_VENDA'   '08' 'Tp. Venda'    ' ' '' '',
      'IT_SAIDA' 'VL94'       '15' 'Vl Hedge'     ' ' '' '',
      'IT_SAIDA' 'VL59'       '15' 'Vl Simulador' ' ' '' '',
      'IT_SAIDA' 'DIFERENCA'  '15' 'Diferença'    ' ' '' '',
      'IT_SAIDA' 'SEM_EMAIL'  '15' 'Não envia Email' '' 'X' 'X'.  " 26.03.2025 - 170634 - RAMON

  CALL SCREEN 0100.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FCAT_MI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0445   text
*      -->P_0446   text
*      -->P_0447   text
*      -->P_0448   text
*----------------------------------------------------------------------*
FORM fcat_mi  USING    VALUE(p_tabela)
                       VALUE(p_fielname)
                       VALUE(p_tamanho)
                       VALUE(p_nome)
                       VALUE(p_zero)
   " 26.03.2025 - 170634 - RAMON -->
                       VALUE(p_edit)
                       VALUE(p_check).
  " 26.03.2025 - 170634 - RAMON --<

  APPEND
  VALUE #(
            tabname = p_tabela
            fieldname = p_fielname
            outputlen = p_tamanho
            scrtext_l = p_nome
            lzero = p_zero
            edit = p_edit " 26.03.2025 - 170634 - RAMON
            checkbox = p_check " 26.03.2025 - 170634 - RAMON
  ) TO t_fcat.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ENVIAR_EMAIL_MI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enviar_email_mi.

  DATA: _type          TYPE sood-objtp,
        _email         TYPE adr6-smtp_addr,
        lo_document    TYPE REF TO cl_document_bcs,
        lo_bcs         TYPE REF TO cl_bcs,
        lo_sapuser_bcs TYPE REF TO cl_sapuser_bcs,
        lo_recipient   TYPE REF TO if_recipient_bcs,
        lo_ex_bcs      TYPE REF TO cx_bcs,
        lv_message     TYPE string,
        vuser          TYPE sy-uname.

  MESSAGE s000 WITH |Enviando e-mail!|.
  ADD 1 TO seq.
  APPEND VALUE #( seq = seq message = |Enviando e-mail!| ) TO it_log.

  " 28.03.2025 - RAMON - 170634 -->

***  SELECT *
***    FROM zmail
***      INTO TABLE @DATA(it_email)
***  WHERE tcode EQ 'HEDGE'.

  SELECT email FROM zsdt0374
    INTO TABLE @DATA(it_email).

  " 28.03.2025 - RAMON - 170634 --<

  ""XXXXXXXXXXXXXXXX ZSDT0374

  IF it_email IS INITIAL.
    IF job_ok IS INITIAL.
      MESSAGE s000 WITH |Nenhum e-mail cadastrado para "HEDGE" na ZMAIL!|.
    ELSE.
      ADD 1 TO seq.
      APPEND VALUE #( seq = seq message = |Nenhum e-mail cadastrado para "HEDGE" na ZMAIL!| ) TO it_log.
    ENDIF.
    EXIT.
  ENDIF.

  LOOP AT it_email INTO DATA(wa_zmail).

    MOVE: 'HTM'          TO _type,
           wa_zmail-email TO _email.

    CLEAR: lo_document, vuser.

    IF sy-sysid NE 'PRD'.
      lv_sub = |{ sy-sysid }-{ sy-mandt } { lv_sub }|.
    ENDIF.

    lo_document = cl_document_bcs=>create_document(
      i_type    = _type
      i_subject = lv_sub
      i_text    = t_html ).

    lo_bcs = cl_bcs=>create_persistent( ).
    lo_bcs->set_document( lo_document ).

    lo_recipient = cl_cam_address_bcs=>create_internet_address( _email ).

    lo_bcs->set_message_subject( ip_subject = CONV #( lv_sub ) ).

    TRY.
        CALL METHOD lo_bcs->add_recipient
          EXPORTING
            i_recipient = lo_recipient
            i_express   = abap_true.
      CATCH cx_send_req_bcs.
    ENDTRY.

    vuser = sy-uname.
    "sy-uname = 'JOBADM'.

    lo_sapuser_bcs = cl_sapuser_bcs=>create( sy-uname ).
    lo_bcs->set_sender( i_sender = lo_sapuser_bcs ).

    IF 1 = 2.
      lo_bcs->set_send_immediately( 'X' ). " 02.04.2025 - RAMON - 170634
    ENDIF.


    TRY.
        CALL METHOD lo_bcs->send( ).
        COMMIT WORK.
        MESSAGE s000 WITH |E-mail enviado com sucesso!|.
        ADD 1 TO seq.
        APPEND VALUE #( seq = seq message = |E-mail enviado com sucesso para { _email }!| ) TO it_log.
      CATCH cx_bcs INTO lo_ex_bcs.
        sy-uname = vuser.
        lv_message = lo_ex_bcs->get_text( ).
        MESSAGE s000 WITH lv_message.
    ENDTRY.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_HTM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_htm USING tipo.

  CHECK it_saida IS NOT INITIAL.

  MESSAGE s000 WITH |Montando Html para envio!|.
  ADD 1 TO seq.
  APPEND VALUE #( seq = seq message = |Montando Html para envio!| ) TO it_log.

  DATA: len1         TYPE i,
        len2         TYPE i,
        ofst1        TYPE i,
        ofst2        TYPE i,
        ofst3        TYPE i,
        vl_html      TYPE string,
        str_aux      TYPE string VALUE 'a b',
        str_c        TYPE string,
        z_html       TYPE zstring,
        html         TYPE string,
        vl_01        TYPE string VALUE 'Solicitação',
        vl_02        TYPE string VALUE 'Fixação',
        vl_03        TYPE string VALUE 'Tipo de Venda',
        vl_04        TYPE string VALUE 'Tipo',
        vl_05        TYPE string VALUE 'Valor Hedge',
        vl_06        TYPE string VALUE 'Valor MI',
        vl_07        TYPE string VALUE 'Diferença',
        lv_zebra     TYPE c,
        lv_zebra_s   TYPE c LENGTH 40,
        lv_com_dados TYPE c.

  lv_sub = |E-mail de diferença Hedge "{ tipo }"! - { sy-datum }-{ sy-uzeit }|.

  CASE tipo.
    WHEN 'IN'.
      vl_01 = 'Simulador'.
      vl_02 = 'Item'.
      vl_03 = 'Tipo de Venda'.
      vl_06 = 'Valor Insumos'.

    WHEN 'PD'.
      vl_01 = 'Pedido'.
      vl_02 = 'Item'.
      vl_03 = 'Tipo de Venda'.
      vl_06 = 'Valor Pedido'.
    WHEN 'AQ'.
      vl_01 = 'Remessa'.
      vl_02 = 'Item'.
      vl_03 = 'Tipo de Venda'.
      vl_06 = 'Valor Frete'.
  ENDCASE.


  html =
  |<!DOCTYPE html>| &&
  |<html lang="pt-br">| &&
  |  <head>| &&
  |    <!-- Meta tags Obrigatórias -->| &&
  |    <meta charset="utf-8">| &&
  |    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">| &&

  |    <!-- Bootstrap CSS -->| &&
  |    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css" integrity="sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO" crossorigin="anonymous">| &&

  |    <title>{ lv_sub }</title>| &&
  |  </head>| &&
  |  <body>| &&
  |    <table class="table">| &&
  |        <thead>| &&
  |          <tr>| &&
  |            <th scope="col"></th>| &&
  |            <th scope="col"> { vl_01 } </th>| &&
  |            <th scope="col"> { vl_02 } </th>| &&
  |            <th scope="col"> { vl_03 } </th>| &&
  |            <th scope="col"> { vl_04 } </th>| &&
  |            <th scope="col"> { vl_05 } </th>| &&
  |            <th scope="col"> { vl_06 } </th>| &&
  |            <th scope="col"> { vl_07 } </th>| &&
  |          </tr>| &&
  |        </thead>| &&
  |        <tbody>|.

  LOOP AT it_saida INTO DATA(_saida) WHERE sem_email = abap_false. " 28.03.2025 - RAMON - 170634

    lv_com_dados = abap_true.

    IF lv_zebra = abap_false.

      lv_zebra_s = space.
      lv_zebra = abap_true.

    ELSE.

      lv_zebra_s = 'style="background-color: #EEEEEE;"'.
      lv_zebra = abap_false.

    ENDIF.

    " 04.04.2025 - RAMON -->
    DATA lv_vl94 TYPE c LENGTH 30.
    DATA lv_vl59 TYPE c LENGTH 30.
    DATA lv_diff TYPE c LENGTH 30.

    WRITE _saida-vl94 TO lv_vl94 LEFT-JUSTIFIED.
    WRITE _saida-vl59 TO lv_vl59 LEFT-JUSTIFIED.
    WRITE _saida-diferenca TO lv_diff LEFT-JUSTIFIED.

    " 04.04.2025 - RAMON --<

    html = |{ html }| &&
      |            <tr { lv_zebra_s } > | &&
      |            <th scope="row">{ sy-tabix }</th>| &&
      |            <td>{ _saida-nro_sol_ov }</td>| &&
      |            <td>{ _saida-fixacao }</td>| &&
      |            <td>{ _saida-tp_venda }</td>| &&
      |            <td>{ _saida-tipo }</td>| &&
      |            <td style="text-align: right;">{ lv_vl94 }</td>| &&
      |            <td style="text-align: right;">{ lv_vl59 }</td>| &&
      |            <td style="text-align: right;">{ lv_diff }</td>| &&
      |          </tr>|.

  ENDLOOP.

  html = |{ html }| &&
  |        </tbody>| &&
  |      </table>| &&

  |    <!-- JavaScript (Opcional) -->| &&
  |    <!-- jQuery primeiro, depois Popper.js, depois Bootstrap JS -->| &&
  |    <script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>| &&
  |    <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js" integrity="sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49" crossorigin="anonymous"></script>| &&
  |    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js" integrity="sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy" crossorigin="anonymous"></script>| &&
  |  </body>| &&
  |</html>|.

  t_html = zcl_solicitacao_ov=>converte_envio_html( html ).

  CHECK lv_com_dados = abap_true.

  PERFORM enviar_email_mi.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  " 26.03.2025 - 170634 - RAMON -->
  DATA lo_handle TYPE REF TO lcl_event_handler.
  DATA lo_ontool TYPE REF TO lcl_alv_toolbar.
  " 26.03.2025 - 170634 - RAMON <--

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TI0100'.

  _layout =
            VALUE #(
                      zebra      = abap_true
                      sel_mode = 'A'
                      cwidth_opt = abap_true
                   ).

  IF container1 IS INITIAL.

    CREATE OBJECT container1
      EXPORTING
        container_name = 'CC'.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container1.

    " 26.03.2025 - 170634 - RAMON -->
    CREATE OBJECT lo_handle.
    CREATE OBJECT lo_ontool.
    SET HANDLER lo_handle->handle_data_changed FOR grid1.
    SET HANDLER lo_ontool->on_toolbar FOR grid1.
    SET HANDLER lo_ontool->handle_user_command_itens FOR grid1.
    " 26.03.2025 - 170634 - RAMON <--


    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        is_layout       = _layout
      CHANGING
        it_fieldcatalog = t_fcat
        it_outtab       = it_saida.

  ELSE.
  ENDIF.

*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      IT_FIELDCAT = IT_FCAT
*    TABLES
*      T_OUTTAB    = IT_SAIDA.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
      " 26.03.2025 - 170634 - RAMON -->
    WHEN 'SAVE'.
      PERFORM f_gravar_dados.
      " 26.03.2025 - 170634 - RAMON --<
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'DIFE'.
      PERFORM executar_diferenca.
    WHEN 'REFRESH'.
      PERFORM busca_dados.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  EXECUTAR_DIFERENCA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM executar_diferenca .

  DATA(obj)  = NEW zcl_taxa_curva_db( ).
  DATA param_espec TYPE sy-subrc.

  CALL METHOD grid1->get_selected_rows
    IMPORTING
      et_index_rows = _index.

  LOOP AT _index INTO DATA(w_in).
    DATA(_saida) = it_saida[ w_in-index ].

    SELECT COUNT(*)
      FROM zsdt0051
      WHERE nro_sol_ov = _saida-nro_sol_ov
    AND param_espec = 'M'.

    param_espec = sy-subrc.

*   "// Mercado Interno...
    IF _saida-area EQ 'MI'.

      IF _saida-waerk EQ 'USD'.
        IF _saida-vl94 IS NOT INITIAL "// Existe disparo de Hedge
       AND _saida-tipo EQ 'VDA'.      "// Venda
          CALL METHOD obj->edicao     "// o Metodo Edição estornar qualquer lançamento do Hedge
            EXPORTING
              i_numero  = _saida-nro_sol_ov  " Numero de Solicitação de Ordem de Venda
              i_fixacao = _saida-fixacao     " Nº item do documento de vendas e distribuição
              i_tcode   = 'ZSDT0062'.        " Código de transação atual
        ENDIF.
        CONTINUE.
      ENDIF.

      IF _saida-status NE 'L' AND
         _saida-vl94 IS NOT INITIAL.
        CALL METHOD obj->edicao
          EXPORTING
            i_numero  = _saida-nro_sol_ov  " Numero de Solicitação de Ordem de Venda
            i_fixacao = _saida-fixacao     " Nº item do documento de vendas e distribuição
            i_tcode   = 'ZSDT0062'.        " Código de transação atual
        CONTINUE.
      ENDIF.

      IF _saida-status EQ 'L'
     AND _saida-vl94 IS INITIAL.
        IF _saida-tipo EQ 'VDA'.

          IF param_espec IS INITIAL.

            CALL METHOD obj->frame
              EXPORTING
                i_numero = _saida-nro_sol_ov " Numero de Solicitação de Ordem de Venda
                i_tcode  = 'ZSDT0062'.       " Código de transação atual
            CONTINUE.
          ELSE.

            CALL METHOD obj->liberar_ov
              EXPORTING
                i_numero  = _saida-nro_sol_ov   " Numero de Solicitação de Ordem de Venda
                i_tcode   = 'ZSDT0062'          " Código de transação atual
                i_fixacao = _saida-fixacao.     " Nº item do documento de vendas e distribuição
            CONTINUE.
          ENDIF.
        ELSE.

          IF param_espec IS INITIAL. " 23.01.2024 - RAMON - só executa o frete, se tiver o de venda disparado
            CALL METHOD obj->frete
              EXPORTING
                i_numero  = _saida-nro_sol_ov    " Numero de Solicitação de Ordem de Venda
                i_fixacao = _saida-fixacao    " Nº item do documento de vendas e distribuição
                i_status  = 'F'
                i_tcode   = 'ZSDT0062'.    " Código de transação atual
            CONTINUE.
          ENDIF.

        ENDIF.
      ENDIF.

      CALL METHOD obj->diferenca "// Lança a Diferença Frete do Hedge
        EXPORTING
          i_numero    = _saida-nro_sol_ov
          i_fixacao   = _saida-fixacao
          i_diferenca = _saida-diferenca
          i_tcode     = 'ZSDT0062'
          i_tipo      = _saida-tipo.

    ENDIF.

*   "// Insumos...
    IF _saida-area EQ 'IN'.

      sy-cprog = 'ZSDR016'.

      CASE _saida-tipo.
        WHEN 'VDI'.

          IF _saida-waerk EQ 'BRL'.

            IF _saida-status EQ 'A'.
              IF _saida-vl94 IS INITIAL.

                CASE _saida-tpsim.
                  WHEN 'BN' OR 'PM'.
                    CONTINUE.
                  WHEN OTHERS.
*                 "// Realiza o Disparo Inicial
                    CALL METHOD zcl_webservice_tx_curva=>hedge_insumos
                      EXPORTING
                        i_numero = _saida-nro_sol_ov  " Numero do documento de simulação de venda
                        i_tipo   = _saida-tipo.       " Campo de 3 bytes de comprimento
                    CONTINUE.
                ENDCASE.
              ELSE.
*             "// Estorna Bonificação e Permuta caso estiverem aprovados.
                IF _saida-tpsim EQ 'BN' OR _saida-tpsim EQ 'PM'.
                  CALL METHOD zcl_webservice_tx_curva=>hedge_insumos
                    EXPORTING
                      i_numero = _saida-nro_sol_ov   " Numero do documento de simulação de venda
                      i_tipo   = 'EST'.              " Campo de 3 bytes de comprimento
                  CONTINUE.
                ENDIF.
              ENDIF.
            ELSE.

              IF _saida-vl94 IS NOT INITIAL.
*             "// estorna o hedge se não estiver aprovado
                CALL METHOD zcl_webservice_tx_curva=>hedge_insumos
                  EXPORTING
                    i_numero = _saida-nro_sol_ov   " Numero do documento de simulação de venda
                    i_tipo   = 'EST'.              " Campo de 3 bytes de comprimento
                CONTINUE.
              ENDIF.

            ENDIF.

          ELSEIF _saida-waerk EQ 'USD'. "IF _saida-waerk EQ 'USD'. " ADICIONADO 10.06.2024
*       "// USD

            IF _saida-vl94 IS NOT INITIAL.
*           "// estorna a Venda em caso da Moeda ser USD
              CALL METHOD zcl_webservice_tx_curva=>hedge_insumos
                EXPORTING
                  i_numero = _saida-nro_sol_ov   " Numero do documento de simulação de venda
                  i_tipo   = 'EST'.              " Campo de 3 bytes de comprimento
              CONTINUE.
            ENDIF.

            " 10.06.2024 - RAMON INCLUSAO DA CORREÇÃO DO DISPARO PARA USC ( CENARIO: 370830 )-->
          ELSE.
            "BREAK RBLIMA.
            CALL METHOD obj->diferenca "// Lança a Diferença Frete do Hedge
              EXPORTING
                i_numero    = _saida-nro_sol_ov
                i_fixacao   = _saida-fixacao
                i_diferenca = _saida-diferenca
                i_tcode     = 'ZSDT0062'
                i_tipo      = _saida-tipo.

            " 10.06.2024 - RAMON INCLUSAO DA CORREÇÃO DO DISPARO PARA USC --<

          ENDIF.

        WHEN 'FRI'.

          IF _saida-status EQ 'A'. "//Aprovado
            IF _saida-vl94 IS INITIAL. "// Hedge em Branco

              CASE _saida-tpsim.
                WHEN 'PM'.
                  CONTINUE.
                WHEN OTHERS.
*                 "// Realiza o Disparo Inicial
                  CALL METHOD zcl_webservice_tx_curva=>hedge_insumos
                    EXPORTING
                      i_numero = _saida-nro_sol_ov
                      i_tipo   = 'FRI'.
                  CONTINUE.
              ENDCASE.
            ELSE.
*             "// Estorna Bonificação e Permuta caso estiverem aprovados.
              IF _saida-tpsim EQ 'PM'.
                CALL METHOD zcl_webservice_tx_curva=>hedge_insumos
                  EXPORTING
                    i_numero = _saida-nro_sol_ov   " Numero do documento de simulação de venda
                    i_tipo   = 'EST'.              " Campo de 3 bytes de comprimento
                CONTINUE.
              ENDIF.
            ENDIF.
          ELSE.

            IF _saida-vl94 IS NOT INITIAL.
*             "// estorna o hedge se não estiver aprovado
              CALL METHOD zcl_webservice_tx_curva=>hedge_insumos
                EXPORTING
                  i_numero = _saida-nro_sol_ov   " Numero do documento de simulação de venda
                  i_tipo   = 'EST'.              " Campo de 3 bytes de comprimento
              CONTINUE.
            ENDIF.

          ENDIF.

      ENDCASE.

      CALL METHOD obj->diferenca_in "// Lança a Diferença Frete do Hedge
        EXPORTING
          i_numero    = _saida-nro_sol_ov
          i_diferenca = _saida-diferenca
          i_waerk     = _saida-waerk
          i_bezei     = _saida-bezei
          i_tipo      = _saida-tipo.

    ENDIF.

    IF _saida-area EQ 'LES'.
      DATA: _vbrk TYPE vbrk,
            _vbrp TYPE vbrpvb.

      READ TABLE it_vbrk INTO _vbrk WITH KEY vbeln = _saida-nro_sol_ov.
      READ TABLE it_vbrp INTO _vbrp WITH KEY vbeln = _saida-nro_sol_ov.

      CALL METHOD obj->frete_aqv
        EXPORTING
          _vbrk = _vbrk    " Documento de faturamento: dados de cabeçalho
          _vbrp = _vbrp.   " Documento de faturamento: dados de item
    ENDIF.

    IF _saida-area EQ 'MM'.
      DATA: _ekko TYPE ekko,
            _ekpo TYPE ekpo.

      READ TABLE it_ekko INTO _ekko WITH KEY ebeln = _saida-nro_sol_ov.
      READ TABLE it_ekpo INTO _ekpo WITH KEY ebeln = _saida-nro_sol_ov.

      CALL METHOD obj->frete_pedido
        EXPORTING
          _ekko      = _ekko               " Documento ped.compras   : dados de cabeçalho
          _ekpo      = _ekpo               " Documento pes.compras   : dados de item
          _diferenca = _saida-diferenca.   " Diferenca
    ENDIF.

  ENDLOOP.

  PERFORM busca_dados.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALCULA_CADENCIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calcula_cadencia USING vl_nro_sol_ov vl_fixacao CHANGING mon1.

  DATA(obj) = NEW zcl_taxa_curva_db( ).
  DATA: mon              TYPE p LENGTH 13 DECIMALS 5, "Valor do Montante.
        at_total_54      TYPE dmbtr,
        at_total_53      TYPE dmbtr,
        at_total_peso_53 TYPE dzmeng.

  CLEAR mon1.

  CALL METHOD obj->get_totais
    EXPORTING
      i_nro_sol_ov    = vl_nro_sol_ov    " Numero de Solicitação de Ordem de Venda
    IMPORTING
      e_total_53      = at_total_53   " Montante em moeda interna
      e_total_54      = at_total_54   " Montante em moeda interna
*     e_total_55      =     " Quantidade diaria
*     e_indice        =     " Indice para calculo do total proporcional (HEDGE)
      e_total_peso_53 = at_total_peso_53.   " Qtd.prevista em UMV

  SELECT SINGLE *
    FROM zsdt0051
    INTO @DATA(w_0051)
  WHERE nro_sol_ov EQ @vl_nro_sol_ov.

  CHECK sy-subrc IS INITIAL.

  SELECT *
    FROM zsdt0052
    INTO TABLE @DATA(_0052)
  WHERE nro_sol_ov EQ @vl_nro_sol_ov.

  SELECT *
    FROM t052
    INTO TABLE @DATA(_t052)
    FOR ALL ENTRIES IN @_0052
  WHERE zterm EQ @_0052-zterm.

  SELECT *
    FROM zsdt0053
    INTO TABLE @DATA(_0053)
    WHERE nro_sol_ov EQ @vl_nro_sol_ov
      AND fixacao EQ @vl_fixacao
  AND status NOT IN ( 'C' ).

  SELECT *
    FROM zsdt0055
    INTO TABLE @DATA(_0055)
    WHERE nro_sol_ov EQ @vl_nro_sol_ov
      AND fixacao EQ @vl_fixacao
  AND status NOT IN ( 'C' ).

  SELECT *
    FROM zsdt0059
    INTO TABLE @DATA(_0059)
    WHERE nro_sol_ov EQ @vl_nro_sol_ov
      AND posnr EQ @vl_fixacao
  AND cod_fp IN ('0016', '0009', '0008', '0010').

  SELECT *
    FROM zsdt0054
    INTO TABLE @DATA(_0054)
  WHERE nro_sol_ov EQ @vl_nro_sol_ov.

  TRY .

      " 19.01.2024 - 131275 - RBL -->

      LOOP AT _0053 ASSIGNING FIELD-SYMBOL(<fs_0053>).

        READ TABLE it_0051 ASSIGNING FIELD-SYMBOL(<fs_0051>)
          WITH KEY nro_sol_ov = <fs_0053>-nro_sol_ov.

        IF sy-subrc EQ 0.

          IF <fs_0051>-matkl = '700400'.

            READ TABLE it_0059 ASSIGNING FIELD-SYMBOL(<fs_0059>)
              WITH KEY nro_sol_ov = <fs_0053>-nro_sol_ov
                            bezei = 'CONVERSOR BIO'.

            IF sy-subrc EQ 0.

              <fs_0053>-zmeng = <fs_0053>-zmeng * <fs_0059>-formula2.
              "at_total_peso_53 = at_total_peso_53 * <fs_0059>-formula2.

            ENDIF.

          ENDIF.

        ENDIF.

      ENDLOOP.

      IF <fs_0059> IS ASSIGNED.
        at_total_peso_53 = at_total_peso_53 * <fs_0059>-formula2.
      ENDIF.

      " 19.01.2024 - 131275 - RBL <--

      DATA(w_0053) = _0053[ 1 ].

    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  TRY .
      DATA(w_0059_porto) = _0059[ cod_fp = '0009' ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  TRY .
      DATA(w_0059_cif)   = _0059[ cod_fp = '0016' ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  TRY .
      DATA(w_0059_fobs)  = _0059[ cod_fp = '0008' ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  TRY .
      DATA(w_0059_tx_cambio)  = _0059[ cod_fp = '0010' ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  TRY .
      DATA(w_0052) = _0052[ 1 ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  TRY .
      DATA(w_t052) = _t052[ zterm = w_0052-zterm ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  IF w_0051-param_espec NE 'M'.
    CASE w_0051-risco_sacado.
      WHEN: 'S'.
        LOOP AT _0055 INTO DATA(w_0055).

          IF ( w_0055-zieme EQ  w_0053-pmein ).
            mon = ( ( w_0059_porto-formula2 - w_0059_cif-formula2 ) * w_0053-zmeng ).
          ELSE.
            mon = ( w_0059_porto-formula2 - w_0059_cif-formula2 - w_0059_fobs-formula2 ).
            CASE w_0055-zieme.
              WHEN 'KG'.
                DIVIDE   mon BY 1000.
              WHEN 'TO'.
                MULTIPLY mon BY 1000.
                " 19.01.2024 - 131275 - RBL -->
              WHEN 'L'.
                DIVIDE   mon BY 1000.
              WHEN 'M3'.
                MULTIPLY mon BY 1000.
                " 19.01.2024 - 131275 - RBL --<
            ENDCASE.
            MULTIPLY mon BY w_0055-cadencia_qte.
          ENDIF.

          ADD mon TO mon1.

        ENDLOOP.
      WHEN: 'N'.
        IF w_0053-valdt IS NOT INITIAL OR w_0052-valdt IS NOT INITIAL.
          IF  _0054 IS NOT INITIAL.
            IF line_exists( _0055[ nro_sol_ov = w_0053-nro_sol_ov ] ).
              LOOP AT _0055 INTO w_0055.
                IF ( w_0053-zieme EQ  w_0053-pmein ).
                  mon = ( ( w_0059_porto-formula2 - w_0059_cif-formula2 ) * w_0053-zmeng ).
                  CONTINUE.
                ENDIF.

                mon = ( w_0059_porto-formula2 - w_0059_cif-formula2 - w_0059_fobs-formula2 ).
                CASE w_0053-zieme.
                  WHEN 'KG'.
                    DIVIDE   mon BY 1000.
                  WHEN 'TO'.
                    MULTIPLY mon BY 1000.
                    " 19.01.2024 - 131275 - RBL -->
                  WHEN 'L'.
                    DIVIDE   mon BY 1000.
                  WHEN 'M3'.
                    MULTIPLY mon BY 1000.
                    " 19.01.2024 - 131275 - RBL --<
                ENDCASE.
                MULTIPLY mon BY w_0055-cadencia_qte.

                ADD mon TO mon1.

              ENDLOOP.
            ELSE.
              IF w_0053-zieme EQ  w_0053-pmein.
                mon = ( ( w_0059_porto-formula2 - w_0059_cif-formula2 ) * at_total_peso_53 ).
              ELSE.

                mon = ( w_0059_porto-formula2 - w_0059_cif-formula2 - w_0059_fobs-formula2 ).
                CASE w_0053-zieme.
                  WHEN 'KG'.
                    DIVIDE   mon BY 1000.
                  WHEN 'TO'.
                    MULTIPLY mon BY 1000.
                    " 19.01.2024 - 131275 - RBL -->
                  WHEN 'L'.
                    DIVIDE   mon BY 1000.
                  WHEN 'M3'.
                    MULTIPLY mon BY 1000.
                    " 19.01.2024 - 131275 - RBL --<
                ENDCASE.
                MULTIPLY mon BY at_total_peso_53.
              ENDIF.

              ADD mon TO mon1.

            ENDIF.
          ELSE.
            IF line_exists( _0055[ nro_sol_ov = w_0053-nro_sol_ov ] ).

              LOOP AT _0055 INTO w_0055.
                IF w_0053-zieme EQ  w_0053-pmein.
                  mon = ( ( w_0059_porto-formula2 - w_0059_cif-formula2 ) * w_0055-cadencia_qte ).
                ELSE.

                  mon = ( w_0059_porto-formula2 - w_0059_cif-formula2 - w_0059_fobs-formula2 ).
                  CASE w_0053-zieme.
                    WHEN 'KG'.
                      DIVIDE   mon BY 1000.
                    WHEN 'TO'.
                      MULTIPLY mon BY 1000.
                      " 19.01.2024 - 131275 - RBL -->
                    WHEN 'L'.
                      DIVIDE   mon BY 1000.
                    WHEN 'M3'.
                      MULTIPLY mon BY 1000.
                      " 19.01.2024 - 131275 - RBL --<
                  ENDCASE.
                  MULTIPLY mon BY w_0055-cadencia_qte.
                ENDIF.

                ADD mon TO mon1.
              ENDLOOP.

            ELSE.

              LOOP AT _0053 INTO w_0053.

                IF w_0053-zieme EQ  w_0053-pmein.
                  mon = ( ( w_0059_porto-formula2 - w_0059_cif-formula2 ) * w_0053-zmeng ).
                ELSE.
                  mon = ( w_0059_porto-formula2 - w_0059_cif-formula2 - w_0059_fobs-formula2 ).
                  CASE w_0053-zieme.
                    WHEN 'KG'.
                      DIVIDE   mon BY 1000.
                    WHEN 'TO'.
                      MULTIPLY mon BY 1000.
                      " 19.01.2024 - 131275 - RBL -->
                    WHEN 'L'.
                      DIVIDE   mon BY 1000.
                    WHEN 'M3'.
                      MULTIPLY mon BY 1000.
                      " 19.01.2024 - 131275 - RBL --<
                  ENDCASE.
                  MULTIPLY mon BY w_0053-zmeng.
                ENDIF.

                ADD mon TO mon1.

              ENDLOOP.

            ENDIF.
          ENDIF.
        ELSE.
          IF ( _0054[] IS INITIAL ) AND ( _0055[] IS INITIAL ).
            CASE w_t052-zdart.
              WHEN: 'D'.

                IF ( w_0053-zieme EQ  w_0053-pmein ).
                  mon = ( ( w_0059_porto-formula2 - w_0059_cif-formula2 ) * w_0053-zmeng ).
                ELSE.

                  mon = ( w_0059_porto-formula2 - w_0059_cif-formula2 - w_0059_fobs-formula2 ).
                  CASE w_0053-zieme.
                    WHEN 'KG'.
                      DIVIDE   mon BY 1000.
                    WHEN 'TO'.
                      MULTIPLY mon BY 1000.
                      " 19.01.2024 - 131275 - RBL -->
                    WHEN 'L'.
                      DIVIDE   mon BY 1000.
                    WHEN 'M3'.
                      MULTIPLY mon BY 1000.
                      " 19.01.2024 - 131275 - RBL --<
                  ENDCASE.
                  MULTIPLY mon BY w_0053-zmeng.
                ENDIF.

                ADD mon TO mon1.

              WHEN: 'B'.
                LOOP AT _0053 INTO w_0053.
                  IF ( w_0053-zieme EQ  w_0053-pmein ).
                    mon = ( ( w_0059_porto-formula2 - w_0059_cif-formula2 ) * w_0053-zmeng ).
                    CONTINUE.
                  ENDIF.

                  mon = ( w_0059_porto-formula2 - w_0059_cif-formula2 - w_0059_fobs-formula2 ).
                  CASE w_0053-zieme.
                    WHEN 'KG'.
                      DIVIDE   mon BY 1000.
                    WHEN 'TO'.
                      MULTIPLY mon BY 1000.
                      " 19.01.2024 - 131275 - RBL -->
                    WHEN 'L'.
                      DIVIDE   mon BY 1000.
                    WHEN 'M3'.
                      MULTIPLY mon BY 1000.
                      " 19.01.2024 - 131275 - RBL --<
                  ENDCASE.
                  MULTIPLY mon BY w_0053-zmeng.

                  ADD mon TO mon1.

                ENDLOOP.
            ENDCASE.
          ELSE.

            IF _0054 IS INITIAL AND _0055 IS NOT INITIAL.
              LOOP AT _0055 INTO w_0055.

                IF ( w_0055-zieme EQ  w_0053-pmein ).
                  mon = ( ( w_0059_porto-formula2 - w_0059_cif-formula2 ) * w_0053-zmeng ).
                  CONTINUE.
                ENDIF.

                mon = ( w_0059_porto-formula2 - w_0059_cif-formula2 - w_0059_fobs-formula2 ).
                CASE w_0055-zieme.
                  WHEN 'KG'.
                    DIVIDE   mon BY 1000.
                  WHEN 'TO'.
                    MULTIPLY mon BY 1000.
                    " 19.01.2024 - 131275 - RBL -->
                  WHEN 'L'.
                    DIVIDE   mon BY 1000.
                  WHEN 'M3'.
                    MULTIPLY mon BY 1000.
                    " 19.01.2024 - 131275 - RBL --<
                ENDCASE.
                MULTIPLY mon BY w_0055-cadencia_qte.

                ADD mon TO mon1.

              ENDLOOP.

            ELSEIF _0054 IS NOT INITIAL AND _0055 IS INITIAL.
              LOOP AT _0053 INTO w_0053.

                IF ( w_0053-zieme EQ  w_0053-pmein ).
                  mon = ( ( w_0059_porto-formula2 - w_0059_cif-formula2 ) * w_0053-zmeng ).
                  CONTINUE.
                ENDIF.

                mon = ( w_0059_porto-formula2 - w_0059_cif-formula2 - w_0059_fobs-formula2 ).
                CASE w_0053-zieme.
                  WHEN 'KG'.
                    DIVIDE   mon BY 1000.
                  WHEN 'TO'.
                    MULTIPLY mon BY 1000.
                    " 19.01.2024 - 131275 - RBL -->
                  WHEN 'L'.
                    DIVIDE   mon BY 1000.
                  WHEN 'M3'.
                    MULTIPLY mon BY 1000.
                    " 19.01.2024 - 131275 - RBL --<
                ENDCASE.
                MULTIPLY mon BY w_0053-zmeng.

                ADD mon TO mon1.

              ENDLOOP.

            ELSEIF _0054 IS NOT INITIAL AND _0055 IS NOT INITIAL.
              IF ( at_total_54 < at_total_53 ).
                LOOP AT _0053 INTO w_0053.
                  LOOP AT _0055 INTO w_0055.

                    IF ( w_0055-zieme EQ  w_0053-pmein ).
                      mon = ( ( w_0059_porto-formula2 - w_0059_cif-formula2 ) * w_0053-zmeng ).
                      CONTINUE.
                    ENDIF.

                    mon = ( w_0059_porto-formula2 - w_0059_cif-formula2 - w_0059_fobs-formula2 ).
                    CASE w_0055-zieme.
                      WHEN 'KG'.
                        DIVIDE   mon BY 1000.
                      WHEN 'TO'.
                        MULTIPLY mon BY 1000.
                        " 19.01.2024 - 131275 - RBL -->
                      WHEN 'L'.
                        DIVIDE   mon BY 1000.
                      WHEN 'M3'.
                        MULTIPLY mon BY 1000.
                        " 19.01.2024 - 131275 - RBL --<
                    ENDCASE.
                    MULTIPLY mon BY w_0055-cadencia_qte.

                    ADD mon TO mon1.

                  ENDLOOP.
                ENDLOOP.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.
  ELSE.

    IF line_exists( _0055[ nro_sol_ov = w_0053-nro_sol_ov ] ).

      LOOP AT _0055 INTO w_0055.
        IF w_0053-zieme EQ  w_0053-pmein.
          mon = ( ( w_0059_porto-formula2 - w_0059_cif-formula2 ) * w_0055-cadencia_qte ).
        ELSE.
          mon = ( w_0059_porto-formula2 - w_0059_cif-formula2 - w_0059_fobs-formula2 ).
          CASE w_0053-zieme.
            WHEN 'KG'.
              DIVIDE   mon BY 1000.
            WHEN 'TO'.
              MULTIPLY mon BY 1000.
              " 19.01.2024 - 131275 - RBL -->
            WHEN 'L'.
              DIVIDE   mon BY 1000.
            WHEN 'M3'.
              MULTIPLY mon BY 1000.
              " 19.01.2024 - 131275 - RBL --<
          ENDCASE.
          MULTIPLY mon BY w_0055-cadencia_qte.
        ENDIF.

        ADD mon TO mon1.
      ENDLOOP.

    ELSE.

      LOOP AT _0053 INTO w_0053.

        IF w_0053-zieme EQ  w_0053-pmein.
          mon = ( ( w_0059_porto-formula2 - w_0059_cif-formula2 ) * w_0053-zmeng ).
        ELSE.
          mon = ( w_0059_porto-formula2 - w_0059_cif-formula2 - w_0059_fobs-formula2 ).
          CASE w_0053-zieme.
            WHEN 'KG'.
              DIVIDE   mon BY 1000.
            WHEN 'TO'.
              MULTIPLY mon BY 1000.
              " 19.01.2024 - 131275 - RBL -->
            WHEN 'L'.
              DIVIDE   mon BY 1000.
            WHEN 'M3'.
              MULTIPLY mon BY 1000.
              " 19.01.2024 - 131275 - RBL --<
          ENDCASE.
          MULTIPLY mon BY w_0053-zmeng.
        ENDIF.

        ADD mon TO mon1.

      ENDLOOP.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_MSG  text
*----------------------------------------------------------------------*
FORM progress  USING:   porcentagem
                        msg.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = porcentagem
      text       = msg.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS_IN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selecionar_dados_in .

  CALL METHOD obj->get_porcentagem_frete
    IMPORTING
      porc_frete = DATA(porc_frete).     " Porcentagem do Frete

  SELECT
      'I' AS sign,
      'EQ' AS option,
       doc_simulacao AS low
     FROM zsdt0040
     INTO TABLE @r_nro_sol_ov
    WHERE "data_atual >= @at_datainicial
          data_atual IN @s_data
      AND vkorg       IN @s_bukrs
  AND doc_simulacao IN @s_solov.

  SORT r_nro_sol_ov BY low.
  DELETE ADJACENT DUPLICATES FROM r_nro_sol_ov COMPARING low.

  CHECK r_nro_sol_ov IS NOT INITIAL.

  SELECT *
    FROM zsdt0040
    INTO TABLE it_0040
  WHERE doc_simulacao IN r_nro_sol_ov.

  CHECK it_0040 IS NOT INITIAL.

  SELECT bukrs kursk
    FROM zsdt0117
    INTO CORRESPONDING FIELDS OF TABLE it_0117
    FOR ALL ENTRIES IN it_0040
   WHERE bukrs      EQ it_0040-vkorg
  AND desativado EQ abap_false.

  SELECT *
    FROM zsdt0041
    INTO TABLE it_0041
  WHERE doc_simulacao IN r_nro_sol_ov.

  SELECT *
    FROM zsdt0094
    INTO CORRESPONDING FIELDS OF TABLE it_0094
  WHERE nro_sol_ov IN r_nro_sol_ov.

  SELECT *
    FROM zsdt0090
    INTO TABLE it_0090
   WHERE doc_simulacao IN r_nro_sol_ov
  AND estorno EQ abap_false.

  CHECK it_0090 IS NOT INITIAL.

  SELECT *
    FROM zsdt0037
    INTO CORRESPONDING FIELDS OF TABLE it_0037
    FOR ALL ENTRIES IN it_0090
   WHERE matkl          EQ it_0090-matklv
     AND filial_origem  EQ it_0090-werksv
     AND meins          EQ it_0090-kmeinv
  AND waers          EQ 'BRL'.

  SELECT *
    FROM zsdt0037
    APPENDING CORRESPONDING FIELDS OF TABLE it_0037
    FOR ALL ENTRIES IN it_0090
   WHERE matkl         EQ it_0090-matkl
     AND filial_origem EQ it_0090-werks
     AND meins         EQ it_0090-kmein
     AND waers         EQ 'BRL'
     AND val_de        LE sy-datum
  AND val_ate       GE sy-datum.

  SELECT vbeln, vbeln AS vbeln_o, knumv
    FROM vbak
    INTO CORRESPONDING FIELDS OF TABLE @it_vbak
    FOR ALL ENTRIES IN @it_0090
   WHERE vbeln EQ @it_0090-vbelv
  OR vbeln EQ @it_0090-vbeln.

  CHECK it_vbak IS NOT INITIAL.

  " 24.05.2024 - RAMON - 141604 -->
  " CODIGO ADICIONADO PARA LINKAR DOC_SIMULACAO COM OV
  LOOP AT it_0090 ASSIGNING FIELD-SYMBOL(<fs_0090>).

    IF <fs_0090>-vbelv IS NOT INITIAL.
      DATA(lv_vbeln) = <fs_0090>-vbelv.
    ELSEIF <fs_0090>-vbeln IS NOT INITIAL.
      lv_vbeln = <fs_0090>-vbeln.
    ELSE.
      CLEAR lv_vbeln.
    ENDIF.

    CHECK lv_vbeln IS NOT INITIAL.

    READ TABLE it_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>)
      WITH KEY vbeln = lv_vbeln.

    CHECK sy-subrc EQ 0.

    <fs_vbak>-doc_sim = <fs_0090>-doc_simulacao.

    READ TABLE it_0090 ASSIGNING FIELD-SYMBOL(<fs_0090_o>)
      WITH KEY vbeln = lv_vbeln.

    CHECK sy-subrc EQ 0.

    CHECK <fs_0090_o>-vbelv IS NOT INITIAL.

    <fs_vbak>-vbeln_o = <fs_0090_o>-vbelv.

  ENDLOOP.
  " 24.05.2024 - RAMON - 141604 --<

  SELECT vbap~vbeln,vbap~posnr,matnr,spart,netwr,
         brgew,mwsbp,kwmeng,lifsp
    FROM vbap AS vbap
      " 24.05.2024 - RAMON - 141604 -->
    LEFT JOIN vbep ON vbep~vbeln = vbap~vbeln
                   AND vbep~posnr = vbap~posnr
                   AND vbep~etenr = '0001'
      " 24.05.2024 - RAMON - 141604 --<
    INTO TABLE @it_vbap
    FOR ALL ENTRIES IN @it_vbak
  WHERE vbap~vbeln EQ @it_vbak-vbeln.


*---> 19/07/2023 - Migração S4 - DG
*  SELECT *
*    FROM konv
*    INTO CORRESPONDING FIELDS OF TABLE it_konv
*    FOR ALL ENTRIES IN it_vbak
*   WHERE knumv EQ it_vbak-knumv
*     AND kschl EQ 'RB00'
*     AND kbetr NE 0.

  SELECT *
    FROM v_konv
    INTO CORRESPONDING FIELDS OF TABLE @it_konv
    FOR ALL ENTRIES IN @it_vbak
   WHERE knumv EQ @it_vbak-knumv
     AND kschl EQ 'RB00'
  AND kbetr NE 0.
*<--- 19/07/2023 - Migração S4 - DG


  CHECK it_vbap IS NOT INITIAL.

  SELECT *
    FROM mara
    INTO CORRESPONDING FIELDS OF TABLE it_mara
    FOR ALL ENTRIES IN it_vbap
  WHERE matnr EQ it_vbap-matnr.

  " 26.03.2025 - 170634 - RAMON -->
  PERFORM f_seleciona_dados_email.
  " 26.03.2025 - 170634 - RAMON <--

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TRATAR_DADOS_IN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tratar_dados_in.

*  IF r_new IS NOT INITIAL.

  DATA: var_total TYPE dmbtr.

  DATA(it_doc) = it_0041.
  SORT it_doc BY doc_simulacao.
  DELETE ADJACENT DUPLICATES FROM it_doc COMPARING doc_simulacao.

  "// Retorna a porcentagem de calculo para o frete
  PERFORM get_frete CHANGING porcentagem_fre.

  LOOP AT it_doc INTO DATA(_doc).

    DATA(_0040) = it_0040[ doc_simulacao = _doc-doc_simulacao ].

    IF _0040-taxa_frete IS NOT INITIAL.
      vl_kursk = _0040-taxa_frete.
    ELSE.

      " 12.12.2022 - RAMON LIMA - 96701 CS2021000667 ZSDT0171 -->
      "vl_kursk = it_0117[ bukrs = _0040-vkorg ]-kursk.

      vl_kursk = VALUE #( it_0117[ bukrs = _0040-vkorg ]-kursk DEFAULT '' ).

      " 12.12.2022 - RAMON LIMA - 96701 CS2021000667 ZSDT0171 <--

    ENDIF.

    IF 'BN_PM' NS _0040-tpsim AND _0040-tpsim IS NOT INITIAL.

      CALL METHOD obj_tx_curva->agrupa_dados
        EXPORTING
          i_numero = _doc-doc_simulacao
          i_tipo   = 'VDI'
        IMPORTING
          i_0041   = it_41.

      PERFORM pf_exe_add_90_to_41 USING _doc-doc_simulacao.

      " 25.04.2024 - RAMON -> Correção somatoria pelo SPART e ( não matkl/spart) -->
      "PERFORM f_agrupa_dados_exibicao.
      " 25.04.2024 - RAMON -> Correção somatoria pelo SPART e ( não matkl/spart) --<

      LOOP AT it_41 INTO DATA(_0041).

        CLEAR: _saida, var_ckeck, sum_a, calc_m ,sum_m, sum_c, sum_r, sum_e, sum_y.

        _saida-area = 'IN'.
        _saida-tipo = 'VDI'.

        _saida-nro_sol_ov = _0041-doc_simulacao.
        _saida-fixacao    = |{ _0041-posnr ALPHA = IN }|.
        _saida-tpsim      = _0040-tpsim.
        _saida-waerk      = _0040-waerk.
        _saida-status     = _0040-status.
        _saida-spart  = _0041-spart. "#debug

        _saida-bezei      = SWITCH #( _0041-spart
                                      WHEN '03' THEN 'DF'
                                      WHEN '02' THEN 'FT'
                                      WHEN '04' THEN |S{ _0040-cultura(1) }|
                                    ).

        _saida-vl94       = REDUCE #(
                                      INIT x TYPE zdmbtr
                                        FOR ls IN it_0094
                                      WHERE ( nro_sol_ov EQ  _0041-doc_simulacao
                                          AND tipo EQ _saida-tipo
                                          AND ( bezei EQ _saida-bezei
                                             OR bezei EQ |{ _saida-bezei }D|
                                             OR bezei EQ |{ _saida-bezei }W|
                                             OR bezei EQ |{ _saida-bezei }Y|
                                             OR bezei EQ 'DIFE_VDI_MI' )
                                             )
                                       NEXT x = x + ls-total_proporc
                                    ).

        PERFORM pf_exec_monta_90_vdi USING _doc-doc_simulacao 'A' _0041-spart CHANGING sum_a. "// Alteração de Quantidade
        PERFORM pf_exec_monta_90_vdi USING _doc-doc_simulacao 'M' _0041-spart CHANGING sum_m. "// Troca de Materiais
        PERFORM pf_exec_monta_90_vdi USING _doc-doc_simulacao 'C' _0041-spart CHANGING sum_c. "// Trava Cambio
        PERFORM pf_exec_monta_90_vdi USING _doc-doc_simulacao 'R' _0041-spart CHANGING sum_r. "// Redistribuição
        PERFORM pf_exec_monta_90_vdi USING _doc-doc_simulacao 'E' _0041-spart CHANGING sum_e. "// Encerramento da O.V.
        PERFORM pf_exec_monta_90_vdi USING _doc-doc_simulacao 'Y' _0041-spart CHANGING sum_y. "// Devolução.

        IF sum_c IS NOT INITIAL.
          ADD sum_c TO _saida-vl59.
        ELSE.
          ADD _0041-vlrtot TO _saida-vl59.

          " " 07.06.2024 - RAMON -  foi retirado pq estava pegando a diferença de preço da ov e calculando junto com o valor do simulador: 371577
          ADD sum_a TO _saida-vl59.

          " " 06.06.2024 - RAMON - Foi retirado pq estava diminuindo o valor de troca de material categoria = 'M' -->
          ADD sum_m TO _saida-vl59.

          " 06.06.2024 <--

          ADD sum_e TO _saida-vl59.
          ADD sum_r TO _saida-vl59.

          " " 07.06.2024 - RAMON -  foi retirado pq estava pegando a diferença de preço da ov e calculando junto com o valor do simulador: 371577
          ADD sum_y TO _saida-vl59.
        ENDIF.

        " 29.04.2024 - RAMON - Codigo feito se o valor da 159 estiver em dolar, pq o hedge(0094) sempre estará em reais -->
        " entao esse codigo foi feito para nao tirar diferença de USD com BRL por exemplo.
*        IF _saida-waerk = 'USD'.
*          IF ls_0094-taxa_cambio IS NOT INITIAL.
*            _saida-vl94 = _saida-vl94 / ls_0094-taxa_cambio.
*          ELSEIF vl_kursk IS NOT INITIAL.
*            _saida-vl94 = _saida-vl94 / vl_kursk.
*          ENDIF.
*        ENDIF.
        " 29.04.2024 - RAMON - Codigo feito se o valor da 159 estiver em dolar, pq o hedge(0094) sempre estará em reais --<

        _saida-diferenca = _saida-vl59 - _saida-vl94.

        PERFORM pf_exec_validar_41 USING _saida CHANGING var_ckeck.

        IF var_ckeck IS NOT INITIAL. "OR p_teste = abap_true.
          APPEND _saida TO it_saida.
        ENDIF.

        CLEAR: _saida, sum_a, calc_m ,sum_m, sum_c.

      ENDLOOP.

    ENDIF.

    CALL METHOD obj_tx_curva->agrupa_dados
      EXPORTING
        i_numero = _doc-doc_simulacao
        i_tipo   = 'FRI'
      IMPORTING
        i_0041   = it_41.

    PERFORM pf_exe_add_90_to_41 USING _doc-doc_simulacao.

    " 25.04.2024 - RAMON -> Correção somatoria pelo SPART e ( não matkl/spart) -->
    "PERFORM f_agrupa_dados_exibicao.
    " 25.04.2024 - RAMON -> Correção somatoria pelo SPART e ( não matkl/spart) --<

    LOOP AT it_41 INTO _0041.

      CLEAR: _saida, var_ckeck, sum_a, sum_m, sum_, sum_r, sum_e, sum_y, sum_f.

      _saida-area = 'IN'.
      _saida-tipo = 'FRI'.

      _saida-nro_sol_ov = _0041-doc_simulacao.
      _saida-fixacao    = |{ _0041-posnr ALPHA = IN }|.
      _saida-tpsim      = _0040-tpsim.
      _saida-waerk      = _0040-waerk.
      _saida-status     = _0040-status.
      _saida-spart  = _0041-spart. "#debug
      _saida-bezei      = SWITCH #( _0041-spart
                                    WHEN '03' THEN 'DF'
                                    WHEN '02' THEN 'FT'
                                    WHEN '04' THEN |S{ _0040-cultura(1) }|
                                  ).

      PERFORM pf_exec_monta_90_fri USING _doc-doc_simulacao 'A' _0041-spart CHANGING sum_a. "// Alteração de Quantidade
      PERFORM pf_exec_monta_90_fri USING _doc-doc_simulacao 'M' _0041-spart CHANGING sum_m. "// Troca de Materiais
      PERFORM pf_exec_monta_90_fri USING _doc-doc_simulacao ' ' _0041-spart CHANGING sum_.  "// Transferencia
      PERFORM pf_exec_monta_90_fri USING _doc-doc_simulacao 'R' _0041-spart CHANGING sum_r. "// Redistribuição
      PERFORM pf_exec_monta_90_fri USING _doc-doc_simulacao 'E' _0041-spart CHANGING sum_e. "// Encerramento da O.V.
      PERFORM pf_exec_monta_90_fri USING _doc-doc_simulacao 'Y' _0041-spart CHANGING sum_y. "// Devolução
      PERFORM pf_exec_monta_90_fri USING _doc-doc_simulacao 'F' _0041-spart CHANGING sum_f. "// Frete


      _saida-vl94       = REDUCE #(
                                    INIT x TYPE zdmbtr
                                      FOR ls IN it_0094
                                    WHERE ( nro_sol_ov EQ  _0041-doc_simulacao
                                        AND tipo EQ _saida-tipo
                                        AND ( bezei EQ _saida-bezei
                                            OR bezei EQ |{ _saida-bezei }D|
                                            OR bezei EQ |{ _saida-bezei }W|
                                            OR bezei EQ |{ _saida-bezei }Y| )
                                           )
                                     NEXT x = x + ls-total_proporc
                                  ).

      obj_tx_curva->set_matkl( i_matkl = _0041-matkl
                               i_brgew = _0041-brgew
                               ).

      IF _0041-spart EQ '03'.
        IF ( _0040-waerk EQ 'USD' ) AND ( vl_kursk IS NOT INITIAL ).
          var_total = ( _0041-vlrtot * porcentagem_fre ) * vl_kursk.
        ELSE.
          var_total = _0041-vlrtot * porcentagem_fre.
        ENDIF.
      ELSE.
        var_total = _0041-vlrtot.
      ENDIF.

      obj_tx_curva->set_bezei( CONV #( abap_false ) ).

      obj_tx_curva->set_cadencia_in( i_cadencia = _0041-zmeng
                                     i_negativa = 'S'
                                     i_0040     = _0040
                                     ).

      obj_tx_curva->set_total_proporcional( i_total    = var_total
                                            i_negativa = abap_true
                                            ).

      var_total = obj_tx_curva->get_total_proporcional( ).

      sum_f = abs( sum_f ).

      ADD var_total TO _saida-vl59.
      ADD sum_a TO _saida-vl59.
      ADD sum_m TO _saida-vl59.
      ADD sum_  TO _saida-vl59.
      ADD sum_r TO _saida-vl59.
      ADD sum_e TO _saida-vl59.
      ADD sum_y TO _saida-vl59.
      ADD sum_f TO _saida-vl59.

      _saida-vl94 = abs( _saida-vl94 ).
      _saida-vl59 = abs( _saida-vl59 ).

      _saida-diferenca = _saida-vl94 - _saida-vl59.

      PERFORM pf_exec_validar_41 USING _saida CHANGING var_ckeck.

      IF var_ckeck IS NOT INITIAL.
        APPEND _saida TO it_saida.
      ENDIF.

      CLEAR: _saida, sum_a, calc_m ,sum_m.

    ENDLOOP.

  ENDLOOP.

  " 04.04.2025 - 170634 - RAMON -->
  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

    READ TABLE gt_zsdt0373 ASSIGNING FIELD-SYMBOL(<fs_0373>)
      WITH KEY nro_sol_ov = <fs_saida>-nro_sol_ov
               fixacao = <fs_saida>-fixacao
               tp_venda = <fs_saida>-tipo.

    IF sy-subrc EQ 0.
      <fs_saida>-sem_email = <fs_0373>-sem_email.
    ENDIF.

  ENDLOOP.
  " 04.04.2025 - 170634 - RAMON --<

*  ELSE.

*    it_doc = it_0041.
*    SORT it_doc BY doc_simulacao.
*    DELETE ADJACENT DUPLICATES FROM it_doc COMPARING doc_simulacao.
*
** "// Retorna a porcentagem de calculo para o frete
*    PERFORM get_frete CHANGING porcentagem_fre.
*
*    LOOP AT it_doc INTO _doc.
*
*      CALL METHOD obj_tx_curva->agrupa_dados
*        EXPORTING
*          i_numero = _doc-doc_simulacao
*          i_tipo   = 'VDI'
*        IMPORTING
*          i_0041   = it_41.
*
*      PERFORM pf_exe_add_90_to_41 USING _doc-doc_simulacao.
*
*      LOOP AT it_41 INTO _0041.
*
*        CLEAR: _saida, sum_a, calc_m, sum_m.
*
*        _0040 = it_0040[ doc_simulacao = _0041-doc_simulacao ].
*
*        _saida-area = 'IN'.
*        _saida-tipo = 'VDI'.
*
*        _saida-nro_sol_ov = _0041-doc_simulacao.
*        _saida-fixacao    = |{ _0041-posnr ALPHA = IN }|.
*        _saida-tpsim      = _0040-tpsim.
*        _saida-waerk      = _0040-waerk.
*        _saida-status     = _0040-status.
*        _saida-bezei      = SWITCH #( _0041-spart
*                                      WHEN '03' THEN 'DF'
*                                      WHEN '02' THEN 'FT'
*                                      WHEN '04' THEN |S{ _0040-cultura(1) }|
*                                    ).
*
*        _saida-vl94       = REDUCE #(
*                                      INIT x TYPE zdmbtr
*                                        FOR ls IN it_0094
*                                      WHERE ( nro_sol_ov EQ  _0041-doc_simulacao
*                                          AND tipo EQ _saida-tipo
*                                          AND ( bezei EQ _saida-bezei
*                                             OR bezei EQ |{ _saida-bezei }D| )
*                                             )
*                                       NEXT x = x + ls-total_proporc
*                                    ).
*        CHECK _saida-tpsim NE 'BN'.
*
*        ADD _0041-vlrtot TO _saida-vl59.
*
*        PERFORM calc_90_vdi TABLES it_0090 USING _0041 CHANGING _saida-vl59.
*
*        _saida-diferenca = _saida-vl59 - _saida-vl94.
*
*        IF _saida-waerk EQ 'USD'.
*          IF _saida-vl94 IS INITIAL.
*            CONTINUE.
*          ELSE.
*            APPEND _saida TO it_saida.
*            CONTINUE.
*          ENDIF.
*        ENDIF.
*
*        IF _saida-status EQ 'A'.
*          IF _saida-vl94 IS NOT INITIAL
*            AND _saida-diferenca IS INITIAL.
*            CONTINUE.
*          ENDIF.
*        ELSE.
*          IF _saida-vl94 IS INITIAL.
*            CONTINUE.
*          ELSE.
*            APPEND _saida TO it_saida.
*            CONTINUE.
*          ENDIF.
*        ENDIF.
*
*        IF _saida-diferenca IS NOT INITIAL.
*          APPEND _saida TO it_saida.
*        ENDIF.
*
*        CLEAR: _saida, sum_a, _0040, calc_m ,sum_m.
*
*      ENDLOOP.
*
**  DATA(IT_DOC) = IT_0041.
**  SORT IT_DOC BY DOC_SIMULACAO.
**  DELETE ADJACENT DUPLICATES FROM IT_DOC COMPARING DOC_SIMULACAO.
**
*** "// Retorna a porcentagem de calculo para o frete
**  PERFORM GET_FRETE CHANGING PORCENTAGEM_FRE.
**
**  LOOP AT IT_DOC INTO DATA(_DOC).
**
*      CALL METHOD obj_tx_curva->agrupa_dados
*        EXPORTING
*          i_numero = _doc-doc_simulacao
*          i_tipo   = 'FRI'
*        IMPORTING
*          i_0041   = it_41.
*
*      PERFORM pf_exe_add_90_to_41 USING _doc-doc_simulacao.
*
*      LOOP AT it_41 INTO DATA(w_41).
*
*        _0040 = it_0040[ doc_simulacao = w_41-doc_simulacao ].
*
*        vl_kursk = it_0117[ bukrs = _0040-vkorg ]-kursk.
*
*        CLEAR: _saida.
*
*        _saida-area = 'IN'.
*        _saida-tipo = 'FRI'.
*
*        _saida-nro_sol_ov = w_41-doc_simulacao.
*        _saida-fixacao    = |{ w_41-posnr ALPHA = IN }|.
*        _saida-tpsim      = _0040-tpsim.
*        _saida-waerk      = _0040-waerk.
*        _saida-status     = _0040-status.
*        _saida-bezei      = SWITCH #( w_41-spart
*                                      WHEN '03' THEN 'DF'
*                                      WHEN '02' THEN 'FT'
*                                      WHEN '04' THEN |S{ _0040-cultura(1) }|
*                                    ).
*
*        _saida-vl94       = REDUCE #(
*                                      INIT x TYPE zdmbtr
*                                        FOR ls IN it_0094
*                                      WHERE ( nro_sol_ov EQ  w_41-doc_simulacao
*                                          AND tipo EQ _saida-tipo
*                                          AND ( bezei EQ _saida-bezei
*                                          OR bezei EQ |{ _saida-bezei }D| )
*                                             )
*                                       NEXT x = x + ls-total_proporc
*                                    ).
*
*        IF w_41-spart EQ '03'.
*
*          IF _saida-waerk EQ 'USD'
*             AND vl_kursk IS NOT INITIAL.
*            _saida-vl59 = ( w_41-vlrtot * porcentagem_fre ) * vl_kursk.
*          ELSE.
*            _saida-vl59 = w_41-vlrtot * porcentagem_fre.
*          ENDIF.
*
*        ELSE.
*          _saida-vl59 = w_41-vlrtot.
*        ENDIF.
*
*        PERFORM calc_90_fri TABLES it_0090 USING w_41 CHANGING _saida-vl59.
*
*        _saida-vl59 = abs( _saida-vl59 ).
*        _saida-vl94 = abs( _saida-vl94 ).
*
*        _saida-diferenca = _saida-vl94 - _saida-vl59.
*
*        IF _saida-status NE 'A' AND _saida-vl94 IS INITIAL.
*          CONTINUE.
*        ENDIF.
*
*        IF _saida-diferenca IS NOT INITIAL.
*          APPEND _saida TO it_saida.
*        ENDIF.
*
*      ENDLOOP.
*    ENDLOOP.
*
*    SORT it_saida BY nro_sol_ov.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  busca_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_dados .

  FREE it_saida.

  IF r_01 IS NOT INITIAL OR r_05 IS NOT INITIAL.
*  Processo do Mercado Interno
    PERFORM selecionar_dados_mi.
    PERFORM tratar_dados_mi.
  ENDIF.

  IF r_02 IS NOT INITIAL OR r_05 IS NOT INITIAL.
* Processo do INSUMOS
    PERFORM selecionar_dados_in.
    PERFORM tratar_dados_in.
*    PERFORM vencimento_in.
  ENDIF.

  IF r_03 IS NOT INITIAL OR r_05 IS NOT INITIAL.
* Processo do AQUAVIARIO, SERVIÇOS PORTUARIOS, TRANSBORDO.
    PERFORM seleciona_dados_vbrk.
    PERFORM trata_dados_vbrk.
  ENDIF.

  IF r_04 IS NOT INITIAL OR r_05 IS NOT INITIAL.
* Processo de Pedido ZMM0149 ME22N ME29N ME21N
    PERFORM seleciona_dados_ekko.
    PERFORM trata_dados_ekko.
  ENDIF.

  IF grid1 IS NOT INITIAL.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = _stable.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_FRETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_PORCENTAGEM_FRE  text
*----------------------------------------------------------------------*
FORM get_frete  CHANGING vl_porcentagem_fre.

  CLEAR vl_porcentagem_fre.

  DATA(it_set) = obj_tx_curva->get_auart( 'MAGGI_FRI_HEDGE' ).  "// Taxa do Frete em Porcentagem

  TRY .
      DATA(_set) = it_set[ 1 ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  CALL FUNCTION 'MOVE_CHAR_TO_NUM'
    EXPORTING
      chr             = _set-low
    IMPORTING
      num             = vl_porcentagem_fre
    EXCEPTIONS
      convt_no_number = 1
      convt_overflow  = 2
      OTHERS          = 3.

  vl_porcentagem_fre = vl_porcentagem_fre / 100.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALC_90
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_0090  text
*      -->P_W_41  text
*      <--P__SAIDA_VL59  text
*----------------------------------------------------------------------*
FORM calc_90_fri  TABLES   t_0090 STRUCTURE zsdt0090
              USING    _41 STRUCTURE zsdt0041
              CHANGING vl59.
  DATA: vlr_frete TYPE  zsdt0037-vlr_frete.

  LOOP AT it_0090 INTO DATA(wa_0090) WHERE doc_simulacao EQ _41-doc_simulacao AND matnrv EQ _41-matnr.

    CLEAR sum_a.

    CASE wa_0090-categoria.
      WHEN 'A'. "// Auteração de Quantidade

        IF wa_0090-spartv EQ '03'.

          IF _saida-waerk EQ 'USD'
         AND vl_kursk IS NOT INITIAL.
            sum_a = ( ( wa_0090-zmengv  * wa_0090-netprv ) * porcentagem_fre ) * vl_kursk.
          ELSE.
            sum_a = ( ( wa_0090-zmengv  * wa_0090-netprv ) * porcentagem_fre ).
          ENDIF.

        ELSE.
          sum_a = ( wa_0090-zmengv  / 1000 ) * _41-vlr_frete.
        ENDIF.

        ADD sum_a TO vl59.

      WHEN 'M'. "// Troca de Material

        PERFORM get_vlfrete USING wa_0090 'V' CHANGING sum_a.
        SUBTRACT sum_a FROM vl59.

    ENDCASE.

  ENDLOOP.

  LOOP AT it_0090 INTO wa_0090 WHERE doc_simulacao EQ _41-doc_simulacao AND matnr EQ _41-matnr.

    CLEAR sum_a.

    CASE wa_0090-categoria.
      WHEN 'M'. "// Troca de Material

        PERFORM get_vlfrete USING wa_0090 'N' CHANGING sum_a.
        ADD sum_a TO vl59.

    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_VLFRETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_0090  text
*      -->P__41  text
*      <--P_VLR_FRETE  text
*----------------------------------------------------------------------*
FORM get_vlfrete  USING    p_0090  STRUCTURE zsdt0090
                           p_dir
                  CHANGING p_vlr_frete.

  DATA: var_total     TYPE dmbtr,
        vl_qtde_equal TYPE c,
        valido        TYPE c VALUE abap_true.

  sy-cprog = 'ZSDR0042'.

  TRY .
      DATA(wa_0040) = it_0040[ doc_simulacao = p_0090-doc_simulacao ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  l_0090 = VALUE #(
                    docsi = p_0090-doc_simulacao
                    vbeln = COND #( WHEN p_dir EQ 'V' THEN p_0090-vbelv  ELSE p_0090-vbeln )
                    matnr = COND #( WHEN p_dir EQ 'V' THEN p_0090-matnrv ELSE p_0090-matnr )
                    matkl = COND #( WHEN p_dir EQ 'V' THEN p_0090-matklv ELSE p_0090-matkl )
                    werks = COND #( WHEN p_dir EQ 'V' THEN p_0090-werksv ELSE p_0090-werks )
                    kmein = COND #( WHEN p_dir EQ 'V' THEN p_0090-kmeinv ELSE p_0090-kmein )
                    zieme = COND #( WHEN p_dir EQ 'V' THEN p_0090-ziemev ELSE p_0090-zieme )
                    zmeng = COND #( WHEN p_dir EQ 'V' THEN p_0090-zmengv ELSE p_0090-zmeng )
                    netpr = COND #( WHEN p_dir EQ 'V' THEN p_0090-netprv ELSE p_0090-netpr )
                    inco1 = COND #( WHEN p_dir EQ 'V' THEN p_0090-inco1v ELSE p_0090-inco1 )
                    spart = COND #( WHEN p_dir EQ 'V' THEN p_0090-spartv ELSE p_0090-spart )
                  ).

  PERFORM pf_exec_validar_90 USING p_0090 CHANGING valido.

*  CALL FUNCTION 'ZSDMF001_COMPARE_UNIT_MAT'
*    EXPORTING
*      I_MATNR_01 = P_0090-MATNR
*      I_MENGE_01 = P_0090-ZMENG
*      I_MATNR_02 = P_0090-MATNRV
*      I_MENGE_02 = P_0090-ZMENGV
*    IMPORTING
*      E_EQUAL    = VL_QTDE_EQUAL.
*
*  IF ( P_0090-MATKLV EQ P_0090-MATKL ) AND
*     ( P_0090-MATKLV NE '658445'     ) AND
*     ( VL_QTDE_EQUAL EQ ABAP_FALSE   ).
*
*    CLEAR VALIDO.
*
*  ELSEIF ( P_0090-MATKLV NE P_0090-MATKL ) OR
*         ( P_0090-INCO1V NE P_0090-INCO1 ).
*
*    CLEAR VALIDO.
*
*  ENDIF.

  CHECK valido IS INITIAL.

  CASE l_0090-inco1.
    WHEN 'CIF' OR 'CPT' OR 'CFR'.
      IF ( 'CPT_CFR' CS l_0090-inco1 ).
        CHECK l_0090-spart EQ '03'.
      ENDIF.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

  TRY .
      DATA(wa_0041) = it_0041[ doc_simulacao = l_0090-docsi
                               vbeln         = l_0090-vbeln
                               matnr         = l_0090-matnr ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  TRY .
      DATA(wa_mara) = it_mara[ matnr = l_0090-matnr ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  IF wa_0041 IS NOT INITIAL AND wa_0041-inco1 NE 'FOB'.
    wa_0037-vlr_frete = wa_0041-vlr_frete.
    wa_0037-meins  = wa_0041-zieme.
  ELSE.
    TRY .
        wa_0037 = it_0037[ bukrs          = wa_0040-vkorg
                           matkl          = l_0090-matkl
                           filial_origem  = l_0090-werks
                           meins          = l_0090-kmein
                           filial_destino = wa_0040-vkbur ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDIF.

  CALL METHOD obj_tx_curva->set_numero
    EXPORTING
      i_numero = l_0090-docsi.    " Numero de Solicitação de Ordem de Venda

  CALL METHOD obj_tx_curva->set_matkl
    EXPORTING
      i_matkl = wa_mara-matkl    " Grupo de mercadorias
      i_brgew = wa_mara-brgew.   " Peso bruto

  CALL METHOD obj_tx_curva->set_netpr
    EXPORTING
      i_netpr = l_0090-netpr    " Preço líquido
      i_kmein = l_0090-kmein.   " Unidade de medida da condição

  CALL METHOD obj_tx_curva->set_zieme
    EXPORTING
      i_zieme = l_0090-zieme.    " UM qtd.prevista

  CALL METHOD obj_tx_curva->set_bezei
    EXPORTING
      i_bezei = CONV #( abap_false ).   " Denominação

  CALL METHOD obj_tx_curva->set_cadencia_in
    EXPORTING
      i_cadencia = l_0090-zmeng. " Qtd.prevista em UMV

  CALL METHOD obj_tx_curva->set_cadencia_in
    EXPORTING
      i_cadencia = obj_tx_curva->get_cadencia( ) " Qtd.prevista em UMV
      i_negativa = 'S'                           " Código de uma posição
      i_0040     = wa_0040.                      " Simulador de Vendas - dados de cabeçalho

  CALL METHOD obj_tx_curva->set_frete_in
    EXPORTING
      i_frete = wa_0037-vlr_frete " Montante ou porcentagem da condição
      i_zieme = wa_0037-meins.    " UM qtd.prevista

  IF l_0090-spart EQ '03'.

    IF ( wa_0040-waerk = 'USD' ) AND ( vl_kursk IS NOT INITIAL ).
      var_total = ( ( ( l_0090-zmeng * l_0090-netpr ) * porcentagem_fre ) * -1 ) * vl_kursk.
    ELSE.
      var_total = ( ( l_0090-zmeng * l_0090-netpr ) * porcentagem_fre ) * -1.
    ENDIF.

    CALL METHOD obj_tx_curva->set_total_proporcional
      EXPORTING
        i_total = var_total.   " Montante em moeda interna

  ENDIF.

  CALL METHOD obj_tx_curva->get_total_proporcional
    RECEIVING
      e_total = var_total.   " Montante em moeda interna

  var_total = abs( var_total ).

  ADD var_total TO p_vlr_frete.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALC_90_VDI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      --> T_0090  text
*      --> _0041  text
*      <-- VL59  text
*----------------------------------------------------------------------*
FORM calc_90_vdi  TABLES   t_0090 STRUCTURE zsdt0090
                  USING    _0041 STRUCTURE zsdt0041
                  CHANGING vl59.

  DATA: seguir TYPE char1.

  LOOP AT it_0090 INTO DATA(wa_0090) WHERE doc_simulacao EQ _0041-doc_simulacao AND matnrv EQ _0041-matnr.

    CLEAR: sum_a, seguir.
    PERFORM pf_exc_validacao_90 USING wa_0090 CHANGING seguir.

    CHECK seguir IS NOT INITIAL.

    CASE wa_0090-categoria.
      WHEN 'A'. "// Auteração de Quantidade
        PERFORM pf_exc_status_a USING wa_0090 CHANGING sum_a.
        ADD sum_a TO vl59.
      WHEN 'M'. "// Troca de Material
        PERFORM pf_exc_status_m USING wa_0090 'V' CHANGING sum_a.
        SUBTRACT sum_a FROM vl59.
    ENDCASE.

  ENDLOOP.

  LOOP AT it_0090 INTO wa_0090 WHERE doc_simulacao EQ _0041-doc_simulacao AND matnr EQ _0041-matnr.

    CLEAR: sum_a, seguir.
    PERFORM pf_exc_validacao_90 USING wa_0090 CHANGING seguir.

    CHECK seguir IS NOT INITIAL.

    CASE wa_0090-categoria.
      WHEN 'M'. "// Troca de Material
        PERFORM pf_exc_status_m USING wa_0090 'N' CHANGING sum_a.
        ADD sum_a TO vl59.
    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_EXC_STATUS_A
*&---------------------------------------------------------------------*
*       "// Auteração de Quantidade
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_exc_status_a USING p_0090 STRUCTURE zsdt0090
                     CHANGING p_vlr_a.

  DATA: var_total TYPE dmbtr,
        taxa_0090 TYPE kursf.

  sy-cprog = 'ZSDR0042'.

  TRY .
      DATA(wa_0040) = it_0040[ doc_simulacao = p_0090-doc_simulacao ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  l_0090 = VALUE #(
                    docsi = p_0090-doc_simulacao
                    vbeln = p_0090-vbelv
                    matnr = p_0090-matnrv
                    matkl = p_0090-matklv
                    werks = p_0090-werksv
                    kmein = p_0090-kmeinv
                    zieme = p_0090-ziemev
                    zmeng = p_0090-zmengv
                    netpr = p_0090-netprv
                    inco1 = p_0090-inco1v
                    spart = p_0090-spartv
                  ).

  TRY .
      DATA(wa_mara) = it_mara[ matnr = l_0090-matnr ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  TRY .
      DATA(wa_vbak) = it_vbak[ vbeln = l_0090-vbeln ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  TRY .
      DATA(wa_vbap) = it_vbap[ vbeln = l_0090-vbeln
                               matnr = l_0090-matnr ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  CALL METHOD obj_tx_curva->set_numero
    EXPORTING
      i_numero = l_0090-docsi.    " Numero de Solicitação de Ordem de Venda

  CALL METHOD obj_tx_curva->set_matkl
    EXPORTING
      i_matkl = wa_mara-matkl    " Grupo de mercadorias
      i_brgew = wa_mara-brgew.   " Peso bruto

  CALL METHOD obj_tx_curva->set_zieme
    EXPORTING
      i_zieme = l_0090-zieme.    " UM qtd.prevista

  CALL METHOD obj_tx_curva->set_netpr
    EXPORTING
      i_netpr = l_0090-netpr   " Preço líquido
      i_kmein = l_0090-kmein.  " Unidade de medida da condição

  CALL METHOD obj_tx_curva->set_bezei
    EXPORTING
      i_bezei = CONV #( abap_false ).   " Denominação

  CALL METHOD obj_tx_curva->set_cadencia_in
    EXPORTING
      i_cadencia = l_0090-zmeng    " Qtd.prevista em UMV
      i_tipo     = 'VDI'.          " Campo de 3 bytes de comprimento

  CALL METHOD obj_tx_curva->set_tipo
    EXPORTING
      i_tipo = 'VDI'.    " Tipo

  "Voltar Desconto para o item da O.V
  LOOP AT it_konv INTO DATA(wa_konv) WHERE knumv EQ wa_vbak-knumv
                                       AND kposn EQ wa_vbap-posnr.
    IF wa_konv-kbetr < 0.
      wa_vbap-netwr = wa_vbap-netwr + abs( wa_konv-kbetr ).
    ELSE.
      wa_vbap-netwr = wa_vbap-netwr - abs( wa_konv-kbetr ).
    ENDIF.
  ENDLOOP.

  IF wa_0040-waerk = 'USD'.

    CALL FUNCTION 'ZSDMF001_CHECK_OV_TRAVA_CAMBIO'
      EXPORTING
        i_doc_simulacao = l_0090-docsi
        i_vbeln         = l_0090-vbeln
      CHANGING
        c_taxa          = taxa_0090.

    var_total = ( ( wa_vbap-netwr + wa_vbap-mwsbp ) / wa_vbap-kwmeng ) * l_0090-zmeng * taxa_0090.
  ELSE.
    var_total = ( ( wa_vbap-netwr + wa_vbap-mwsbp ) / wa_vbap-kwmeng ) * l_0090-zmeng.
  ENDIF.

  CALL METHOD obj_tx_curva->set_total_proporcional
    EXPORTING
      i_total = var_total.   " Montante em moeda interna

  CALL METHOD obj_tx_curva->get_total_proporcional
    RECEIVING
      e_total = var_total.   " Montante em moeda interna

*  VAR_TOTAL = ABS( VAR_TOTAL ).

  ADD var_total TO p_vlr_a.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_EXC_STATUS_M
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_0090  text
*      <--P_SUM_A  text
*----------------------------------------------------------------------*
FORM pf_exc_status_m  USING    p_0090 STRUCTURE zsdt0090
                               p_dir
                      CHANGING p_vlr_a.

  DATA: var_total TYPE dmbtr,
        taxa_0090 TYPE kursf.

  sy-cprog = 'ZSDR0042'.

  TRY .
      DATA(wa_0040) = it_0040[ doc_simulacao = p_0090-doc_simulacao ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.


  l_0090 = VALUE #(
                    docsi = p_0090-doc_simulacao
                    vbeln = COND #( WHEN p_dir EQ 'V' THEN p_0090-vbelv  ELSE p_0090-vbeln )
                    matnr = COND #( WHEN p_dir EQ 'V' THEN p_0090-matnrv ELSE p_0090-matnr )
                    matkl = COND #( WHEN p_dir EQ 'V' THEN p_0090-matklv ELSE p_0090-matkl )
                    werks = COND #( WHEN p_dir EQ 'V' THEN p_0090-werksv ELSE p_0090-werks )
                    kmein = COND #( WHEN p_dir EQ 'V' THEN p_0090-kmeinv ELSE p_0090-kmein )
                    zieme = COND #( WHEN p_dir EQ 'V' THEN p_0090-ziemev ELSE p_0090-zieme )
                    zmeng = COND #( WHEN p_dir EQ 'V' THEN p_0090-zmengv ELSE p_0090-zmeng )
                    netpr = COND #( WHEN p_dir EQ 'V' THEN p_0090-netprv ELSE p_0090-netpr )
                    inco1 = COND #( WHEN p_dir EQ 'V' THEN p_0090-inco1v ELSE p_0090-inco1 )
                    spart = COND #( WHEN p_dir EQ 'V' THEN p_0090-spartv ELSE p_0090-spart )
                    netwr = p_0090-netwr
                  ).

  TRY .
      DATA(wa_mara) = it_mara[ matnr = l_0090-matnr ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  TRY .
      DATA(wa_vbak) = it_vbak[ vbeln = l_0090-vbeln ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  TRY .
      DATA(wa_vbap) = it_vbap[ vbeln = l_0090-vbeln
                               matnr = l_0090-matnr ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  CALL METHOD obj_tx_curva->set_numero
    EXPORTING
      i_numero = l_0090-docsi.    " Numero de Solicitação de Ordem de Venda

  CALL METHOD obj_tx_curva->set_matkl
    EXPORTING
      i_matkl = wa_mara-matkl    " Grupo de mercadorias
      i_brgew = wa_mara-brgew.   " Peso bruto

  CALL METHOD obj_tx_curva->set_zieme
    EXPORTING
      i_zieme = l_0090-zieme.    " UM qtd.prevista

  CALL METHOD obj_tx_curva->set_netpr
    EXPORTING
      i_netpr = l_0090-netpr   " Preço líquido
      i_kmein = l_0090-kmein.  " Unidade de medida da condição

  CALL METHOD obj_tx_curva->set_bezei
    EXPORTING
      i_bezei = CONV #( abap_false ).   " Denominação

  CALL METHOD obj_tx_curva->set_cadencia_in
    EXPORTING
      i_cadencia = COND #( WHEN wa_mara-matkl EQ '658445' THEN 0 ELSE l_0090-zmeng ).    " Qtd.prevista em UMV

  CALL METHOD obj_tx_curva->set_tipo
    EXPORTING
      i_tipo = 'VDI'. " Tipo

  CALL METHOD obj_tx_curva->set_zieme
    EXPORTING
      i_zieme = l_0090-zieme.    " UM qtd.prevista

  LOOP AT it_konv INTO DATA(wa_konv) WHERE knumv EQ wa_vbak-knumv
                                      AND kposn EQ wa_vbap-posnr.
    IF wa_konv-kbetr < 0.
      wa_vbap-netwr = wa_vbap-netwr + abs( wa_konv-kbetr ).
    ELSE.
      wa_vbap-netwr = wa_vbap-netwr - abs( wa_konv-kbetr ).
    ENDIF.
  ENDLOOP.

  IF p_dir EQ 'V'.
    IF wa_0040-waerk = 'USD'.

      CALL FUNCTION 'ZSDMF001_CHECK_OV_TRAVA_CAMBIO'
        EXPORTING
          i_doc_simulacao = l_0090-docsi
          i_vbeln         = l_0090-vbeln
        CHANGING
          c_taxa          = taxa_0090.

      CALL METHOD obj_tx_curva->calc_tot
        EXPORTING
          i_taxa = taxa_0090.    " Taxa de câmbio

    ELSE.
      CALL METHOD obj_tx_curva->set_total_proporcional
        EXPORTING
          i_total    = CONV #( l_0090-netwr )   " Montante em moeda interna
          i_negativa = COND #( WHEN l_0090-zmeng < 0
                               THEN abap_true
                               ELSE abap_false ).
    ENDIF.
  ELSE.
    CALL METHOD obj_tx_curva->set_total_proporcional
      EXPORTING
        i_total = CONV #( l_0090-netwr ).    " Montante em moeda interna
  ENDIF.

  CALL METHOD obj_tx_curva->get_total_proporcional
    RECEIVING
      e_total = var_total.   " Montante em moeda interna

  var_total = abs( var_total ).

  ADD var_total TO p_vlr_a.


ENDFORM.

*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_EXC_VALIDACAO_90
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_0090  text
*      <--P_SEGUIR  text
*----------------------------------------------------------------------*
FORM pf_exc_validacao_90  USING    p_0090 STRUCTURE zsdt0090
                          CHANGING seguir.

  IF p_0090-matkl EQ p_0090-matklv AND
     p_0090-inco1 EQ p_0090-inco1v AND
     p_0090-werks EQ p_0090-werksv.
    seguir = abap_false.
  ELSE.
    seguir = abap_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_EXE_ADD_90_TO_41
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P__DOC_DOC_SIMULACAO  text
*----------------------------------------------------------------------*
FORM pf_exe_add_90_to_41  USING p_doc.

  FREE it_41_aux.

*  LOOP AT IT_41 INTO DATA(_0041) WHERE DOC_SIMULACAO EQ P_DOC.
  LOOP AT it_0090 INTO DATA(wa_0090) WHERE doc_simulacao EQ p_doc.
    IF line_exists(
                    it_41_aux[
                               doc_simulacao = wa_0090-doc_simulacao
                               matnr = wa_0090-matnrv
                             ]
                  ).
      CONTINUE.
    ENDIF.

    IF line_exists(
                    it_41[
                               doc_simulacao = wa_0090-doc_simulacao
                               spart = wa_0090-spartv
                             ]
                  ).
      CONTINUE.
    ENDIF.

    IF 'M_A' NS wa_0090-categoria.
      CONTINUE.
    ENDIF.

    APPEND
    VALUE #(
            doc_simulacao = wa_0090-doc_simulacao
            posnr = wa_0090-posnv
            auart = wa_0090-auartv
            spart = wa_0090-spartv
            matnr = wa_0090-matnrv
            werks = wa_0090-werksv
            zieme = wa_0090-ziemev
*                VBELN = WA_0090-VBELV
            matkl = wa_0090-matklv
    ) TO it_41_aux.

    IF wa_0090-categoria EQ 'M'.
      APPEND
      VALUE #(
              doc_simulacao = wa_0090-doc_simulacao
              posnr = wa_0090-posnn
              auart = wa_0090-auart
              spart = wa_0090-spart
              matnr = wa_0090-matnr
              werks = wa_0090-werks
              zieme = wa_0090-zieme
              matkl = wa_0090-matkl
      ) TO it_41_aux.
    ENDIF.


  ENDLOOP.
*  ENDLOOP.

  SORT it_41_aux.
  DELETE ADJACENT DUPLICATES FROM it_41_aux COMPARING ALL FIELDS.

  APPEND LINES OF it_41_aux TO it_41.

  LOOP AT it_41 ASSIGNING FIELD-SYMBOL(<f_41>).
    CASE <f_41>-matkl.
      WHEN '700150' OR '658440'. "// Fertilizantes
        <f_41>-cultura_apl = 'DF'.
      WHEN '658445'. " Defensivos
        <f_41>-cultura_apl = 'FT'.
      WHEN '700230' OR '700130' OR '700240' OR '700350'. " Sementes
        IF <f_41>-matkl EQ '700240'.
          <f_41>-cultura_apl = 'SM'.
        ELSE.
          IF <f_41>-matkl EQ '700350'.
            <f_41>-cultura_apl = 'SA'.
          ELSE.
            <f_41>-cultura_apl = 'SS'.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_EXEC_VALIDAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P__SAIDA  text
*      <--P_VAR_CKECK  text
*----------------------------------------------------------------------*
FORM pf_exec_validar_41  USING    p_saida STRUCTURE _saida
                      CHANGING p_ckeck.

  CASE _saida-tipo.
    WHEN 'VDI'.

*      "// Bonificação e Permulta não dispara Hedge na Venda
      IF p_saida-tpsim EQ 'BN' OR
         p_saida-tpsim EQ 'PM'.
        IF p_saida-vl94 IS NOT INITIAL.
          p_ckeck = abap_true.
*        ELSE.
*          EXIT.
        ENDIF.
      ENDIF.


      " verificar outros comentarios na mesma data -->
      "//Venda em USD não Dispara o Hedge se existir adiciona para correção
      IF p_saida-waerk EQ 'USD'.
        IF p_saida-vl94 IS INITIAL.
          EXIT.
        ELSE.
          p_ckeck = abap_true.
          EXIT.
        ENDIF.
      ENDIF.

*      "// Simuladores aprovador tem que ter disparo de Hedge
*      "// caso não estejam aprovado e exista Hedge é inserido para Correção
      IF p_saida-status EQ 'A'.
        IF p_saida-vl94 IS NOT INITIAL
          AND p_saida-diferenca IS INITIAL.
          EXIT.
        ENDIF.
      ELSE.
        IF p_saida-vl94 IS INITIAL.
          EXIT.
        ELSE.
          p_ckeck = abap_true.
          EXIT.
        ENDIF.
      ENDIF.

    WHEN 'FRI'.

*      "// Permulta não dispara Hedge
      IF p_saida-tpsim EQ 'PM'.
        IF p_saida-vl94 IS NOT INITIAL.
          p_ckeck = abap_true.
          EXIT.
        ELSE.
          EXIT.
        ENDIF.
      ELSE.
*       "// caso for FOB não dispara o Hedge
        SELECT COUNT(*)
          FROM zsdt0041
          WHERE doc_simulacao EQ p_saida-nro_sol_ov
        AND inco1         IN ('CPT','CIF','CFR').
        IF sy-subrc IS NOT INITIAL.
          EXIT.
        ENDIF.
      ENDIF.

      IF p_saida-status NE 'A' AND p_saida-vl94 IS INITIAL.
        EXIT.
      ENDIF.

  ENDCASE.

  IF p_saida-diferenca IS NOT INITIAL.
    p_ckeck = abap_true.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_EXEC_MONTA_90
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P__DOC_SIMULACAO  text
*----------------------------------------------------------------------*
FORM pf_exec_monta_90_vdi USING p_simulacao i_dir i_spart CHANGING sum.

  DATA: var_total     TYPE dmbtr,
        v_total       TYPE dmbtr,
        vl_qtde_equal TYPE c,
        taxa_0090     TYPE kursf,
        vl_brgew      TYPE brgew_ap,
        vl_netwr      TYPE netwr_ap,
        vl_mwsbp      TYPE mwsbp.


  CLEAR: sum.

  DATA(_0040) = it_0040[ doc_simulacao = p_simulacao ].

  LOOP AT it_0090 INTO DATA(wa_0090) WHERE doc_simulacao EQ p_simulacao AND categoria EQ i_dir.

    IF wa_0090-spartv EQ i_spart AND wa_0090-categoria NE 'R'. "// "R" -> Redistribuição

*     "// Regra para ñ calcular a Troca caso esteja na mesma OV pq isso ñ dispara o Hedge
      IF wa_0090-categoria EQ 'M' AND wa_0090-vbeln EQ wa_0090-vbelv.
        CONTINUE.
      ENDIF.

      TRY .
          DATA(wa_vbak) = it_vbak[ vbeln = wa_0090-vbelv ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      IF wa_0090-categoria EQ 'C'.

        TRY .
            DATA(wa_vbap) = it_vbap[ vbeln = wa_0090-vbelv ].
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        CLEAR: vl_brgew, vl_netwr, vl_mwsbp.

        vl_brgew = REDUCE #( INIT x = vl_brgew FOR ls1 IN it_vbap WHERE ( vbeln =  wa_vbak-vbeln ) NEXT x = x + ls1-brgew ).
        vl_netwr = REDUCE #( INIT y = vl_netwr FOR ls2 IN it_vbap WHERE ( vbeln =  wa_vbak-vbeln ) NEXT y = y + ls2-netwr ).
        vl_mwsbp = REDUCE #( INIT z = vl_mwsbp FOR ls3 IN it_vbap WHERE ( vbeln =  wa_vbak-vbeln ) NEXT z = z + ls3-mwsbp ).

      ELSE.

        TRY .
            wa_vbap = it_vbap[ vbeln = wa_0090-vbelv
                                     matnr = wa_0090-matnrv ].
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

      ENDIF.

      " 06.06.2024 - RAMON Correção da seleção pq estava pegando o valor LIFSP = '12' -->

      IF wa_vbap-lifsp = '12'.

        LOOP AT it_vbak INTO wa_vbak WHERE vbeln_o = wa_vbap-vbeln AND  vbeln NE wa_vbap-vbeln.
        ENDLOOP.

        IF sy-subrc EQ 0.

          TRY .
              wa_vbap = it_vbap[ vbeln = wa_vbak-vbeln
                                       matnr = wa_0090-matnrv ].
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

        ENDIF.

      ENDIF.

      " 06.06.2024 -<<

      TRY .
          DATA(wa_mara) = it_mara[ matnr = wa_vbap-matnr ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      CLEAR: taxa_0090.
      IF _0040-waerk = 'USD'.
        CALL FUNCTION 'ZSDMF001_CHECK_OV_TRAVA_CAMBIO'
          EXPORTING
            i_doc_simulacao = wa_0090-doc_simulacao
            i_vbeln         = wa_0090-vbelv
          CHANGING
            c_taxa          = taxa_0090.
      ENDIF.

      CALL FUNCTION 'ZSDMF001_COMPARE_UNIT_MAT'
        EXPORTING
          i_matnr_01 = wa_0090-matnr
          i_menge_01 = wa_0090-zmeng
          i_matnr_02 = wa_0090-matnrv
          i_menge_02 = wa_0090-zmengv
        IMPORTING
          e_equal    = vl_qtde_equal.

      obj_tx_curva->set_matkl( i_matkl = wa_mara-matkl
                               i_brgew = wa_mara-brgew
                               ).
      obj_tx_curva->set_zieme( wa_0090-ziemev ).
      obj_tx_curva->set_netpr( i_netpr = wa_0090-netprv
                               i_kmein = wa_0090-kmeinv ).
      obj_tx_curva->set_bezei( CONV #( abap_false ) ).
      obj_tx_curva->set_cadencia_in( i_cadencia = wa_0090-zmengv
                                     i_tipo     = 'VDI' ).
      obj_tx_curva->set_tipo( 'VDI' ).

      CASE wa_0090-categoria.
        WHEN 'A'.

          LOOP AT it_konv INTO DATA(wa_konv) WHERE knumv EQ wa_vbak-knumv AND kposn EQ wa_vbap-posnr.
            IF wa_konv-kbetr < 0.
              wa_vbap-netwr = wa_vbap-netwr + abs( wa_konv-kbetr ).
            ELSE.
              wa_vbap-netwr = wa_vbap-netwr - abs( wa_konv-kbetr ).
            ENDIF.
          ENDLOOP.

          var_total = ( ( wa_vbap-netwr + wa_vbap-mwsbp ) / wa_vbap-kwmeng ) * wa_0090-zmengv.

          IF _0040-waerk EQ 'USD'.
            MULTIPLY var_total BY taxa_0090.
          ENDIF.

          ADD var_total TO sum.

        WHEN 'M'.

          obj_tx_curva->set_zieme( wa_0090-ziemev ).

          IF _0040-waerk EQ 'USD'.
            obj_tx_curva->calc_tot( i_taxa = taxa_0090 ).
          ELSE.
            obj_tx_curva->set_total_proporcional( i_total = CONV #( wa_0090-netwr )
                                                  i_negativa = COND #( WHEN wa_0090-zmengv < 0
                                                                       THEN abap_true
                                                                       ELSE abap_false ) ).
          ENDIF.
          var_total = obj_tx_curva->get_total_proporcional( ).

          ADD var_total TO sum.

        WHEN 'C'.

          _saida-waerk = |{ _saida-waerk(2) }C|.
          obj_tx_curva->set_cadencia_in( i_cadencia = COND #( WHEN wa_0090-matklv EQ '658445' THEN 0 ELSE CONV #( vl_brgew ) ) ).
          var_total = ( ( vl_netwr + vl_mwsbp ) * wa_0090-kurrf ).

          ADD var_total TO sum.

        WHEN 'E'.

          obj_tx_curva->set_zieme( wa_0090-ziemev ).
          obj_tx_curva->calc_tot( i_taxa = COND #( WHEN _0040-waerk EQ 'USD' THEN taxa_0090 ELSE 0 ) ).
          var_total = obj_tx_curva->get_total_proporcional( ).

          ADD var_total TO sum.

      ENDCASE.

    ENDIF.

    CHECK NOT wa_0090-vbeln IS INITIAL.

    IF wa_0090-spart EQ i_spart.

      TRY .
          wa_mara = it_mara[ matnr = wa_0090-matnr ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      TRY .
          wa_vbak = it_vbak[ vbeln = wa_0090-vbeln ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      TRY .
          wa_vbap = it_vbap[ vbeln = wa_0090-vbeln
                                   matnr = wa_0090-matnr ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      obj_tx_curva->set_matkl( i_matkl = wa_mara-matkl
                               i_brgew = wa_mara-brgew
                               ).
      obj_tx_curva->set_zieme( wa_0090-zieme ).
      obj_tx_curva->set_bezei( CONV #( abap_false ) ).
      obj_tx_curva->set_tipo( 'VDI' ).

      CASE wa_0090-categoria.
        WHEN 'R'.

          obj_tx_curva->set_zieme( wa_0090-zieme ).
          obj_tx_curva->set_cadencia_in( abs( wa_0090-zmeng ) ).
          obj_tx_curva->set_netpr( i_netpr = wa_0090-netpr i_kmein = wa_0090-kmein ).
          obj_tx_curva->calc_tot( i_taxa = COND #( WHEN _0040-waerk = 'USD' THEN taxa_0090 ELSE 0 ) ).
          var_total = obj_tx_curva->get_total_proporcional( ).


          obj_tx_curva->set_zieme( wa_0090-ziemev ).
          obj_tx_curva->set_cadencia_in( abs( wa_0090-zmengv ) ).
          obj_tx_curva->set_netpr( i_netpr = wa_0090-netprv i_kmein = wa_0090-kmeinv ).
          obj_tx_curva->calc_tot( i_taxa = COND #( WHEN _0040-waerk = 'USD' THEN taxa_0090 ELSE 0 ) ).
          v_total = obj_tx_curva->get_total_proporcional( ).

          obj_tx_curva->set_cadencia_in( 0 ).

          obj_tx_curva->set_total_proporcional( var_total - v_total ).

          var_total = obj_tx_curva->get_total_proporcional( ).

          ADD var_total TO sum.

        WHEN OTHERS.

          obj_tx_curva->set_cadencia_in( i_cadencia = COND #( WHEN wa_0090-matkl EQ '658445' THEN 0 ELSE wa_0090-zmeng ) ).

          IF ( wa_0090-categoria EQ 'M' ) AND
             ( wa_0090-matklv EQ wa_0090-matkl ) AND
             ( vl_qtde_equal EQ abap_false   ).

            obj_tx_curva->set_total_proporcional( 0 ).
            obj_tx_curva->set_tipo_taxa( '' ).

          ELSE.

            LOOP AT it_konv INTO wa_konv WHERE knumv EQ wa_vbak-knumv
                                           AND kposn EQ wa_vbap-posnr.

              DATA(coeficiente_diferenca) =
                zcl_solicitacao_ov=>get_imposto(
                _direcao = 'O'
                _vbeln   = wa_vbap-vbeln
                _posnr   = wa_vbap-posnr
              ).
              coeficiente_diferenca = COND #( WHEN coeficiente_diferenca IS INITIAL THEN 1 ELSE coeficiente_diferenca ).
              DIVIDE wa_vbap-netwr BY coeficiente_diferenca.

            ENDLOOP.

            var_total = ( wa_vbap-netwr / wa_vbap-kwmeng ) * wa_0090-zmeng.

            IF _0040-waerk = 'USD'.
              MULTIPLY var_total BY taxa_0090.
            ENDIF.

            obj_tx_curva->set_total_proporcional( COND #( WHEN wa_0090-categoria EQ 'M' THEN CONV #( wa_0090-netwr ) ELSE var_total ) ).

          ENDIF.

          var_total = obj_tx_curva->get_total_proporcional( ).

          ADD var_total TO sum.

      ENDCASE.

    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PF_EXEC_MONTA_90_FRI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P__DOC_DOC_SIMULACAO  text
*      -->P_4267   text
*      <--P_SUM_A  text
*----------------------------------------------------------------------*
FORM pf_exec_monta_90_fri USING p_simulacao i_dir i_spart CHANGING sum.

  DATA: var_total TYPE dmbtr,
        var_ckeck TYPE c VALUE abap_true,
        vl_inco1  TYPE zsdt0090-inco1,
        vl_spart  TYPE zsdt0090-spart,
        contador  TYPE i,
        v_inco    TYPE char3.


  PERFORM get_frete CHANGING porcentagem_fre.

  DATA(_0040) = it_0040[ doc_simulacao = p_simulacao ].

  LOOP AT it_0090 INTO DATA(wa_0090) WHERE doc_simulacao EQ p_simulacao AND categoria EQ i_dir.

    PERFORM pf_exec_validar_90 USING wa_0090 CHANGING var_ckeck.
    CHECK var_ckeck IS INITIAL.

    TRY .
        DATA(wa_0041) = it_0041[ doc_simulacao = wa_0090-doc_simulacao
                                 vbeln         = wa_0090-vbelv
                                 matnr         = wa_0090-matnrv ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    IF wa_0041 IS NOT INITIAL AND wa_0041-inco1 NE 'FOB'.
      wa_0037-vlr_frete = wa_0041-vlr_frete.
      wa_0037-meins     = wa_0041-zieme.
    ELSE.
      TRY .
          wa_0037 = it_0037[ bukrs          = _0040-vkorg
                             matkl          = wa_0090-matklv
                             filial_origem  = wa_0090-werksv
                             meins          = wa_0090-kmeinv
                             filial_destino = _0040-vkbur ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDIF.

    IF wa_0090-spartv EQ i_spart AND
       wa_0090-categoria NE 'R' AND
       wa_0090-categoria NE 'Y' AND
       wa_0090-categoria NE 'W'.

      CASE wa_0090-inco1v.
        WHEN 'CIF' OR 'CPT' OR 'CFR' OR 'FOB'.
*          IF ( ( 'CPT_CFR' CS wa_0090-inco1v ) AND ( wa_0090-categoria NE 'A' AND wa_0090-categoria NE 'F') ).
          IF 'CPT_CFR' CS wa_0090-inco1v AND wa_0090-categoria NE 'F'.
          ELSE.

            IF  wa_0090-inco1v EQ 'FOB' AND
              ( wa_0090-categoria EQ 'F' OR
                wa_0090-categoria EQ 'A' OR
                wa_0090-categoria EQ 'M' ).
              var_ckeck = abap_true.
            ENDIF.

            " 11.04.2024 - RAMON - 96174 -->
            " Motivo: estava com dump no QAS
            TRY .
                vl_kursk = it_0117[ bukrs = _0040-vkorg ]-kursk.
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.
            " 11.04.2024 - RAMON --<

            TRY .
                DATA(wa_mara) = it_mara[ matnr = wa_0090-matnrv ].
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.

            obj_tx_curva->set_matkl( i_matkl = wa_mara-matkl
                                     i_brgew = wa_mara-brgew
                                     ).
            obj_tx_curva->set_netpr( i_netpr = wa_0090-netprv
                                     i_kmein = wa_0090-kmeinv ).
            obj_tx_curva->set_zieme( wa_0090-ziemev ).
            obj_tx_curva->set_bezei( CONV #( abap_false ) ).

            obj_tx_curva->set_cadencia_in( i_cadencia =  wa_0090-zmengv ).
            obj_tx_curva->set_cadencia_in( i_cadencia = obj_tx_curva->get_cadencia( )
                                           i_negativa = 'S'
                                           ).
            CASE wa_0090-categoria.
              WHEN 'A' OR 'M' OR 'E' OR ' '.

                IF var_ckeck IS INITIAL.

                  IF wa_0090-categoria NE 'M'.
                    obj_tx_curva->set_zieme( COND #( WHEN wa_mara-matkl EQ '658445'
                                                          THEN wa_0090-ziemev
                                                          ELSE obj_tx_curva->get_zieme( )
                                                    )
                                           ).
                  ENDIF.

                  obj_tx_curva->set_frete_in( i_frete = wa_0037-vlr_frete
                                              i_zieme = wa_0037-meins
                                              ).

                  IF wa_0090-spartv EQ '03'.
                    var_total = ( ( wa_0090-zmengv * wa_0090-netprv ) * porcentagem_fre ) * -1.

                    IF _0040-waerk EQ 'USD'
                    AND vl_kursk IS NOT INITIAL.
                      MULTIPLY var_total BY vl_kursk.
                    ENDIF.

                    obj_tx_curva->set_total_proporcional( var_total ).

                  ENDIF.

                  var_total = obj_tx_curva->get_total_proporcional( ).
                  ADD var_total TO sum.

                ENDIF.

              WHEN 'F'.

*   fob -> cpt, fob -> cfr
                IF (  wa_0090-inco1v EQ 'FOB' ).
                  IF ( wa_0090-inco1 EQ 'CPT' AND wa_0090-spartv NE '03' ) OR ( wa_0090-inco1 EQ 'CFR' ).
                    CONTINUE.
                  ENDIF.
                ENDIF.

*   CPT -> FOB, CPT -> CFR
                IF (  wa_0090-inco1v EQ 'CPT' ).
                  IF ( wa_0090-inco1 EQ 'FOB' OR wa_0090-inco1 EQ 'CFR' ) AND wa_0090-spartv NE '03' .
                    CONTINUE.
                  ENDIF.
                ENDIF.

*   CFR -> FOB, CFR -> CPT
                IF (  wa_0090-inco1v EQ 'CFR' ).
                  IF ( wa_0090-inco1 EQ 'FOB' OR ( wa_0090-inco1 EQ 'CPT' AND wa_0090-spartv NE '03' ) ).
                    CONTINUE.
                  ENDIF.
                ENDIF.

                DATA(v_um) = -1.
                DATA(v_negativa) = 'S'.
                DATA(v_tp_tx) = 'V'.

                v_inco = wa_0090-inco1.

                IF v_inco NE 'CIF'.
                  v_um = 1.
                  v_negativa = 'N'.
                  v_tp_tx = 'C'.
                ENDIF.

                IF _0040-taxa_frete IS NOT INITIAL.
                  vl_kursk = _0040-taxa_frete.
                ENDIF.

                obj_tx_curva->set_cadencia_in( i_cadencia = obj_tx_curva->get_cadencia( )
                                               i_negativa = v_negativa
                                               ).

                IF wa_0090-spartv EQ '03'.

                  obj_tx_curva->set_zieme( wa_0090-ziemev ).

                  var_total = ( ( wa_0090-zmengv * wa_0090-netprv ) * porcentagem_fre ) * v_um.

                  IF _0040-waerk EQ 'USD'
                   AND vl_kursk IS NOT INITIAL.
                    MULTIPLY var_total BY vl_kursk.
                  ENDIF.

                  obj_tx_curva->set_total_proporcional( var_total ).
                  obj_tx_curva->set_cadencia_in( i_cadencia =  0 ).
                ELSE.
                  obj_tx_curva->set_frete_in( i_frete = wa_0037-vlr_frete
                                              i_zieme = wa_0037-meins
                                              ).
                ENDIF.

                var_total = obj_tx_curva->get_total_proporcional( ).
*                var_total = abs( var_total ).
                ADD var_total TO sum.

            ENDCASE.
          ENDIF.
      ENDCASE.
    ENDIF.


    CHECK NOT wa_0090-vbeln IS INITIAL.

    MOVE-CORRESPONDING wa_0037 TO wa_0037_aux.

    IF wa_0090-spart EQ i_spart.

      FREE: wa_0041, wa_0037, wa_mara.

      IF wa_0090-categoria EQ 'Y' OR wa_0090-categoria EQ 'W'. "// Y -> Devolução, W -> Complemento

        obj_tx_curva->set_tables( 0 ).

        CALL METHOD obj_tx_curva->get_firts_frete
          EXPORTING
            i_vbeln         = wa_0090-vbelv         " Nº documento de vendas e distribuição
            i_matnr         = wa_0090-matnrv        " Nº do material
            i_doc_simulacao = wa_0090-doc_simulacao " Numero do documento de simulação de venda
          RECEIVING
            w_0041          = wa_0041.  " Simulador de Vendas - dados de itens

        vl_inco1 = wa_0041-inco1.
        vl_spart = wa_0041-spart.

      ELSE.

        TRY .
            wa_0041 = it_0041[ doc_simulacao = wa_0090-doc_simulacao
                               vbeln         = wa_0090-vbeln
                               matnr         = wa_0090-matnr ].
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
        vl_inco1 = wa_0090-inco1.
        vl_spart = wa_0090-spart.

      ENDIF.

      TRY.
          wa_mara = it_mara[ matnr = wa_0090-matnr ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      IF wa_0041 IS NOT INITIAL AND wa_0041-inco1 NE 'FOB'.
        wa_0037-vlr_frete = wa_0041-vlr_frete.
        wa_0037-meins     = wa_0041-zieme.
      ELSE.
        TRY .
            wa_0037 = it_0037[ bukrs          = _0040-vkorg
                               matkl          = wa_0090-matkl
                               filial_origem  = wa_0090-werks
                               meins          = wa_0090-kmein
                               filial_destino = _0040-vkbur ].
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
      ENDIF.

      CASE vl_inco1.
        WHEN 'CIF' OR 'CPT' OR 'CFR'.
          IF ( 'CPT_CFR' CS vl_inco1 ).
            CHECK vl_spart EQ '03'.
          ENDIF.
        WHEN OTHERS.
          EXIT.
      ENDCASE.

      vl_kursk = it_0117[ bukrs = _0040-vkorg ]-kursk.

      TRY .
          wa_mara = it_mara[ matnr = wa_0090-matnr ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      obj_tx_curva->set_matkl( i_matkl = wa_mara-matkl
                               i_brgew = wa_mara-brgew
                               ).
      obj_tx_curva->set_bezei( CONV #( abap_false ) ).
      obj_tx_curva->set_tipo( 'FRI' ).

      CASE wa_0090-categoria.
        WHEN 'R'.

*         "// Dados da O.V Velha
          obj_tx_curva->set_zieme( wa_0090-ziemev ).
          obj_tx_curva->set_cadencia_in( abs( wa_0090-zmengv ) ).
          obj_tx_curva->set_frete_in( i_frete = wa_0037_aux-vlr_frete
                                      i_zieme = wa_0037_aux-meins
                                      ).
          IF wa_0090-spartv EQ '03'.
            var_total = abs( wa_0090-zmengv ) * wa_0090-netprv.
            IF _0040-waerk = 'USD' AND
              vl_kursk IS NOT INITIAL.
              MULTIPLY var_total BY vl_kursk.
            ENDIF.
          ELSE.
            var_total = obj_tx_curva->get_total_proporcional( ).
          ENDIF.

*         "// Dados da O.V Nova
          IF wa_0090-werks NE wa_0090-werksv.
            obj_tx_curva->set_zieme( wa_0090-zieme ).
            obj_tx_curva->set_cadencia_in( abs( wa_0090-zmeng ) ).
            obj_tx_curva->set_frete_in( i_frete = wa_0037-vlr_frete
                                        i_zieme = wa_0037-meins
                                        ).
          ENDIF.

          IF wa_0090-spart EQ '03'.
            var_total = ( var_total - ( abs( wa_0090-zmeng ) * wa_0090-netpr ) ) * porcentagem_fre.
            IF _0040-waerk = 'USD' AND
              vl_kursk IS NOT INITIAL.
              MULTIPLY var_total BY vl_kursk.
            ENDIF.
          ELSE.
            var_total = var_total - obj_tx_curva->get_total_proporcional( ).
          ENDIF.

          ADD var_total TO sum.

        WHEN 'M'.

          obj_tx_curva->set_zieme( wa_0090-zieme ).
          obj_tx_curva->set_cadencia_in( i_cadencia =  wa_0090-zmeng ).
          obj_tx_curva->set_cadencia_in( i_cadencia = obj_tx_curva->get_cadencia( )
                                         i_negativa = 'S'
                                         ).
          obj_tx_curva->set_frete_in( i_frete = wa_0037-vlr_frete
                                      i_zieme = wa_0037-meins
                                      ).

          IF wa_0090-spart EQ '03'.
            var_total = ( ( wa_0090-zmeng * wa_0090-netpr ) * porcentagem_fre ).
            IF _0040-waerk = 'USD' AND
               vl_kursk IS NOT INITIAL.
              MULTIPLY var_total BY vl_kursk.
            ENDIF.
          ELSE.
            var_total = obj_tx_curva->get_total_proporcional( ).
          ENDIF.

          obj_tx_curva->set_total_proporcional( i_total    = var_total
                                                i_negativa = abap_true ).

          var_total = obj_tx_curva->get_total_proporcional( ).
          var_total = abs( var_total ) * -1.

          ADD var_total TO sum.

        WHEN 'Y' OR 'W'.

          TRY .
              wa_0037 = it_0037[ bukrs          = _0040-vkorg
                                 matkl          = wa_0090-matklv
                                 filial_origem  = wa_0090-werksv
                                 meins          = wa_0090-kmeinv
                                 filial_destino = _0040-vkbur ].
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          obj_tx_curva->set_matkl( i_matkl = wa_mara-matkl
                                   i_brgew = wa_mara-brgew
                                   ).
          obj_tx_curva->set_zieme( wa_0090-zieme ).
          obj_tx_curva->set_bezei( CONV #( abap_false ) ).
          obj_tx_curva->set_tipo( 'FRI' ).

          obj_tx_curva->set_bezei( CONV #( wa_0090-categoria ) ).

          obj_tx_curva->set_cadencia_in( wa_0090-zmeng ).
          obj_tx_curva->set_cadencia_in( i_cadencia = obj_tx_curva->get_cadencia( )
                                         i_negativa = 'S'
                                         i_0040     = _0040
                                         ).

          obj_tx_curva->set_frete_in( i_frete = wa_0037-vlr_frete
                                      i_zieme = wa_0037-meins ).

          IF wa_0090-spart EQ '03'.
            var_total = ( ( wa_0090-zmeng * wa_0090-netpr ) * porcentagem_fre ) * -1.
            IF _0040-waerk = 'USD' AND
               vl_kursk IS NOT INITIAL.
              MULTIPLY var_total BY vl_kursk.
            ENDIF.
            obj_tx_curva->set_total_proporcional( i_total = var_total ).
          ENDIF.

          var_total = obj_tx_curva->get_total_proporcional( ).

          ADD var_total TO sum.

      ENDCASE.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_EXEC_VALIDAR_90
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_0090  text
*      <--P_VAR_CKECK  text
*----------------------------------------------------------------------*
FORM pf_exec_validar_90  USING    p_0090 STRUCTURE zsdt0090
                         CHANGING valido.

  DATA: vl_qtde_equal TYPE c.

  CALL FUNCTION 'ZSDMF001_COMPARE_UNIT_MAT'
    EXPORTING
      i_matnr_01 = p_0090-matnr
      i_menge_01 = p_0090-zmeng
      i_matnr_02 = p_0090-matnrv
      i_menge_02 = p_0090-zmengv
    IMPORTING
      e_equal    = vl_qtde_equal.

  IF ( p_0090-matklv EQ p_0090-matkl ) AND
     ( p_0090-matklv NE '658445'     ) AND
     ( vl_qtde_equal EQ abap_false   ).

    CLEAR valido.

  ELSEIF ( p_0090-matklv NE p_0090-matkl ) OR
         ( p_0090-inco1v NE p_0090-inco1 ).

    CLEAR valido.

  ENDIF.

  IF p_0090-netpr NE p_0090-netprv AND
     p_0090-matklv EQ '658445' AND
     p_0090-categoria EQ 'R'.
    CLEAR valido.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VENCIMENTO_IN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vencimento_in .

  DATA sequencia TYPE zsdt0094-fixacao.

  LOOP AT it_0090 INTO DATA(wa_0090) WHERE categoria EQ 'V'.

    sequencia = |{ wa_0090-sequencia ALPHA = IN }|.

    IF NOT line_exists( it_0094[ nro_sol_ov = wa_0090-doc_simulacao fixacao = sequencia ] ).
      _saida-area = 'IN'.
      _saida-tipo = 'VEN'.
      _saida-nro_sol_ov = wa_0090-doc_simulacao.
      _saida-fixacao    = sequencia.
      APPEND _saida TO it_saida.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_VBRK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados_ekko.

  FREE: it_zmmt0037,
        it_ekpo_aux.

  DATA(r_bsart) = obj_tx_curva->get_auart( 'ZSDR0119_BSART' ).

  SELECT *
    FROM ekko
    INTO TABLE it_ekko
   WHERE "aedat GE at_datainicial
         aedat IN s_data
     AND ebeln IN s_solov
     AND bsart IN r_bsart
     AND bukrs IN s_bukrs
  AND waers EQ 'BRL'.

  CHECK it_ekko[] IS NOT INITIAL.

  SELECT *
    FROM ekpo
    INTO TABLE it_ekpo
     FOR ALL ENTRIES IN it_ekko
  WHERE ebeln = it_ekko-ebeln.

  SELECT *
    FROM zmmt0035
    INTO TABLE it_zmmt0035
     FOR ALL ENTRIES IN it_ekko
  WHERE ebeln = it_ekko-ebeln.

  IF it_zmmt0035[] IS NOT INITIAL.
    SELECT *
      FROM zmmt0037
      INTO TABLE it_zmmt0037
       FOR ALL ENTRIES IN it_zmmt0035
    WHERE nro_sol_cp = it_zmmt0035-nro_sol_cp.
  ENDIF.

  DELETE it_zmmt0037 WHERE ebeln IS INITIAL.

  IF it_zmmt0037[] IS NOT INITIAL.
    SELECT *
      FROM ekpo
      INTO TABLE it_ekpo_aux
       FOR ALL ENTRIES IN it_zmmt0037
    WHERE ebeln = it_zmmt0037-ebeln.
  ENDIF.

  SELECT *
    FROM zsdt0094
    INTO TABLE it_zsdt0094
     FOR ALL ENTRIES IN it_ekko
  WHERE nro_sol_ov = it_ekko-ebeln.

  " 26.03.2025 - 170634 - RAMON -->
  PERFORM f_seleciona_dados_email.
  " 26.03.2025 - 170634 - RAMON <--

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_VBRK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados_vbrk .

  DATA(obj_tx_curva)    = NEW zcl_taxa_curva( ).
  DATA(obj_tx_curva_db) = NEW zcl_taxa_curva_db( ).
  DATA: dt_ini TYPE bsak-augdt.

  DATA(r_ini) = obj_tx_curva->get_auart( 'ZFI0064_INI_AQUAV' ).

  TRY .
      dt_ini = r_ini[ 1 ]-low.
    CATCH cx_sy_itab_line_not_found.
      EXIT.
  ENDTRY.

  CHECK dt_ini LE sy-datum AND dt_ini IS NOT INITIAL.

  DATA(r_aqv) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_AQV' ).
  DATA(r_spt) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_SPT' ).
  DATA(r_tbo) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_TB' ).

  DATA(r_ger) = r_aqv.
  APPEND LINES OF r_spt TO r_ger.
  APPEND LINES OF r_tbo TO r_ger.

  CHECK r_ger IS NOT INITIAL.

  DATA(r_bur) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_BUKRS' ).

  CHECK r_bur IS NOT INITIAL.

  SELECT *
    FROM vbrk
    INTO TABLE it_vbrk
    WHERE "fkdat GE at_datainicial
          fkdat IN s_data
      AND vbeln IN s_solov
      AND fkart IN r_ger
      AND vkorg IN r_bur
      AND vkorg IN s_bukrs
      AND waerk EQ 'BRL'
* ---> S4 Migration - 17/07/2023 - LA
  AND draft EQ space.
* <--- S4 Migration - 17/07/2023 - LA
  CHECK it_vbrk IS NOT INITIAL.

  SELECT *
  FROM zsdt0094
  INTO CORRESPONDING FIELDS OF TABLE it_0094
    FOR ALL ENTRIES IN it_vbrk
  WHERE nro_sol_ov EQ it_vbrk-vbeln.

  SELECT *
    FROM vbrp
    INTO TABLE it_vbrp
    FOR ALL ENTRIES IN it_vbrk
  WHERE vbeln EQ it_vbrk-vbeln.

  SELECT *
    FROM kna1
    INTO TABLE it_kna1
    FOR ALL ENTRIES IN it_vbrk
  WHERE kunnr EQ it_vbrk-kunag.

  SELECT *
    FROM t052
    INTO TABLE it_t052
    FOR ALL ENTRIES IN it_vbrk
  WHERE zterm EQ it_vbrk-zterm.

  " 26.03.2025 - 170634 - RAMON -->
  PERFORM f_seleciona_dados_email.
  " 26.03.2025 - 170634 - RAMON <--

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TRATA_DADOS_VBRK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trata_dados_ekko.

  LOOP AT it_ekko INTO DATA(_ekko).

    CLEAR: _saida,  l_vl59a, l_vl59b, wa_zmmt0035, wa_zmmt0037,
           l_valor, l_wmwst.

    READ TABLE it_zmmt0035 INTO wa_zmmt0035 WITH KEY ebeln      = _ekko-ebeln.
    READ TABLE it_zmmt0037 INTO wa_zmmt0037 WITH KEY nro_sol_cp = wa_zmmt0035-nro_sol_cp.
    READ TABLE it_ekpo     INTO DATA(_ekpo) WITH KEY ebeln      = _ekko-ebeln.

    obj_tx_curva->set_tipo( i_tipo = 'PDI' ).

    _saida-area       = 'MM'.
    _saida-tipo       = obj_tx_curva->get_tipo( ).
    _saida-nro_sol_ov = _ekko-ebeln.
    _saida-waerk      = _ekko-waers.
    _saida-status     = COND #( WHEN _ekko-procstat = '05' THEN 'L' ELSE 'N' ).

    _saida-vl94       = REDUCE #( INIT x TYPE zdmbtr
                                   FOR ls  IN it_zsdt0094
                                 WHERE ( nro_sol_ov = _saida-nro_sol_ov )
                                  NEXT x = x + ls-total_proporc
                                ).

    l_vl59a           = REDUCE #( INIT x TYPE zdmbtr
                                   FOR ls2  IN it_ekpo
                                 WHERE ( ebeln = _saida-nro_sol_ov )
                                  NEXT x = x + ls2-netwr
                                ).

    l_vl59b           = REDUCE #( INIT x TYPE zdmbtr
                                   FOR ls3  IN it_ekpo_aux
                                 WHERE ( ebeln = wa_zmmt0037-ebeln )
                                  NEXT x = x + ls3-netwr
                                ).

    _saida-vl59       = l_vl59a + l_vl59b.
*    _saida-vl59       = l_vl59a.

    PERFORM r_imposto_item(zfir0031) USING _ekko-lifnr
                                           _ekpo-werks
                                           _ekpo-ebelp
                                           _ekpo-ebeln
                                 CHANGING  l_valor
                                           l_wmwst.

    _saida-vl59       = _saida-vl59 + l_wmwst.
    _saida-diferenca  = _saida-vl59 - _saida-vl94.

    IF _saida-diferenca IS NOT INITIAL.
      APPEND _saida TO it_saida.
    ENDIF.

  ENDLOOP.

  " 04.04.2025 - 170634 - RAMON -->
  PERFORM f_info_email.
  " 04.04.2025 - 170634 - RAMON --<

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TRATA_DADOS_VBRK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trata_dados_vbrk.

  CONSTANTS: venda        TYPE c VALUE 'V',
             compra       TYPE c VALUE 'C',
             kurst        TYPE c VALUE 'B',
             waerk        TYPE c LENGTH 3 VALUE 'USD',
             tcurr        TYPE c LENGTH 3 VALUE 'BRL',
             intercompany TYPE c LENGTH 4 VALUE 'ZCIC'.

  DATA: werks              TYPE werks_d,
        var_data           TYPE datum,
        zdata              TYPE datum,
        var_data_calculada TYPE d,
        var_mes            TYPE n LENGTH 2,
        var_mes_aux        TYPE c LENGTH 2,
        var_ano            TYPE c LENGTH 4,
        var_w              TYPE char1.

  CHECK it_vbrk IS NOT INITIAL.

  DATA(obj_tx_curva)    = NEW zcl_taxa_curva( ).
  DATA(obj_tx_curva_db) = NEW zcl_taxa_curva_db( ).
  DATA(obj_w_tx_curva)  = NEW zcl_webservice_tx_curva( ).
  DATA(obj_util_sd)     = NEW zcl_util_sd( ).

  DATA(r_emp) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_EX_EMP' ).  "// Empresa que não Dispara ZCIC

  DATA(r_ex1) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_EX_ZCIC' ). "// Grupo de Contas de Cliente
  DATA(r_cli) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_EX_CLI' ).  "// Exceção de Cliente que não dispara o Hedge
  DATA(r_cl2) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_EX_CL2' ).  "// Exceção de Cliente que dispara a segunda perna do Hedge

  DATA(r_ex2) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_EXCECAO' ). "// Grupo de Contas de Cliente

  LOOP AT it_vbrk INTO DATA(_vbrk).

    IF _vbrk-vkorg IN r_emp AND r_emp IS NOT INITIAL.
      DATA(r_exc) = r_ex1.
    ELSE.
      r_exc = r_ex2.
      FREE: r_cli, r_cl2.
    ENDIF.

*   "// Verifica se a Empresa esta na Exceção
    IF _vbrk-vkorg IN r_emp AND r_emp IS NOT INITIAL.
      IF r_cli IS NOT INITIAL.
*   "// Não dispara se a Cliente esta na Exceção
        CHECK NOT _vbrk-kunag IN r_cli.
      ENDIF.
    ENDIF.

*   "// Inicio
*   "// Verifica se o Cliente é Intercompany
*   "// se for ZCIC não será disparado nenhuma perna do Hedge.
    READ TABLE it_kna1 INTO DATA(wa_kna1) WITH KEY kunnr = _vbrk-kunag.
    IF sy-subrc IS INITIAL.
*   "// Não dispara se a Grupo de Contas de Cliente esta na Exceção
      IF r_exc IS NOT INITIAL.
        CHECK NOT wa_kna1-ktokd IN r_exc.
      ENDIF.
    ENDIF.

    READ TABLE it_vbrp INTO DATA(_vbrp) WITH KEY vbeln = _vbrk-vbeln.

    obj_tx_curva->set_total_proporcional( CONV #( ( _vbrk-netwr + _vbrk-mwsbk ) ) ).
    obj_tx_curva->set_tipo( i_tipo = obj_tx_curva->get_tipo_auart( _vbrk ) ).

    _saida-area = 'LES'.
    _saida-tipo = obj_tx_curva->get_tipo( ).

    _saida-nro_sol_ov = _vbrk-vbeln.
    _saida-fixacao    = |{ _vbrp-posnr ALPHA = IN }|.
    _saida-waerk      = _vbrk-stwae.
    werks =  |{ _vbrk-kunag ALPHA = IN }|.

    SELECT SINGLE *
      FROM t001w
      INTO @DATA(wa_t001w)
    WHERE werks EQ @werks.

    _saida-bezei      = wa_t001w-vkorg.

    _saida-vl94       = REDUCE #(
                                  INIT x TYPE zdmbtr
                                    FOR ls IN it_0094
                                  WHERE ( nro_sol_ov EQ  _saida-nro_sol_ov
                                      AND tipo EQ _saida-tipo
                                      AND bezei EQ _saida-bezei
                                      AND tipo_taxa EQ 'C' )
                                   NEXT x = x + ls-total_proporc
                                ).

    _saida-vl59 = obj_tx_curva->get_total_proporcional( ).
    _saida-diferenca = _saida-vl59 - _saida-vl94.

    IF _saida-diferenca IS NOT INITIAL.
      APPEND _saida TO it_saida.
    ENDIF.

*   "// Verifica se o Cliente esta na Exceção de clientes que são intercompany para disparar o Hedge

    IF _vbrk-vkorg IN r_emp AND r_emp IS NOT INITIAL.
      IF wa_kna1-ktokd EQ intercompany.
        "// Verifica se o Cliente é Intercompany
        CHECK wa_kna1-ktokd EQ intercompany.
      ELSE.
        "// Verifica se é um cliente comum, mas para essa empresa(R_EMP) é tratado como Cliente Intercompany.
        CHECK r_cl2 IS NOT INITIAL.
        CHECK _vbrk-kunag IN r_cl2.
      ENDIF.
    ELSE.
*     "// Verifica se o Cliente é Intercompany
      CHECK wa_kna1-ktokd EQ intercompany.
    ENDIF.

    obj_tx_curva->set_total_proporcional( i_total = CONV #( _vbrk-netwr + _vbrk-mwsbk )
                                          i_negativa = abap_true ).

    "// Buscar qual é a empresa do cliente
    werks =  |{ _vbrk-kunag ALPHA = IN }|.

    SELECT SINGLE *
      FROM t001w
      INTO wa_t001w
    WHERE werks EQ werks.

    obj_tx_curva->set_bezei( CONV #( wa_t001w-vkorg ) ).

    _saida-area = 'LES'.
    _saida-tipo = obj_tx_curva->get_tipo( ).

    _saida-nro_sol_ov = _vbrk-vbeln.
    _saida-fixacao    = |{ _vbrp-posnr ALPHA = IN }|.
    _saida-waerk      = _vbrk-stwae.
    _saida-bezei      = obj_tx_curva->get_bezei( ).

    _saida-vl94       = REDUCE #(
                                  INIT x TYPE zdmbtr
                                    FOR ls IN it_0094
                                  WHERE ( nro_sol_ov EQ  _saida-nro_sol_ov
                                      AND tipo EQ _saida-tipo
                                      AND bezei EQ _saida-bezei
                                      AND tipo_taxa EQ 'V' )
                                   NEXT x = x + ls-total_proporc
                                ).

    _saida-vl59 = obj_tx_curva->get_total_proporcional( ).

    _saida-diferenca = _saida-vl59 - _saida-vl94.

    IF _saida-diferenca IS NOT INITIAL.
      APPEND _saida TO it_saida.
    ENDIF.

  ENDLOOP.

  " 04.04.2025 - 170634 - RAMON -->
  PERFORM f_info_email.
  " 04.04.2025 - 170634 - RAMON --<


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_agrupa_dados_exibicao
*&---------------------------------------------------------------------*
FORM f_agrupa_dados_exibicao .

  DATA(lt_41_aux) = it_41[].

  LOOP AT it_41 ASSIGNING FIELD-SYMBOL(<fs_41>).

    LOOP AT lt_41_aux ASSIGNING FIELD-SYMBOL(<fs_41_aux>)
                              WHERE spart = <fs_41>-spart
                                AND matkl <> <fs_41>-matkl.

      DATA(lv_tabix) = sy-tabix.

      CHECK <fs_41> IS ASSIGNED.

      ADD <fs_41_aux>-vlrtot TO <fs_41>-vlrtot.

      DELETE it_41 INDEX lv_tabix.

    ENDLOOP.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_soma_trava_cambio
*&---------------------------------------------------------------------*
FORM f_soma_trava_cambio USING uv_doc_sim TYPE zsded003
                               uv_vbeln TYPE vbeln
                               uv_spart TYPE spart
                      CHANGING cv_brgew TYPE brgew_ap
                               cv_netwr TYPE netwr_ap
                               cv_mwsbp TYPE mwsbp.

  LOOP AT it_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>) WHERE doc_sim = uv_doc_sim AND vbeln_o = uv_vbeln.

    LOOP AT it_vbap ASSIGNING FIELD-SYMBOL(<fs_vbap>)
      WHERE vbeln = <fs_vbak>-vbeln
        AND spart = uv_spart.

      IF <fs_vbap>-lifsp <> '12'.

        ADD <fs_vbap>-brgew TO cv_brgew.
        ADD <fs_vbap>-netwr TO cv_netwr.
        ADD <fs_vbap>-mwsbp TO cv_mwsbp.

        " se for igual a 12, significa que o valor foi movido,
        " então tem que procurar para onde foi
      ELSE.

        " não pode percorrer ele mesmo.....
        CHECK <fs_vbak>-vbeln NE <fs_vbak>-vbeln_o.

        PERFORM f_soma_trava_cambio
          USING uv_doc_sim
                <fs_vbap>-vbeln
                uv_spart
       CHANGING cv_brgew
                cv_netwr
                cv_mwsbp.

      ENDIF.

    ENDLOOP.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM f_screen_modify .

  LOOP AT SCREEN.

    IF screen-name CS 'R_05' OR screen-name CS 'I05'.

      r_05 = abap_false.

      screen-invisible = 1.

      MODIFY SCREEN.

    ENDIF.

    IF screen-name CS 'P_ANT'.

      IF r_02 = abap_true.
        screen-invisible = 0.
      ELSE.

        screen-invisible = 1.
      ENDIF.

      MODIFY SCREEN.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_data_changed
*&---------------------------------------------------------------------*
FORM f_data_changed USING uo_obj TYPE REF TO cl_alv_changed_data_protocol.

  DATA lv_field TYPE c LENGTH 40.

  LOOP AT uo_obj->mt_mod_cells ASSIGNING FIELD-SYMBOL(<fs_mod>).

    READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX <fs_mod>-row_id.

    CHECK sy-subrc EQ 0.

    lv_field = '<FS_SAIDA>-' && <fs_mod>-fieldname.

    ASSIGN (lv_field) TO FIELD-SYMBOL(<fs_field>).

    CHECK sy-subrc EQ 0.

    <fs_field> = <fs_mod>-value.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_gravar_dados
*&---------------------------------------------------------------------*
FORM f_gravar_dados .

  DATA lt_zsdt0373 TYPE TABLE OF zsdt0373.

  grid1->check_changed_data( ).

  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

    APPEND INITIAL LINE TO lt_zsdt0373 ASSIGNING FIELD-SYMBOL(<fs_0373>).

    <fs_0373>-nro_sol_ov = <fs_saida>-nro_sol_ov.
    <fs_0373>-fixacao = <fs_saida>-fixacao.
    <fs_0373>-tp_venda = <fs_saida>-tipo.

    <fs_0373>-sem_email = <fs_saida>-sem_email.

  ENDLOOP.

  CHECK lt_zsdt0373 IS NOT INITIAL.

  MODIFY zsdt0373 FROM TABLE lt_zsdt0373.

  COMMIT WORK AND WAIT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BOTAO_FUNCTION
*&---------------------------------------------------------------------*
FORM f_botao_function.

  DATA lv_btn TYPE smp_dyntxt.

  lv_btn-icon_id = icon_view_table.
  lv_btn-text = 'Cadastro E-mail'.
  lv_btn-icon_text = 'Cadastro E-mail'.

  sscrfields-functxt_01 = lv_btn.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BOTAO_COMMAND
*&---------------------------------------------------------------------*
FORM f_botao_command.

  IF sy-ucomm = 'FC01'.
    CALL TRANSACTION 'ZSDT0349'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_toolbar
*&---------------------------------------------------------------------*
FORM f_toolbar USING uo_object TYPE REF TO cl_alv_event_toolbar_set.


  APPEND INITIAL LINE TO uo_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_toolbar>).

  <fs_toolbar>-text = 'Marcar Linhas'.
  <fs_toolbar>-icon = '@4B@'.
  <fs_toolbar>-function = 'SELECT'.
  <fs_toolbar>-quickinfo = 'Marcar/Desmarcar linhas'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_marcar_linha
*&---------------------------------------------------------------------*
FORM f_marcar_linha USING uv_ucomm TYPE syucomm.

  DATA wa_stable      TYPE lvc_s_stbl VALUE 'XX'.

  grid1->check_changed_data( ).

  CALL METHOD grid1->get_selected_rows
    IMPORTING
      et_row_no = DATA(lt_rows).

  LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<fs_rows>).

    READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>)
      INDEX <fs_rows>-row_id.

    CHECK sy-subrc EQ 0.

    IF <fs_saida>-sem_email IS INITIAL.
      <fs_saida>-sem_email = abap_true.
    ELSE.
      <fs_saida>-sem_email = abap_false.
    ENDIF.

  ENDLOOP.

  CALL METHOD grid1->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_teste_Dev
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_teste_dev .

  CHECK sy-sysid = 'DEV'.

  DO 10 TIMES.
    APPEND INITIAL LINE TO it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

    <fs_saida>-fixacao  = sy-index.
  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_info_email
*&---------------------------------------------------------------------*
FORM f_info_email .

  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

    READ TABLE gt_zsdt0373 ASSIGNING FIELD-SYMBOL(<fs_0373>)
      WITH KEY nro_sol_ov = <fs_saida>-nro_sol_ov
               fixacao = <fs_saida>-fixacao
               tp_venda = <fs_saida>-tipo.

    IF sy-subrc EQ 0.
      <fs_saida>-sem_email = <fs_0373>-sem_email.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_seleciona_dados_email
*&---------------------------------------------------------------------*
FORM f_seleciona_dados_email .

  IF it_0051[] IS NOT INITIAL.

    SELECT * FROM zsdt0373
      APPENDING TABLE gt_zsdt0373
        FOR ALL ENTRIES IN it_0051
          WHERE nro_sol_ov = it_0051-nro_sol_ov.

  ENDIF.

  IF it_0040[] IS NOT INITIAL.
    SELECT * FROM zsdt0373
      APPENDING TABLE gt_zsdt0373
        FOR ALL ENTRIES IN it_0040
    WHERE nro_sol_ov = it_0040-doc_simulacao.

  ENDIF.

  IF it_0094[] IS NOT INITIAL.

    SELECT * FROM zsdt0373
      APPENDING TABLE gt_zsdt0373
        FOR ALL ENTRIES IN it_0094
    WHERE nro_sol_ov = it_0094-nro_sol_ov.

  ENDIF.

  IF it_ekko[] IS NOT INITIAL.

    SELECT * FROM zsdt0373
      APPENDING TABLE gt_zsdt0373
        FOR ALL ENTRIES IN it_ekko
          WHERE nro_sol_ov = it_ekko-ebeln.

  ENDIF.

  SORT gt_zsdt0373 BY nro_sol_ov fixacao tp_venda ASCENDING.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0373 COMPARING nro_sol_ov fixacao tp_venda.

ENDFORM.

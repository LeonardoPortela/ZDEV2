*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Jaime Tassoni                                           &*
*& Data.....: 08.07.2022                                              &*
*& Descrição: Processamento Transferencia FArdos - Trace Cotton       &*
*&--------------------------------------------------------------------&*
*& Projeto  : Algodão                                                 &*
*&--------------------------------------------------------------------&*
REPORT zsdr0152_contingencia MESSAGE-ID zjob.

************************************************************************
* tabelas
************************************************************************
TABLES: zsdt0330, zmmt0008.

************************************************************************
*  parametro ID_CARGA
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECT-OPTIONS: s_werks FOR zmmt0008-werks,
                  s_lgort FOR zmmt0008-lgort,
                  s_charg FOR zmmt0008-charg.
SELECTION-SCREEN END   OF BLOCK b1.

************************************************************************
*& types
************************************************************************
TYPES: BEGIN OF ty_zsdt0330.
         INCLUDE TYPE zsdt0330.
TYPES:   charg TYPE zmmt0008-charg.
TYPES:  lgortr TYPE zmmt0008-lgortr.
TYPES: END OF ty_zsdt0330.

TYPES: BEGIN OF ty_carga,
         id_carga TYPE zsdt0330-id_carga,
         matnr    TYPE zsdt0330-matnr,
         werks    TYPE zsdt0330-werks,
         lgort    TYPE zsdt0330-lgort,
         acharg   TYPE zsdt0330-acharg,
         safra    TYPE zsdt0330-safra,
         cd_sai   TYPE zsdt0330-cd_sai,
         lgortr   TYPE zmmt0008-lgortr.
TYPES: END OF ty_carga.

TYPES: BEGIN OF type_mchb,
         matnr      TYPE mchb-matnr,
         werks      TYPE mchb-werks,
         lgort      TYPE mchb-lgort,
         charg      TYPE mchb-charg,
         clabs      TYPE mchb-clabs,
         cspem      TYPE mchb-cspem,
         maktx      TYPE makt-maktx,
         lgortr     TYPE mchb-lgort,
         chargr     TYPE mchb-charg,
         matnc      TYPE mchb-matnr,
         cd_sai     TYPE zppt0002-cd_sai,
         icon       TYPE char05,
         log        TYPE char40,
         possuiacts TYPE string,
         status     TYPE string,
       END   OF type_mchb,

       BEGIN OF type_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END   OF type_makt,

       BEGIN OF type_msn,
         tp_msn   TYPE bapi_mtype,
         doc_mat  TYPE bapi2017_gm_head_ret-mat_doc,
         ano      TYPE bapi2017_gm_head_ret-doc_year,
         lote     TYPE charg_d,
         messagem TYPE bapi_msg,
       END   OF type_msn.

************************************************************************
*& variaveis globais
************************************************************************
DATA: t_zsdt0330   TYPE TABLE OF ty_zsdt0330,
      t_0330_carga TYPE TABLE OF ty_zsdt0330,
      t_zmmt0008   TYPE TABLE OF zmmt0008,
      w_zmmt0008   TYPE zmmt0008,
      t_carga      TYPE TABLE OF ty_carga,
      t_mchb       TYPE TABLE OF type_mchb,
      t_trans      TYPE TABLE OF type_mchb,
      t_zppt0002   TYPE TABLE OF zppt0002,
      t_makt       TYPE TABLE OF type_makt,
      t_msn        TYPE TABLE OF type_msn,
      w_zsdt0330   TYPE ty_zsdt0330,
      w_0330_carga TYPE ty_zsdt0330,
      w_carga      TYPE ty_carga,
      w_retorno    TYPE zsdt0331,
      t_status     TYPE zde_btcstatus_t,
      l_tabix      TYPE sy-tabix,
      l_mensagem   TYPE string,
      l_erro       TYPE char01,
      l_erro_lock  TYPE c,
      is_block     TYPE char01,
      l_index      TYPE i,
      l_fardos     TYPE i,
      v_trace      TYPE char1 VALUE 0,
      v_letrac     TYPE char1 VALUE 0,
      vg_qtde      TYPE p,
      vg_werk      TYPE werks_d,
      vg_cd_sai    TYPE char20,
      vg_lote      TYPE char04,
      vg_lgort     TYPE lgort_d,
      sl_mchb      TYPE type_mchb,
      r_data       TYPE zde_acts_tracecotton_t,
      r_charg      TYPE RANGE OF zppt0002-charg.

************************************************************************
*&      Start-Of-Selection
************************************************************************
START-OF-SELECTION.

*---------------------------------------------
*-Processamento
*---------------------------------------------
  PERFORM f_processa_dados.

************************************************************************
*&-processamento dos fardos
************************************************************************
FORM f_processa_dados.

*------------------------------------------
*- selecao dos fardos a processar
*------------------------------------------
  SELECT *
    FROM zmmt0008
    INTO CORRESPONDING FIELDS OF TABLE t_zsdt0330
   WHERE werks              IN s_werks
     AND lgort              IN s_lgort
     AND charg              IN s_charg
*    AND mblnr               = abap_off  "JJJJJJJJJJJJJJJJJJ
     AND lgortr              = 'ARMZ'   "JJJJJJJJJJJJJJJJJ
     AND nr_romaneio        <> '0'
     AND dados_contingencia  = abap_true.

  SELECT *
    FROM zmmt0008
    INTO TABLE t_zmmt0008
   WHERE werks              IN s_werks
     AND lgort              IN s_lgort
     AND charg              IN s_charg
*    AND mblnr               = abap_off  "JJJJJJJJJJJJJJ
     AND nr_romaneio        <> '0'
     AND lgortr              = 'ARMZ'   "JJJJJJJJJJJJJJJJJ
     AND dados_contingencia  = abap_true.

  LOOP AT t_zsdt0330  INTO w_zsdt0330.
    w_zsdt0330-id_carga  = '00001'.
    w_zsdt0330-acharg    = w_zsdt0330-charg.
    MODIFY t_zsdt0330 FROM w_zsdt0330 INDEX sy-tabix.
  ENDLOOP.

  CHECK t_zsdt0330[] IS NOT INITIAL.

*------------------------------------------
*- inicio processamrnto
*------------------------------------------
  t_0330_carga[] = t_zsdt0330[].

  SORT t_0330_carga BY id_carga matnr werks lgort safra.
  DELETE ADJACENT DUPLICATES FROM t_0330_carga
                        COMPARING id_carga matnr werks lgort safra.

  LOOP AT t_0330_carga INTO w_0330_carga.

    FREE: t_carga, l_mensagem.

    LOOP AT t_zsdt0330            INTO w_zsdt0330 WHERE id_carga = w_0330_carga-id_carga
                                                    AND matnr    = w_0330_carga-matnr
                                                    AND werks    = w_0330_carga-werks
                                                    AND lgort    = w_0330_carga-lgort
                                                    AND safra    = w_0330_carga-safra.
      MOVE-CORRESPONDING w_zsdt0330 TO w_carga.
      APPEND w_carga                TO t_carga.
    ENDLOOP.

    PERFORM f_selecao_dados CHANGING l_erro
                                     l_mensagem.

    IF l_erro = abap_true.
      MESSAGE i024(sd) WITH l_mensagem.
      CONTINUE.
    ENDIF.

    PERFORM f_monta_mchb.
    PERFORM f_gerar_transferencia.

  ENDLOOP.

  MESSAGE s024(sd) WITH 'Processo finalizado!' ' Verifique tabele ZMMT0008 ' 'com documentos gerados.'.

ENDFORM.

************************************************************************
*&-selecao dados
************************************************************************
FORM f_selecao_dados CHANGING p_erro
                              p_mensagem.

  FREE: p_erro, p_mensagem.

  PERFORM: f_seleciona_mchb CHANGING p_erro
                                     p_mensagem,
           f_seleciona_makt.

ENDFORM.

************************************************************************
*&-selecao mchb
************************************************************************
FORM f_seleciona_mchb CHANGING p_erro
                               p_mensagem.

  FREE: t_mchb, r_charg, p_erro, p_mensagem.

  READ TABLE t_carga INTO w_carga INDEX 1.

  SELECT matnr werks lgort
         charg clabs cspem
    FROM mchb
    INTO TABLE t_mchb
     FOR ALL ENTRIES IN t_carga
   WHERE matnr  = t_carga-matnr
     AND werks  = t_carga-werks
     AND lgort  = t_carga-lgort
     AND charg  = t_carga-acharg.

*-------------------------------------------
* validar clabs
*-------------------------------------------
  DELETE t_mchb WHERE clabs = 0.
  SORT t_mchb BY matnr ASCENDING
                 werks ASCENDING
                 lgort ASCENDING
                 charg ASCENDING.

  IF t_mchb[] IS INITIAL.
    p_erro               = abap_true.
    p_mensagem           = 'Nenhum Estoque de Lote Encontrado.'.
    MESSAGE s836 WITH TEXT-016 DISPLAY LIKE 'I'.
    EXIT.
  ENDIF.

*-------------------------------------------
* busca deposito destino
*-------------------------------------------
  sl_mchb-lgortr = w_carga-lgortr.
  sl_mchb-chargr = w_carga-safra.
  sl_mchb-matnc  = abap_off.

  MODIFY t_mchb FROM sl_mchb TRANSPORTING lgortr chargr matnc WHERE lgortr = space.

ENDFORM.

************************************************************************
*&-selecao makt
************************************************************************
FORM f_seleciona_makt.

  DATA: tl_mchb TYPE TABLE OF type_mchb.

  REFRESH t_makt.

  CHECK NOT t_mchb[] IS INITIAL.
  tl_mchb[] = t_mchb[].

  SORT tl_mchb BY matnr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_mchb COMPARING matnr.

  SELECT matnr maktx
    FROM makt
    INTO TABLE t_makt
     FOR ALL ENTRIES IN tl_mchb
   WHERE matnr EQ tl_mchb-matnr
     AND spras EQ sy-langu.

  SORT t_makt BY matnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_MAKT

************************************************************************
*&-monta mchb
************************************************************************
FORM f_monta_mchb.

  DATA: sl_makt  TYPE type_makt,
        sl_mchb  TYPE type_mchb,
        vl_index TYPE sy-tabix.

  LOOP AT t_mchb INTO sl_mchb.
    vl_index = sy-tabix.

    READ TABLE t_makt INTO sl_makt WITH KEY matnr = sl_mchb-matnr
                                   BINARY SEARCH.
    sl_mchb-maktx = sl_makt-maktx.

    MODIFY t_mchb FROM sl_mchb INDEX vl_index TRANSPORTING maktx.

    CLEAR: sl_makt,
           sl_mchb.
  ENDLOOP.

ENDFORM.                    " Z_PROCESSA_DADOS

************************************************************************
*&-ajusta zsdt0331
************************************************************************
FORM f_ajusta_taberros.

  LOOP AT t_carga  INTO w_carga.
    UPDATE zsdt0331 SET cancelado    = abap_true
                  WHERE id_carga     = w_carga-id_carga
                    AND matnr        = w_carga-matnr
                    AND werks        = w_carga-werks
                    AND lgort        = w_carga-lgort
                    AND cd_sai       = w_carga-cd_sai
                    AND safra        = w_carga-safra.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.

************************************************************************
*&-gerar erro para o fardo
************************************************************************
FORM f_gerar_transferencia.

  DATA: sl_trans    TYPE type_mchb,
        sl_header   TYPE bapi2017_gm_head_01,
        vl_code     TYPE bapi2017_gm_code,
        vl_material TYPE bapi2017_gm_head_ret-mat_doc,
        vl_year     TYPE bapi2017_gm_head_ret-doc_year,
        tl_item     TYPE TABLE OF bapi2017_gm_item_create,
        tl_return   TYPE TABLE OF bapiret2,
        sl_return   TYPE bapiret2,
        sl_item     TYPE bapi2017_gm_item_create,
        r_data      TYPE zde_acts_tracecotton_t,
        i_date      TYPE bapi2017_gm_head_ret-doc_year,
        i_doc_mat   TYPE bapi2017_gm_head_ret-mat_doc,
        sl_zmmt0008 TYPE zmmt0008.

  FREE: t_trans, t_msn, sl_header.

  LOOP AT t_mchb  INTO sl_trans.
    APPEND sl_trans TO t_trans.
  ENDLOOP.

  SORT t_trans BY matnr ASCENDING
                  werks ASCENDING
                  lgort ASCENDING
                  charg ASCENDING.

  READ TABLE t_trans INTO DATA(ws_trans) WITH KEY icon = icon_led_red.
  IF sy-subrc = 0.
    l_mensagem = 'Existem lotes com status diferente de aprovado ou não possui ACTS, verificar!'.
    PERFORM f_gera_erro_carga    USING '2' l_mensagem.
    EXIT.
  ENDIF.

  READ TABLE t_carga INTO w_carga INDEX 1.

*---------------------------------------
* efetua movto mercadoria
*---------------------------------------
  LOOP AT t_trans INTO sl_trans.

    FREE: vl_material, vl_year, sl_zmmt0008, tl_item, tl_return.

    IF sy-tabix EQ 1.
      vl_code              = '06'.
      sl_header-pstng_date = sy-datum.
      sl_header-doc_date   = sy-datum.
    ENDIF.

    sl_item-move_type      = 'ZA1'.
    sl_item-material       = sl_trans-matnr.
    sl_item-plant          = sl_trans-werks.
    sl_item-stge_loc       = sl_trans-lgort.
    sl_item-batch          = sl_trans-charg.
    sl_item-entry_qnt      = sl_trans-clabs.
    sl_item-move_plant     = sl_trans-werks.
    sl_item-move_stloc     = 'ARMZ'.   "sl_trans-lgortr. JJJJJJJJJJJJJJJ
    sl_item-move_mat       = sl_trans-matnc.
*   sl_item-customer       = p_kunnr.
*   sl_header-header_txt   = p_sgtxt.

    IF NOT sl_trans-chargr IS INITIAL.
      sl_item-move_batch   = sl_trans-chargr.
    ELSE.
      sl_item-move_batch   = sl_trans-charg.
    ENDIF.

    APPEND sl_item TO tl_item.

*-----------------------------------------
*-- executa bapi, verificando bloqueio de fardo
*-----------------------------------------
    DO 5 TIMES.
      FREE: tl_return, l_erro_lock, is_block.

      l_index = sy-index.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header  = sl_header
          goodsmvt_code    = vl_code
        IMPORTING
          materialdocument = vl_material
          matdocumentyear  = vl_year
        TABLES
          goodsmvt_item    = tl_item
          return           = tl_return.

      READ TABLE tl_return INTO sl_return WITH KEY type = 'E'.

      IF sy-subrc = 0.
        CALL FUNCTION 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
          EXPORTING
            id       = sl_return-id
            number   = sl_return-number
          IMPORTING
            is_block = is_block.

        l_erro_lock = is_block.

        IF is_block IS NOT INITIAL AND l_index NE 5.
          WAIT UP TO 3 SECONDS.
        ELSE.
          EXIT.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    IF l_erro_lock = abap_true.
*      PERFORM f_monta_erro TABLES tl_return
*                            USING sl_trans-matnr
*                                  sl_trans-werks
*                                  sl_trans-lgort
*                                  sl_trans-lgortr
*                                  sl_trans-charg
*                                  vl_material
*                                  vl_year
*                                  '0'.
      CONTINUE.
    ENDIF.

    IF NOT vl_material IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      FREE tl_return.

      sl_return-type     = 'S'.
      CONCATENATE TEXT-013
                  vl_material
                  TEXT-014
                  vl_year
                  TEXT-015
             INTO sl_return-message SEPARATED BY space.
      APPEND sl_return TO tl_return.

*      PERFORM f_monta_erro TABLES tl_return
*                            USING sl_trans-matnr
*                                  sl_trans-werks
*                                  sl_trans-lgort
*                                  sl_trans-lgortr
*                                  sl_trans-charg
*                                  vl_material
*                                  vl_year
*                                  '3'.

      IF sl_item-move_type EQ 'ZA1'.
        READ TABLE t_zmmt0008 INTO sl_zmmt0008 WITH KEY werks = sl_trans-werks
                                                        lgort = sl_trans-lgort
                                                        charg = sl_trans-charg.
        sl_zmmt0008-werks  = sl_trans-werks.
        sl_zmmt0008-lgort  = sl_trans-lgort.
        sl_zmmt0008-charg  = sl_trans-charg.
        sl_zmmt0008-menge  = sl_trans-clabs.
*       sl_zmmt0008-matnr  = sl_trans-matnr.    "*-S4H-US 123905-25.09.2023-JT
*       sl_zmmt0008-lgortr = sl_trans-lgortr.   "*-S4H-US 123905-25.09.2023-JT
*       sl_zmmt0008-safra  = sl_trans-chargr.   "*-S4H-US 123905-25.09.2023-JT
        sl_zmmt0008-mblnr  = vl_material.       "*-S4H-US 123905-25.09.2023-JT
        sl_zmmt0008-mjahr  = vl_year.           "*-S4H-US 123905-25.09.2023-JT

        MODIFY zmmt0008 FROM sl_zmmt0008.
      ENDIF.

      DELETE t_trans WHERE matnr EQ sl_trans-matnr
                       AND werks EQ sl_trans-werks
                       AND lgort EQ sl_trans-lgort
                       AND charg EQ sl_trans-charg.
      DELETE t_mchb  WHERE matnr EQ sl_trans-matnr
                       AND werks EQ sl_trans-werks
                       AND lgort EQ sl_trans-lgort
                       AND charg EQ sl_trans-charg.

    ELSE.
*     Retorna Erro
*      PERFORM f_monta_erro TABLES tl_return
*                            USING sl_trans-matnr
*                                  sl_trans-werks
*                                  sl_trans-lgort
*                                  sl_trans-lgortr
*                                  sl_trans-charg
*                                  vl_material
*                                  vl_year
*                                  '2'.
    ENDIF.

    CLEAR: sl_trans,
           sl_item.

  ENDLOOP.

ENDFORM.

************************************************************************
*&-monta saida de erro
************************************************************************
*FORM f_monta_erro TABLES p_return  STRUCTURE bapiret2
*                   USING p_matnr
*                         p_werks
*                         p_lgort
*                         p_lgort_rec
*                         p_charg
*                         p_mat_doc TYPE bapi2017_gm_head_ret-mat_doc
*                         p_year    TYPE bapi2017_gm_head_ret-doc_year
*                         p_status.
*
*  DATA: sl_return TYPE bapiret2,
*        sl_msn    TYPE type_msn.
*
*  LOOP AT p_return INTO sl_return.
*    l_mensagem  = sl_return-message.
*
*    zcl_trace_cotton=>zif_trace_cotton~get_instance(
*       )->set_gravar_log( EXPORTING i_id_carga  = w_carga-id_carga
*                                    i_matnr     = p_matnr
*                                    i_werks     = p_werks
*                                    i_lgort     = p_lgort
*                                    i_acharg    = p_charg
*                                    i_safra     = w_carga-safra
*                                    i_tipo_msg  = COND #( WHEN p_mat_doc IS INITIAL THEN 'E'
*                                                                                    ELSE 'S' )
*                                    i_mensagem  = l_mensagem ).
*  ENDLOOP.
*
*  IF p_mat_doc IS NOT INITIAL.
*    UPDATE zsdt0330 SET mblnr        = p_mat_doc
*                        mjahr        = p_year
*                        lgort_rec    = p_lgort_rec
*                        status_fardo = '3'
*                  WHERE id_carga     = w_carga-id_carga
*                    AND matnr        = p_matnr
*                    AND werks        = p_werks
*                    AND lgort        = p_lgort
*                    AND acharg       = p_charg
*                    AND safra        = w_carga-safra
*                    AND cancelado    = abap_off.
*  ELSE.
*    UPDATE zsdt0330 SET status_fardo = p_status
*                        lgort_rec    = p_lgort_rec
*                  WHERE id_carga     = w_carga-id_carga
*                    AND matnr        = p_matnr
*                    AND werks        = p_werks
*                    AND lgort        = p_lgort
*                    AND acharg       = p_charg
*                    AND safra        = w_carga-safra
*                    AND cancelado    = abap_off.
*  ENDIF.
*
*  COMMIT WORK AND WAIT.
*
*ENDFORM.                    " Z_MONTA_ERRO

************************************************************************
*&-gerar erro para o fardo
************************************************************************
FORM f_gera_erro_carga USING p_status
                             p_mensagem.

  LOOP AT t_carga INTO w_carga.
    zcl_trace_cotton=>zif_trace_cotton~get_instance(
       )->set_gravar_log( EXPORTING i_id_carga  = w_carga-id_carga
                                    i_matnr     = w_carga-matnr
                                    i_werks     = w_carga-werks
                                    i_lgort     = w_carga-lgort
                                    i_acharg    = w_carga-acharg
                                    i_safra     = w_carga-safra
                                    i_tipo_msg  = 'E'
                                    i_mensagem  = p_mensagem ).

    UPDATE zsdt0330 SET status_fardo = p_status
                  WHERE id_carga     = w_carga-id_carga
                    AND matnr        = w_carga-matnr
                    AND werks        = w_carga-werks
                    AND lgort        = w_carga-lgort
                    AND acharg       = w_carga-acharg
                    AND safra        = w_carga-safra
                    AND cancelado    = abap_false.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.

************************************************************************
*&-enviar erro trace cotton
************************************************************************
FORM f_envia_trace_cotton USING p_mensagem.

  LOOP AT  t_trans INTO DATA(w_trans) WHERE icon = icon_led_red.

    CLEAR w_retorno.

    READ TABLE t_carga INTO DATA(_carga) WITH KEY matnr  = w_trans-matnr
                                                  werks  = w_trans-werks
                                                  lgort  = w_trans-lgort
                                                  acharg = w_trans-charg.
    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING _carga      TO w_retorno.
    MOVE                p_mensagem TO w_retorno-mensagem.

    zcl_trace_cotton=>zif_trace_cotton~get_instance(
       )->set_gravar_retorno( w_retorno ).

  ENDLOOP.

  COMMIT WORK.

*--------------------------------------
*-- envio de retorno de erros ao trace
*--------------------------------------
  TRY .
      zcl_trace_cotton=>zif_trace_cotton~get_instance(
         )->set_retorno_trace( i_id_carga = w_carga-id_carga ).

    CATCH zcx_integracao INTO DATA(ex_integra).
    CATCH zcx_error INTO DATA(ex_error).
  ENDTRY.

  COMMIT WORK.

ENDFORM.

************************************************************************
************************************************************************

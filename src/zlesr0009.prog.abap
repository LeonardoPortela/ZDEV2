*$*$ -------------------------------------------------------------- *$*$
*$*$                    GRUPO ANDRÉ MAGGI                           *$*$
*$*$ -------------------------------------------------------------- *$*$
*$*$ Autor     : Robson Motta - BBKO Consulting                     *$*$
*$*$ Data      : 31/08/2009                                         *$*$
*$*$ Descrição : Atualização de lançamento contábil                 *$*$
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

* NOTA: Lógica copiada do cokpit - Automação Posto Gasolina

REPORT  zlesr0009.

***********************************************************************
* TABELAS TRANSPARENTES                                               *
***********************************************************************
TABLES: zib_contabil_chv,
        zib_contabil,
        zlest0008,
        zlest0013,
        zlest0015,
        zlest0016,
        zlest0020,
        zlest0022.

***********************************************************************
* CONSTANTES                                                          *
***********************************************************************
CONSTANTS: cc_e                         VALUE 'E',
           cc_s                         VALUE 'S',
           cc_i                         VALUE 'I',
           cc_eq(2)            TYPE c   VALUE 'EQ',
           cc_a_confirmar               VALUE 'A',
           cc_confirmado                VALUE 'C',
           cc_importado                 VALUE 'I'.

***********************************************************************
* DECLARAÇÃO TIPOS                                                    *
***********************************************************************
TYPES: BEGIN OF ty_obj_contabil,
          codtrp              TYPE zcodtrp,
          codposto            TYPE zcodposto,
          lote                TYPE char10,
          chvid               TYPE zchvid,
          tiptransp           TYPE shtyp,
          ctlglancto          TYPE zctlglancto,
          valor               TYPE kwert,
          obj_key             TYPE awkey,
          docsap              TYPE belnr_d,
          gjahr               TYPE gjahr,
          bukrs               TYPE bukrs,
          data_confer         TYPE dats,
          observ              TYPE reorxcomment,
       END   OF ty_obj_contabil.

***********************************************************************
* DECLARAÇÃO DE TABELAS INTERNAS                                      *
***********************************************************************
DATA: ti_obj_contabil         TYPE STANDARD TABLE OF ty_obj_contabil
                                   WITH HEADER LINE INITIAL SIZE 0,
      ti_zlest0016            TYPE STANDARD TABLE OF zlest0016
                                   WITH HEADER LINE INITIAL SIZE 0.

DATA: ti_cockpit_lote         TYPE zles_cockpit_lote_t,
      ti_cockpit_lancto       TYPE zles_cockpit_lancto_t,
      ti_cockpit_deltas       TYPE zles_cockpit_delta_t.

***********************************************************************
* DECLARAÇÃO DE VARIÁVEIS                                             *
***********************************************************************
DATA: l_waindx                TYPE indx.

DATA: v_wa_lote               TYPE zles_cockpit_lote,
      v_wa_lancto             TYPE zles_cockpit_lancto,
      v_wa_deltas             TYPE zles_cockpit_delta.

DATA: vc_codtrp               TYPE zcodtrp,
      vc_codposto             TYPE zcodposto,
      vc_lote                 TYPE char10,
      v_msgerro               TYPE bapi_msg.

***********************************************************************
* Parâmetro de seleção - Recebido via background (Submit JOB)         *
***********************************************************************
PARAMETER p_keyid LIKE indx-srtfd NO-DISPLAY.




***********************************************************************
* START-OF-SELECTION                                                  *
***********************************************************************
START-OF-SELECTION.

* Verifica chave de acesso aos parâmetros de entrada
  CHECK: NOT p_keyid IS INITIAL.

* Obtem valores de parâmetros de importação do JOB
  IMPORT ti_obj_contabil = ti_obj_contabil
    FROM DATABASE indx(st) ID p_keyid
      TO l_waindx.

  CHECK: sy-subrc IS INITIAL.
  DELETE FROM DATABASE indx(st) ID p_keyid.

  CLEAR zib_contabil.

* Analisa documento contábil
  LOOP AT ti_obj_contabil.

*   Não deve ocorrer o Wait para recuperar o documento
*   Pois o tempo do job schedulado deve ter sido suficiente
    DO 8 TIMES.
      SELECT *
        FROM zib_contabil
          UP TO 1 ROWS
       WHERE obj_key = ti_obj_contabil-obj_key
         AND rg_atualizado = cc_s.
      ENDSELECT.

      IF sy-subrc IS INITIAL.
        EXIT.
      ENDIF.
      WAIT UP TO 2 SECONDS.
    ENDDO.

    CHECK: zib_contabil-rg_atualizado = cc_s.

*   Lê o documento contábil gerado
    CLEAR zib_contabil_chv.

    SELECT SINGLE *
      FROM zib_contabil_chv
     WHERE obj_key = ti_obj_contabil-obj_key.

*   Analisa LOG de Erro na geração do documento contábil
    IF zib_contabil_chv-belnr IS INITIAL.

*     Gera Log cockpit
      PERFORM gera_loglacto_cockpit.

*     Elimina documento contábil
      DELETE FROM zib_contabil
            WHERE obj_key = zib_contabil-obj_key.

*     Analisa o próximo registro
      CONTINUE.
    ENDIF.

*   Atualiza documento gerados
    IF ti_obj_contabil-tiptransp   IS INITIAL OR
       ti_obj_contabil-ctlglancto  IS INITIAL.
*     Lançamento - ABA: Lotes
      PERFORM lancto_grava_interface_posto.
    ELSE.
*    Lançamento - ABA: Conferência
      PERFORM lancto_grava_conferencia.
    ENDIF.

  ENDLOOP.

*&---------------------------------------------------------------------*
*&      Form  LANCTO_GRAVA_INTERFACE_POSTO
*&---------------------------------------------------------------------*
FORM lancto_grava_interface_posto .

* Atualiza pela informações principais do lote
  CHECK: vc_codtrp   <> ti_obj_contabil-codtrp   OR
         vc_codposto <> ti_obj_contabil-codposto OR
         vc_lote     <> ti_obj_contabil-lote.

  vc_codtrp   = ti_obj_contabil-codtrp.
  vc_codposto = ti_obj_contabil-codposto.
  vc_lote     = ti_obj_contabil-lote.

  SELECT *
    FROM zlest0013
      UP TO 1 ROWS
   WHERE codtrp     = ti_obj_contabil-codtrp
     AND codposto   = ti_obj_contabil-codposto
     AND lote       = ti_obj_contabil-lote
     AND status     = cc_importado.
  ENDSELECT.

  CHECK: sy-subrc IS INITIAL.

  PERFORM obtem_docto_posto_gasolina.

  PERFORM gera_docto_dtlh_zlest0016.

  PERFORM gera_docto_hder_zlest0015.

  PERFORM atlz_docto_posto_gasolina.

ENDFORM.                    " LANCTO_GRAVA_INTERFACE_POSTO

*&---------------------------------------------------------------------*
*&      Form  OBTEM_DOCTO_POSTO_GASOLINA
*&---------------------------------------------------------------------*
FORM obtem_docto_posto_gasolina .

  DATA: rt_transp    TYPE lxhme_range_c10_t,
        rt_posto     TYPE lxhme_range_c10_t,
        rt_lote      TYPE lxhme_range_c10_t,
        rt_status    TYPE lxhme_range_c1_t.

  DATA: lw_range_c10 TYPE lxhme_range_c10,
        lw_range_c1  TYPE lxhme_range_c1.

* Transportadora
  lw_range_c10-sign   = cc_i.
  lw_range_c10-option = cc_eq.
  lw_range_c10-low    = zlest0013-codtrp.
  APPEND lw_range_c10 TO rt_transp.

* Posto
  lw_range_c10-low    = zlest0013-codposto.
  APPEND lw_range_c10 TO rt_posto.

* Lote
  lw_range_c10-low    = zlest0013-lote.
  APPEND lw_range_c10 TO rt_lote.

* Status
  lw_range_c10-low    = zlest0013-status.
  APPEND lw_range_c10 TO rt_status.

* Busca conhecimento
  CALL FUNCTION 'Z_LES_COCKPIT_AUTOMACAO_POSTO'
    EXPORTING
      rt_trasnportador = rt_transp[]
      rt_posto         = rt_posto[]
      rt_lote          = rt_lote[]
*     rt_conhecimento  =
*     rt_carta_frete   =
*     rt_periodo       =
      rt_status        = rt_status[]
    IMPORTING
      e_msgerr         = v_msgerro
    TABLES
      t_lotes          = ti_cockpit_lote
      t_lanctos        = ti_cockpit_lancto
      t_deltas         = ti_cockpit_deltas.

ENDFORM.                    " OBTEM_DOCTO_POSTO_GASOLINA

*&---------------------------------------------------------------------*
*&      Form  GERA_DOCTO_DTLH_ZLEST0016
*&---------------------------------------------------------------------*
FORM gera_docto_dtlh_zlest0016 .

  DATA: l_index  TYPE i.

* Posto - Lançamentos - Detalhes
  READ TABLE ti_cockpit_lancto INTO v_wa_lancto
                           WITH KEY codtrp   = zlest0013-codtrp
                                    codposto = zlest0013-codposto
                                    lote     = zlest0013-lote
  BINARY SEARCH.
  CHECK: sy-subrc IS INITIAL.

  l_index = sy-tabix.
  DO.
    zlest0016-transportador   = v_wa_lancto-codtrp.
    zlest0016-posto           = v_wa_lancto-codposto.
    zlest0016-lote            = v_wa_lancto-lote.
    zlest0016-chvid           = v_wa_lancto-chvid.
    zlest0016-ctafrete        = v_wa_lancto-ctafrete.
    zlest0016-conhecimento    = v_wa_lancto-conhec.
    zlest0016-peso_origem     = v_wa_lancto-peso_origem.
    zlest0016-peso_importado  = v_wa_lancto-peso_importado.
    zlest0016-peso_confirmado = v_wa_lancto-peso_confirmado.
    zlest0016-unidade_peso    = v_wa_lancto-unid_peso.
    zlest0016-vlr_origem      = v_wa_lancto-vlrorigem.
    zlest0016-vlr_importado   = v_wa_lancto-vlrimportado.
    zlest0016-vlr_confirmado  = v_wa_lancto-vlrconfirmado.
    zlest0016-diferenca       = v_wa_lancto-vlrdiferenca.
    zlest0016-vlr_programado  = v_wa_lancto-vlrprogramado.
    zlest0016-dta_chegada     = v_wa_lancto-dtacheg.
    zlest0016-erdat           = sy-datum.
    zlest0016-uzeit           = sy-uzeit.
    zlest0016-uname           = sy-uname.

    MODIFY zlest0016 FROM zlest0016.
    CLEAR zlest0016.

    READ TABLE ti_cockpit_deltas INTO v_wa_deltas
                             WITH KEY codtrp = v_wa_lancto-codtrp
                                    codposto = v_wa_lancto-codposto
                                        lote = v_wa_lancto-lote
                                    datalote = v_wa_lancto-datalote
                                       chvid = v_wa_lancto-chvid
                                      conhec = v_wa_lancto-conhec
                                    ctafrete = v_wa_lancto-ctafrete
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.

*     Dados de calculo - Deltas
      MOVE-CORRESPONDING v_wa_deltas  TO zlest0020.
      MOVE: v_wa_lancto-codtrp        TO zlest0020-transportador,
            v_wa_lancto-codposto      TO zlest0020-posto,
            sy-datum                  TO zlest0020-erdat,
            sy-uname                  TO zlest0020-uname.

      MODIFY zlest0020 FROM zlest0020.
      CLEAR zlest0020.

    ENDIF.

*   Lê o próximo registro
    ADD 1 TO l_index.
    READ TABLE ti_cockpit_lancto INTO v_wa_lancto INDEX l_index.
    IF sy-subrc           <> 0                    OR
       zlest0013-codtrp   <> v_wa_lancto-codtrp   OR
       zlest0013-codposto <> v_wa_lancto-codposto OR
       zlest0013-lote     <> v_wa_lancto-lote.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " GERA_DOCTO_DTLH_ZLEST0016

*&---------------------------------------------------------------------*
*&      Form  GERA_DOCTO_HDER_ZLEST0015
*&---------------------------------------------------------------------*
FORM gera_docto_hder_zlest0015 .

* Posto - Lançamentos - Detalhes
  READ TABLE ti_cockpit_lote INTO v_wa_lote
                         WITH KEY codtrp   = zlest0013-codtrp
                                  codposto = zlest0013-codposto
                                  lote     = zlest0013-lote
  BINARY SEARCH.
  CHECK: sy-subrc IS INITIAL.

* Cabeçalho- Posto - Lotes
  zlest0015-transportador     = v_wa_lote-codtrp.
  zlest0015-posto             = v_wa_lote-codposto.
  zlest0015-lote              = v_wa_lote-lote.
  zlest0015-bl                = v_wa_lote-bl.
  zlest0015-data              = v_wa_lote-datalote.
  zlest0015-vencimento        = v_wa_lote-vencimento.
  zlest0015-vlr_origem        = v_wa_lote-vlrorigem.
  zlest0015-vlr_importado     = v_wa_lote-vlrimportado.
  zlest0015-vlr_confirmado    = v_wa_lote-vlrconfirmado.
  zlest0015-vlr_recusado      = v_wa_lote-vlrrecusado.
  zlest0015-vlr_realizado     = v_wa_lote-vlrrealizado.
  zlest0015-diferenca         = v_wa_lote-vlrdiferenca.
  zlest0015-status            = cc_a_confirmar.
  zlest0015-erdat             = sy-datum.
  zlest0015-uzeit             = sy-uzeit.
  zlest0015-uname             = sy-uname.

  zlest0015-docsap            = zib_contabil_chv-belnr.
  zlest0015-gjahr             = zib_contabil_chv-gjahr.
  zlest0015-bukrs             = ti_obj_contabil-bukrs.

  MODIFY zlest0015 FROM zlest0015.

ENDFORM.                    " GERA_DOCTO_HDER_ZLEST0015

*&---------------------------------------------------------------------*
*&      Form  ATLZ_DOCTO_POSTO_GASOLINA
*&---------------------------------------------------------------------*
FORM atlz_docto_posto_gasolina .

  UPDATE zlest0013
     SET status  = cc_confirmado
         data    = sy-datum
         hora    = sy-uzeit
         usuario = sy-uname
   WHERE codtrp     = zlest0013-codtrp
     AND codposto   = zlest0013-codposto
     AND lote       = zlest0013-lote.

ENDFORM.                    " ATLZ_DOCTO_POSTO_GASOLINA

*&---------------------------------------------------------------------*
*&      Form  GERA_LOGLACTO_COCKPIT
*&---------------------------------------------------------------------*
FORM gera_loglacto_cockpit .

  DATA: lt_log_contabil  TYPE STANDARD TABLE OF zib_contabil_log
                              WITH HEADER LINE INITIAL SIZE 0.

  DATA: lc_chave         TYPE epsfilnam,
        ln_idctrl        TYPE zidctrl,
        ln_vcont         TYPE numc10,
        ld_data_contabil TYPE d.

* Gera data para pesquisa no log (Um dia antes da data atual)
  PERFORM yf_calc_date USING sy-datum '01' '00' '00' '-'
                    CHANGING ld_data_contabil.

* Obtem registros de log contábil
  SELECT *
    INTO TABLE lt_log_contabil
    FROM zib_contabil_log
   WHERE obj_key = ti_obj_contabil-obj_key
      AND dtreg >= ld_data_contabil.

* Gera controle para última chave de erro no log
  lc_chave = sy-repid.

  SELECT MAX( idctrl ) MAX( cont )
    INTO (ln_idctrl, ln_vcont)
    FROM zlest0008
   WHERE filename = lc_chave
    GROUP BY idctrl.
  ENDSELECT.

* Gera sequencia de controle de erro
  IF sy-subrc IS INITIAL.
    IF ln_vcont >= '9999999000'.
      ADD 1 TO ln_idctrl.
      CLEAR ln_vcont.
    ENDIF.
  ELSE.
    ADD 1 TO ln_idctrl.
  ENDIF.

* Transfere o erro para area de LOG
  CLEAR zlest0008.

  zlest0008-filename = lc_chave.
  zlest0008-idctrl   = ln_idctrl.
  zlest0008-tcode    = sy-tcode.
  zlest0008-cont     = ln_vcont.
  zlest0008-msgtyp   = cc_e.
  zlest0008-msgspra  = sy-langu.
  zlest0008-msgid    = 'FR'.
  zlest0008-msgnr    = '999'.
  zlest0008-data     = sy-datum.
  zlest0008-hora     = sy-uzeit.
  zlest0008-usuario  = sy-uname.
  zlest0008-lote     = ti_obj_contabil-lote.

  IF lt_log_contabil[] IS INITIAL.
    ADD 1 TO ln_vcont.
    zlest0008-cont  = ln_vcont.
    zlest0008-msgv1 = text-m01.
    INSERT zlest0008.
  ELSE.
    LOOP AT lt_log_contabil.
      ADD 1 TO ln_vcont.
      zlest0008-cont  = ln_vcont.
      CONCATENATE text-m02
                  'key =' lt_log_contabil-obj_key
                  '/'     lt_log_contabil-seqitem
                  '-'     lt_log_contabil-bktxt
             INTO zlest0008-msgv1 SEPARATED BY space.
      INSERT zlest0008.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " GERA_LOGLACTO_COCKPIT

*&---------------------------------------------------------------------*
*&      Form  YF_CALC_DATE
*&---------------------------------------------------------------------*
FORM yf_calc_date     USING p_data_inicio
                            p_dia
                            p_mes
                            p_ano
                            p_sinal
                   CHANGING p_data_calculada.

  DATA: ln_dia   TYPE dlydy,
        ln_mes   TYPE dlymo,
        ln_ano   TYPE dlyyr,
        lc_sinal TYPE spli1.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = p_data_inicio
      days      = ln_dia
      months    = ln_mes
      signum    = lc_sinal
      years     = ln_ano
    IMPORTING
      calc_date = p_data_calculada.

ENDFORM.                    " YF_CALC_DATE

*&---------------------------------------------------------------------*
*&      Form  LANCTO_GRAVA_CONFERENCIA
*&---------------------------------------------------------------------*
FORM lancto_grava_conferencia .

  DATA: lc_qbr_codtrp    TYPE tdlnr,
        lc_qbr_codposto  TYPE lifnr,
        lc_qbr_lote      TYPE char10,
        lc_qbr_chvid     TYPE zchvid.

  SORT ti_obj_contabil BY codtrp codposto lote chvid
                          tiptransp ctlglancto.

  LOOP AT ti_obj_contabil.

    IF ti_obj_contabil-codtrp    <> lc_qbr_codtrp   OR
       ti_obj_contabil-codposto  <> lc_qbr_codposto OR
       ti_obj_contabil-lote      <> lc_qbr_lote     OR
       ti_obj_contabil-chvid     <> lc_qbr_chvid.

      lc_qbr_codtrp   = ti_obj_contabil-codtrp.
      lc_qbr_codposto = ti_obj_contabil-codposto.
      lc_qbr_lote     = ti_obj_contabil-lote.
      lc_qbr_chvid    = ti_obj_contabil-chvid.

      SELECT *
        INTO TABLE ti_zlest0016
        FROM zlest0016
      WHERE transportador = ti_obj_contabil-codtrp
        AND posto         = ti_obj_contabil-codposto
        AND lote          = ti_obj_contabil-lote
      ORDER BY PRIMARY KEY.

      READ TABLE ti_zlest0016
        WITH KEY transportador = ti_obj_contabil-codtrp
                 posto         = ti_obj_contabil-codposto
                 lote          = ti_obj_contabil-lote
                 chvid         = ti_obj_contabil-chvid
      BINARY SEARCH.

      IF NOT sy-subrc IS INITIAL.

*       Extrai a carta frete e conhecimento do lote
        READ TABLE ti_zlest0016
          WITH KEY transportador = ti_obj_contabil-codtrp
                   posto         = ti_obj_contabil-codposto
                   lote          = ti_obj_contabil-lote
        BINARY SEARCH.
        CHECK sy-subrc IS INITIAL.

*       Gera nova entrada para lançamento manual
        CLEAR zlest0016.
        zlest0016-transportador   = ti_obj_contabil-codtrp.
        zlest0016-posto           = ti_obj_contabil-codposto.
        zlest0016-lote            = ti_obj_contabil-lote.
        zlest0016-chvid           = ti_obj_contabil-chvid.
        zlest0016-ctafrete        = ti_zlest0016-ctafrete.
        zlest0016-conhecimento    = ti_zlest0016-conhecimento.
        zlest0016-vlr_confirmado  = ti_obj_contabil-valor.
        zlest0016-dta_chegada     = ti_obj_contabil-data_confer.
        zlest0016-erdat           = sy-datum.
        zlest0016-uzeit           = sy-uzeit.
        zlest0016-uname           = sy-uname.
        MODIFY zlest0016 FROM zlest0016.

      ENDIF.
    ENDIF.

*   Gera históricos de lançamentos
    CLEAR zlest0020.
    MOVE: ti_obj_contabil-codtrp     TO zlest0022-transportador,
          ti_obj_contabil-codposto   TO zlest0022-posto,
          ti_obj_contabil-lote       TO zlest0022-lote,
          ti_obj_contabil-chvid      TO zlest0022-chvid,
          ti_obj_contabil-tiptransp  TO zlest0022-tiptransp,
          ti_obj_contabil-ctlglancto TO zlest0022-ctlglancto,
          ti_obj_contabil-docsap     TO zlest0022-docsap,
          ti_obj_contabil-gjahr      TO zlest0022-gjahr,
          ti_obj_contabil-bukrs      TO zlest0022-bukrs,
          ti_obj_contabil-observ     TO zlest0022-obs_confer,
          sy-datum                   TO zlest0022-erdat,
          sy-uname                   TO zlest0022-uname.
    MODIFY zlest0020 FROM zlest0020.

  ENDLOOP.

* Atualiza status do documento (Header)
  UPDATE zlest0015
      SET status = cc_confirmado
          erdat  = sy-datum
          uzeit  = sy-uzeit
          uname  = sy-uname
    WHERE transportador = ti_obj_contabil-codtrp
      AND posto         = ti_obj_contabil-codposto
      AND lote          = ti_obj_contabil-lote.

ENDFORM.                    " LANCTO_GRAVA_CONFERENCIA

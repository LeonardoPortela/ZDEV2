*&---------------------------------------------------------------------*
*& Report  Z_PP_MFBF
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z_pp_mfbf.

***********************************************************************
* Constantes
***********************************************************************
CONSTANTS:
  cc_a               VALUE 'A',
  cc_c               VALUE 'C',
  cc_e               VALUE 'E',
  cc_i               VALUE 'I',
  cc_s               VALUE 'S',
  cc_p               VALUE 'P',
  cc_n               VALUE 'N',
  cc_m               VALUE 'M',
  cc_x               VALUE 'X',
  cc_w               VALUE 'W',
  cc_16              TYPE i          VALUE 16,
  cc_eq(2)           TYPE c          VALUE 'EQ',
  cc_ge(2)           TYPE c          VALUE 'GE',
  cc_classe_mensagem TYPE arbgb      VALUE 'Z01',
  cc_tpexec_01(2)    TYPE c          VALUE '01',
  cc_tpexec_02(2)    TYPE c          VALUE '02',
  cc_mfbf            TYPE sy-tcode   VALUE 'MFBF',
  cc_pdc_number      TYPE sa_bdenr   VALUE '15',
  cc_deposito_pr01   TYPE alort      VALUE 'PR01',
  cc_deposito_in01   TYPE alort      VALUE 'IN01',
  cc_bwart_131       TYPE bwart      VALUE '131',
  cc_bwart_261       TYPE bwart      VALUE '261',
  cc_bwart_262       TYPE bwart      VALUE '262',
  cc_tipo_conf_01    TYPE bckfltype  VALUE '01',
  cc_tipo_conf_11    TYPE bckfltype  VALUE '11'.

***********************************************************************
* TIPOS
***********************************************************************
TYPES: BEGIN OF ty_execretrn,
         type       TYPE bapi_mtype,
         id         TYPE symsgid,
         number     TYPE symsgno,
         message    TYPE bapi_msg,
         message_v1 TYPE symsgv,
         message_v2 TYPE symsgv,
         message_v3 TYPE symsgv,
         message_v4 TYPE symsgv,
       END    OF ty_execretrn.

***********************************************************************
* TABELAS Internas
***********************************************************************
DATA: yt_execretrn        TYPE TABLE OF ty_execretrn WITH HEADER LINE INITIAL SIZE 0,
      yt_log_mfpf         TYPE TABLE OF zfie_ret_document WITH HEADER LINE INITIAL SIZE 0,
      yt_geo_oubound_ok   TYPE TABLE OF zmme_return_sucess WITH HEADER LINE INITIAL SIZE 0,
      yt_geo_oubound_erro TYPE TABLE OF zmme_return_error WITH HEADER LINE INITIAL SIZE 0,
      yt_goodsmovements   TYPE TABLE OF bapi2017_gm_item_create WITH HEADER LINE INITIAL SIZE 0,
      yt_return           TYPE TABLE OF bapiret2 WITH HEADER LINE INITIAL SIZE 0,
      ti_bdc              TYPE TABLE OF bdcdata,
      ti_msg              TYPE TABLE OF bdcmsgcoll,
      t_xi_mfbf           TYPE TABLE OF zpps_ximfbf_log,
      t_xi_mfbf_aux       TYPE TABLE OF zpps_ximfbf_log,
      t_xi_mfbf_log       TYPE TABLE OF zpps_ximfbf_log.

***********************************************************************
* WORK AREAS
***********************************************************************
DATA: w_xi_mfbf       TYPE zpps_ximfbf_log,
      w_xi_mfbf_aux   TYPE zpps_ximfbf_log,
      w_xi_mfbf_log   TYPE zpps_ximfbf_log,
      w_bflushflags   TYPE bapi_rm_flg,
      w_bflushdatagen TYPE bapi_rm_datgen,
      w_bapiret2      TYPE bapiret2,
      wa_bdc          TYPE bdcdata,
      wa_msg          TYPE bdcmsgcoll.
***********************************************************************
* VARIÁVEIS
***********************************************************************
DATA: vc_confirmation     TYPE prtnr,
      vc_cancconfirm      TYPE canc_prtnr,
      vc_ult_doc_gerado   TYPE mblnr,
      vc_dt_lacto_gerado  TYPE d,
      vc_ano_doc_contabil TYPE mjahr,
      vc_empr_doc_gerado  TYPE bukrs,
      vc_aufnr_pai        TYPE aufnr,
      vc_matnr_pai        TYPE matnr,
      vc_werks_pai        TYPE werks_d,
      vc_lgort_pai        TYPE lgort_d,
      vc_verid_pai        TYPE verid,
      vl_mode             TYPE c LENGTH 1,
      vl_xchar            TYPE marc-xchar,
      vl_erro             TYPE c LENGTH 1,
      l_erro_lote         TYPE c LENGTH 1, "*-#126585 - 31.10.2023 - JT
      vl_ajustado         TYPE c LENGTH 1,
      vl_nrbol            TYPE mkpf-bktxt,
      vl_nrbol_aux        TYPE string,
      vl_mjahr            TYPE c LENGTH 4,
      vl_matnr            TYPE marc-matnr,
      vl_quant            TYPE mseg-menge,
      vf_new_charg        TYPE charg_d,
      vf_new_mblnr        TYPE mblnr,
      vf_quebra,
      vf_muda_bol,
      vf_erro,
      vf_erro_aux.

TABLES:zpps_goodsmv_log, mcha.

TYPES: BEGIN OF ty_zpps_goodsmv_log ,
         obj_key TYPE zpps_goodsmv_log-obj_key,
       END OF ty_zpps_goodsmv_log .

DATA : t_zpps_goodsmv_log     TYPE TABLE OF ty_zpps_goodsmv_log,
       t_zpps_goodsmv_log_aux TYPE TABLE OF ty_zpps_goodsmv_log,
       it_zpps_goodsmv_log    TYPE TABLE OF zpps_goodsmv_log,
       t_bdcmsgcoll           TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

DATA : wa_zpps_goodsmv_log TYPE zpps_goodsmv_log,
       st_zpps_goodsmv_log TYPE ty_zpps_goodsmv_log.


*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
DATA: vg_job TYPE i.

SELECT SINGLE COUNT(*) INTO vg_job
  FROM tbtco
 WHERE jobname EQ 'BOLETIM_CONSUMO_GEO'
   AND status EQ 'R'.

IF ( vg_job EQ 1 ).

  PERFORM processa_erros CHANGING vl_erro.
  CHECK vl_erro = abap_false.

  PERFORM seleciona_dados.
  PERFORM processa.

ENDIF.

*&---------------------------------------------------------------------*
*&      Form  seleciona_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM seleciona_dados.

  DATA : vl_id_boletim TYPE zpps_ximfbf_log-obj_key.

  REFRESH: t_xi_mfbf.
  CLEAR: t_xi_mfbf, t_xi_mfbf[], t_xi_mfbf_log, t_xi_mfbf_log[].

  "--------------------------

*------------------
  MESSAGE s024(sd) WITH '**** inicio - Selecionando dados'.
*------------------

  SELECT *
    FROM zpps_ximfbf_log
    INTO TABLE t_xi_mfbf_log
   WHERE zrg_atulizado EQ 'N'
     AND id_interface  NE 'S' "Projeto Reestruturação Algodao 2024
     AND processado    NE 'P' ORDER BY PRIMARY KEY ."--P = Em processamento

  IF 1 = 2. "Debug "Projeto Reestruturação Algodao 2024
    DATA: lva_obj_key TYPE zpps_ximfbf_log-obj_key.

    SELECT *
      FROM zpps_ximfbf_log INTO TABLE t_xi_mfbf_log
     WHERE obj_key  = lva_obj_key.
  ENDIF.

*------------------
  MESSAGE s024(sd) WITH '**** fim - Selecionando dados'.
*------------------

  t_xi_mfbf[] = t_xi_mfbf_log[].
  SORT t_xi_mfbf BY obj_key.
  DELETE ADJACENT DUPLICATES FROM t_xi_mfbf COMPARING obj_key.

*  SELECT *
*    FROM ZPPS_XIMFBF_LOG
*    INTO TABLE T_XI_MFBF_LOG
*   WHERE ZRG_ATULIZADO EQ 'N'
*     AND PROCESSADO    NE 'P'."--P = Em processamento.

  CLEAR vl_id_boletim.
  IF vl_id_boletim IS NOT INITIAL."----Caso precise debugar algum boletim especifico

    CLEAR: t_xi_mfbf, t_xi_mfbf[], t_xi_mfbf_log, t_xi_mfbf_log[].

    SELECT *
      FROM zpps_ximfbf_log
      INTO TABLE t_xi_mfbf_log
     WHERE obj_key = vl_id_boletim ORDER BY PRIMARY KEY .

    t_xi_mfbf[] = t_xi_mfbf_log[].

    DELETE ADJACENT DUPLICATES FROM t_xi_mfbf COMPARING obj_key.

*    SELECT *
*      FROM ZPPS_XIMFBF_LOG
*      INTO TABLE T_XI_MFBF_LOG
*     WHERE OBJ_KEY = VL_ID_BOLETIM.

  ENDIF .

ENDFORM.                    "seleciona_dados
*&---------------------------------------------------------------------*
*&      Form  PROCESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processa .

* INICIALIZAÇÕES
  PERFORM yf_inicializacoes.

*------------------
  MESSAGE s024(sd) WITH '**** inicio - Alterar Status'.
*------------------

  CLEAR: w_xi_mfbf.
  "Atualiza registros para lidos
  LOOP AT t_xi_mfbf_log INTO w_xi_mfbf_log.
    w_xi_mfbf_log-zrg_atulizado = 'S'.
    w_xi_mfbf_log-processado    = 'P'."--Em processamento
    w_xi_mfbf_log-data          = sy-datum.
    w_xi_mfbf_log-hora          = sy-uzeit.
    MODIFY zpps_ximfbf_log FROM w_xi_mfbf_log.
  ENDLOOP.

*------------------
  MESSAGE s024(sd) WITH '**** fim - Alterar Status'.
*------------------

  CLEAR: w_xi_mfbf.

*-CS2022000332-#84404-02.08.2022-JT-inicio
*-Liberar JOB
  zcl_integracao_cotton_sap=>unlock_objeto( 'BOLETIM_CONSUMO_GEO' ).
*-CS2022000332-#84404-02.08.2022-JT-fim

  COMMIT WORK.

  MOVE t_xi_mfbf[] TO t_xi_mfbf_aux[].

  SORT t_xi_mfbf_aux BY obj_key nrobol fgorigem werks.
  DELETE ADJACENT DUPLICATES FROM t_xi_mfbf_aux COMPARING obj_key nrobol fgorigem werks.

  SORT t_xi_mfbf BY obj_key aufnr mblnr zst_atlz matnr.

  LOOP AT t_xi_mfbf_aux INTO w_xi_mfbf_aux.

    REFRESH: yt_goodsmovements,
             yt_return,
             yt_execretrn.

    CLEAR: yt_goodsmovements,
           w_bflushflags,
           w_bflushdatagen,
           vc_aufnr_pai.

    CLEAR: vf_quebra,
           vf_muda_bol,
           vl_xchar,
           yt_goodsmovements[],
           vf_erro,
           vf_erro_aux.

    REFRESH yt_goodsmovements.

    LOOP AT t_xi_mfbf INTO w_xi_mfbf WHERE obj_key EQ w_xi_mfbf_aux-obj_key
                                       AND nrobol  EQ w_xi_mfbf_aux-nrobol
                                       AND fgorigem EQ w_xi_mfbf_aux-fgorigem
                                       AND werks EQ w_xi_mfbf_aux-werks.

*-CS2022000332-#84404-02.08.2022-JT-inicio
*-----------------------------------------------------
*---verifica se ha um lote criado para o mesmo ID-COTTON
*---nao permitir duplicacao de linhas para mesmo id-cotton,
*---causando problemas de saldo nos consumos
*-----------------------------------------------------
      IF w_xi_mfbf-zst_atlz = cc_i.
*------------------
        MESSAGE s024(sd) WITH '**** inicio - Selecao zpps_ximfbf_log/mseg'.
*------------------

        SELECT mblnr
          INTO @DATA(_mblnr)
          FROM zpps_ximfbf_log
            UP TO 1 ROWS
         WHERE obj_key    = @w_xi_mfbf-obj_key
           AND nrobol     = @w_xi_mfbf-nrobol
           AND fgorigem   = @w_xi_mfbf-fgorigem
           AND werks      = @w_xi_mfbf-werks
           AND matnr      = @w_xi_mfbf-matnr
           AND cd_ccusto  = @w_xi_mfbf-cd_ccusto
           AND charg     <> @w_xi_mfbf-charg
           AND id_cotton  = @w_xi_mfbf-id_cotton
           AND zst_atlz   = @cc_i.
        ENDSELECT.
        IF sy-subrc = 0.
          SELECT SINGLE smbln  "está estornado?
            FROM mseg
            INTO @DATA(_smbln)
           WHERE smbln = @_mblnr.
          IF sy-subrc <> 0.
            vf_erro_aux = abap_true.
            DELETE FROM  zpps_ximfbf_log WHERE obj_key    = @w_xi_mfbf-obj_key
                                           AND nrobol     = @w_xi_mfbf-nrobol
                                           AND fgorigem   = @w_xi_mfbf-fgorigem
                                           AND werks      = @w_xi_mfbf-werks
                                           AND matnr      = @w_xi_mfbf-matnr
                                           AND cd_ccusto  = @w_xi_mfbf-cd_ccusto
                                           AND charg      = @w_xi_mfbf-charg
                                           AND zst_atlz   = @cc_i.
            COMMIT WORK.
            EXIT.
          ENDIF.
        ENDIF.
*------------------
        MESSAGE s024(sd) WITH '**** fim - Selecao zpps_ximfbf_log/mseg'.
*------------------
      ENDIF.
*-CS2022000332-#84404-02.08.2022-JT-fim

      CLEAR vf_new_charg.

*   Alimenta tabelas de itens para mesmo documento
      CASE w_xi_mfbf-zst_atlz.
        WHEN cc_i OR cc_c.  "Entrada
          PERFORM yf_itens_entrada_consumo CHANGING vf_erro.
        WHEN cc_m.
          PERFORM yf_itens_entrada_combustivel CHANGING vf_erro.
        WHEN cc_e.  "Estorno
          PERFORM yf_obtem_nrconfirmacao CHANGING vf_erro.
        WHEN OTHERS.
      ENDCASE.
      IF vf_erro_aux IS INITIAL.
        vf_erro_aux = vf_erro.
      ENDIF.
    ENDLOOP.

    CLEAR: vf_new_mblnr, vl_erro.  "*-CS2022000332-#84404-02.08.2022-JT-fim

    IF vf_erro_aux IS INITIAL.
      MOVE w_xi_mfbf_aux TO w_xi_mfbf.
      IF w_xi_mfbf_aux-zst_atlz = cc_e.
        PERFORM yf_exec_bapi_estorno.
      ELSE.
        IF w_xi_mfbf_aux-zst_atlz = cc_c.
          " Verificando se existe algum log de erro referente ao boletim atual...
          READ TABLE yt_geo_oubound_erro WITH KEY idbol = w_xi_mfbf_aux-obj_key BINARY SEARCH TRANSPORTING NO FIELDS.

          " Se encontrou o id_boletim na tabela de erros, entao limpa a tabela de criacao de doc. material
          IF sy-subrc = 0.
            REFRESH yt_goodsmovements.
          ELSE.
            PERFORM yf_exec_bapi_entrada_consumo.
          ENDIF.

        ELSEIF w_xi_mfbf_aux-zst_atlz = cc_m .
          PERFORM processa_consumo_combustivel.
        ELSE.
          PERFORM yf_exec_bapi_entrada_consumo.
        ENDIF.
      ENDIF.
    ENDIF.

*-CS2022000332-#84404-02.08.2022-JT-inicio
*-para registro = "I", encerrar processamento só se gerou documento material.
*-se não gerou, volta status para reprocessamento
    IF w_xi_mfbf_aux-zst_atlz = cc_i.
      IF vf_new_mblnr IS NOT INITIAL.
        UPDATE zpps_ximfbf_log SET processado = 'S'
                             WHERE obj_key    = w_xi_mfbf_aux-obj_key.
      ELSE.
        UPDATE zpps_ximfbf_log SET processado    = 'N'
                                   zrg_atulizado = 'N'
                             WHERE obj_key       = w_xi_mfbf_aux-obj_key.
      ENDIF.
    ELSE.
      UPDATE zpps_ximfbf_log SET processado = 'S'
                           WHERE obj_key    = w_xi_mfbf_aux-obj_key.
    ENDIF.
*-CS2022000332-#84404-02.08.2022-JT-fim

    COMMIT WORK.

  ENDLOOP.

*------------------
  MESSAGE s024(sd) WITH '**** inicio - retorna log XI 1'.
*------------------

* RETORNA O LOG DE PROCESSAMENTO PARA O XI
  PERFORM yf_retorna_log_interface_xi.

*------------------
  MESSAGE s024(sd) WITH '**** fim - retorna log XI 1'.
*------------------

ENDFORM.                    " PROCESSA

INCLUDE z_pp_mfbf_yf_inicializacoesf01.

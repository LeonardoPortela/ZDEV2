*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
* Cliente....: Maggi                                                   *
* Autor......: Daniela Machado                                         *
* Data.......: 14.07.2010                                              *
* Descrição  : Capturar dados do sistema OPUS para carregar a tabela   *
*              ZSDT0001.                                               *
* Projeto....: Maggi - Projeto Evoluir                                 *
* Cód Espec..: GAP_SD03                                                *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:  .                                                       *
*----------------------------------------------------------------------*

FUNCTION ZSD_INBOUND_REMESSA_BSA.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IB_ROMANEIO TYPE  ZSDT001 OPTIONAL
*"----------------------------------------------------------------------
  REFRESH: IT_LOG,
           IT_ZSDT0001.

  DATA: TL_ZSDT0001      TYPE TABLE OF ZSDT0001_BSA,
        WA_ZSDT0001_AUX  TYPE ZSDT0001_BSA,
        TL_ZSDT000X      TYPE TABLE OF ZSDT0001_BSA,
        VL_INDEX         TYPE SYINDEX,
        VL_TEXTO         TYPE CHAR100,
        VL_DOCNUM_AQUAV  TYPE ZSDT0001_BSA-DOCNUM_AQUAV,
        VL_CT_AQUAV      TYPE ZSDT0001_BSA-CT_AQUAV,
        VL_ST_CCT        TYPE ZSDT0001_BSA-ST_CCT,
        "VL_ID_CARGA      TYPE ZSDT0001-ID_CARGA,
        WL_ZLEST0142_DEL TYPE ZLEST0142,
        WL_ZLEST0104     TYPE ZLEST0104.

  DATA: ZCL_CCT_CONTROL_NF TYPE REF TO ZCL_CCT_CONTROL_NF.

  DATA: WA_IB_ROMANEIO TYPE ZSDS001,
        WA_ZSDT0001   TYPE ZSDT0001_BSA.
* Preenche a estrutura da tabela ZSDT0001 com os valores inseridos
* através da tavela IB_ROMANEIO
  "--MOVE  TO .
  "TL_IB_ROMANEIO[] = IB_ROMANEIO[].
  REFRESH IT_ZSDT0001.

  LOOP AT IB_ROMANEIO INTO WA_IB_ROMANEIO.
    CLEAR WA_ZSDT0001.
    MOVE-CORRESPONDING  WA_IB_ROMANEIO TO WA_ZSDT0001.

    IF WA_ZSDT0001-ID_ORDEM IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(WA_ZSDT0001OD)
        FROM ZSDT0001OD
       WHERE ID_ORDEM EQ @WA_ZSDT0001-ID_ORDEM.

      IF SY-SUBRC IS INITIAL.
        WA_ZSDT0001-PLACA_CAV  = WA_ZSDT0001OD-DS_PLACA_TRATOR.
        WA_ZSDT0001-PLACA_CAR1 = WA_ZSDT0001OD-DS_PLACA_REBOQ_1.
        WA_ZSDT0001-PLACA_CAR2 = WA_ZSDT0001OD-DS_PLACA_REBOQ_2.
        WA_ZSDT0001-PLACA_CAR3 = WA_ZSDT0001OD-DS_PLACA_REBOQ_3.
      ENDIF.

    ENDIF.

    APPEND WA_ZSDT0001 TO IT_ZSDT0001.
  ENDLOOP.

  IF NOT IT_ZSDT0001[] IS INITIAL.

*  Verifica se já Existe Romaneio com Remessa Emitida
    SELECT *
      FROM ZSDT0001_BSA
      INTO TABLE TL_ZSDT0001
       FOR ALL ENTRIES IN IT_ZSDT0001
     WHERE CH_REFERENCIA EQ IT_ZSDT0001-CH_REFERENCIA
       AND STATUS        NE SPACE.

    SORT TL_ZSDT0001 BY CH_REFERENCIA ASCENDING.

*    IF sy-subrc IS INITIAL.
    LOOP AT IT_ZSDT0001 INTO WA_ZSDT0001.

      VL_INDEX = SY-TABIX.

      CLEAR: VL_DOCNUM_AQUAV, VL_CT_AQUAV, VL_ST_CCT. ", VL_ID_CARGA.

      SELECT SINGLE DOCNUM_AQUAV CT_AQUAV ST_CCT
                    "ID_CARGA
             INTO (VL_DOCNUM_AQUAV,VL_CT_AQUAV, VL_ST_CCT
                   ", VL_ID_CARGA
                   )
        FROM ZSDT0001_BSA
       WHERE CH_REFERENCIA = WA_ZSDT0001-CH_REFERENCIA.

*      IF SY-SUBRC IS NOT INITIAL.
*        "Se Encontrar um Romaneio Temporário da ZMM0127 o Mesmo deve vir por WebService
*        SELECT SINGLE ID_CARGA INTO (VL_ID_CARGA)
*          FROM ZSDT0001
*         WHERE CH_REFERENCIA NE WA_ZSDT0001-CH_REFERENCIA
*           AND TP_MOVIMENTO  EQ WA_ZSDT0001-TP_MOVIMENTO
*           AND NR_ROMANEIO   EQ WA_ZSDT0001-NR_ROMANEIO
*           AND NR_SAFRA      EQ WA_ZSDT0001-NR_SAFRA
*           AND BUKRS         EQ WA_ZSDT0001-BUKRS
*           AND BRANCH        EQ WA_ZSDT0001-BRANCH
*           AND ID_CARGA      NE SPACE.
*      ENDIF.

      "Caso seja um range da ZMM0127 não é para aceitar, registro gerado somente por WebService
      IF WA_ZSDT0001-NR_ROMANEIO GE 000700000 AND
         WA_ZSDT0001-NR_ROMANEIO LE 000799999 .
        DELETE IT_ZSDT0001 INDEX VL_INDEX.
        CONTINUE.
      ENDIF.

      "se ja tiver o VL_DOCNUM_AQUAV enão não pode mais atualizar.
      IF VL_DOCNUM_AQUAV  <> '0000000000'  OR VL_CT_AQUAV = 'X' OR VL_ST_CCT = '02'. " OR VL_ID_CARGA IS NOT INITIAL.
        DELETE IT_ZSDT0001 INDEX VL_INDEX.
        CONTINUE.
              ENDIF.

*     Preenche valores para exibição de mensagens
      WA_LOG-OBJ_KEY        = WA_ZSDT0001-CH_REFERENCIA.
      WA_LOG-INTERFACE      = C_14.
      WA_LOG-DT_ATUALIZACAO = SY-DATUM.
      WA_LOG-HR_ATUALIZACAO = SY-UZEIT.
      WA_LOG-ID             = C_SD.
      WA_LOG-NUM            = C_897.
      "Verifica na tabela de romaneios usados se o romaneio do teste consta nessa tabela
      READ TABLE TL_ZSDT0001 INTO WA_ZSDT0001_AUX WITH KEY CH_REFERENCIA = WA_ZSDT0001-CH_REFERENCIA BINARY SEARCH.

      "Se não encontrou o romaneio na tabela de romaneio usado então atualizar dos dados ou excluir quando
      "for cancelamento do OPUS
      IF NOT SY-SUBRC IS INITIAL.
        WA_LOG-TYPE = C_S.
        IF WA_ZSDT0001-TP_MOVIMENTO EQ 'D'.

          FREE ZCL_CCT_CONTROL_NF.
          CREATE OBJECT ZCL_CCT_CONTROL_NF.
          DATA(_ATRIB_NF) = ZCL_CCT_CONTROL_NF->ATRIBUIR_NF_ROM( EXPORTING I_CH_REFERENCIA = WA_ZSDT0001-CH_REFERENCIA
                                                                 IMPORTING E_NOTA_FISCAL   = WL_ZLEST0142_DEL ).
          IF ( _ATRIB_NF EQ ABAP_TRUE ) AND ( WL_ZLEST0142_DEL IS NOT INITIAL  ).
            ZCL_CCT_CONTROL_NF->REMOVER_NF_CCT( I_ZLEST0142 =  WL_ZLEST0142_DEL ).
          ENDIF.

          DELETE IT_ZSDT0001 INDEX VL_INDEX.
          DELETE FROM ZSDT0001_BSA WHERE CH_REFERENCIA = WA_ZSDT0001-CH_REFERENCIA.

          CONCATENATE TEXT-S01 WA_ZSDT0001-CH_REFERENCIA TEXT-S03 INTO WA_LOG-MESSAGE SEPARATED BY SPACE.
        ELSE.
*       text-s01 -> Chave de referência
*       text-s02 -> gravada com sucesso na tabela ZSDT0001.
          CONCATENATE TEXT-S01 WA_ZSDT0001-CH_REFERENCIA TEXT-S02 INTO WA_LOG-MESSAGE SEPARATED BY SPACE.
        ENDIF.
      ELSE.
        DELETE IT_ZSDT0001 INDEX VL_INDEX.
        WA_LOG-TYPE = C_E.
*       text-s01 -> Chave de referência
*       text-e03 -> com remessa emitida, não pode ser alterado.
*       text-e02 ->Romaneio
        CONCATENATE WA_ZSDT0001-CH_REFERENCIA TEXT-E02 INTO VL_TEXTO SEPARATED BY SPACE.
        CONCATENATE TEXT-S01 VL_TEXTO TEXT-E03 INTO WA_LOG-MESSAGE SEPARATED BY SPACE.

        IF WA_ZSDT0001_AUX-ID_REFERENCIA IS INITIAL AND WA_ZSDT0001-ID_REFERENCIA IS NOT INITIAL.
          UPDATE ZSDT0001
             SET ID_REFERENCIA = WA_ZSDT0001-ID_REFERENCIA
           WHERE CH_REFERENCIA = WA_ZSDT0001-CH_REFERENCIA.
        ENDIF.
      ENDIF.

      APPEND WA_LOG TO IT_LOG.

      CLEAR: WA_LOG, VL_TEXTO.
    ENDLOOP.
*  ELSE.
*    wa_log-type           = c_e.
**     text-e01 -> Erro ao gravar dados na tabela ZSDT0001.
*    wa_log-message = text-e01.
*    APPEND wa_log TO it_log.
*    CLEAR wa_log.
*    ROLLBACK WORK.
*  ENDIF.
  ENDIF.

  IF NOT IT_ZSDT0001[] IS INITIAL.
*   Atualiza dados na tabela ZSDT0001
    LOOP AT IT_ZSDT0001 INTO WA_ZSDT0001.

      "Verifica se Romaneio será utilizado na ZLES0077(Aquaviario)
      SELECT SINGLE *
        FROM ZLEST0104 INTO WL_ZLEST0104
       WHERE LOCAL_DESCARGA = WA_ZSDT0001-LOCAL_DESCARGA
         AND BUKRS          = WA_ZSDT0001-BUKRS
         AND BRANCH         = WA_ZSDT0001-BRANCH.

      IF SY-SUBRC IS INITIAL.
        CALL FUNCTION 'Z_LES_RETENCAO_AQUA'
          EXPORTING
            I_BUKRS        = WA_ZSDT0001-BUKRS
            I_BRANCH       = WA_ZSDT0001-BRANCH
            I_LIFNR        = WA_ZSDT0001-PARID
            I_PESO         = WA_ZSDT0001-PESO_SUBTOTAL
          IMPORTING
            E_PESO_RETIDO  = WA_ZSDT0001-PESO_RETIDO_EST
            E_PESO_LIQUIDO = WA_ZSDT0001-PESO_LIQRET_EST.
      ENDIF.

      MODIFY ZSDT0001_BSA FROM WA_ZSDT0001.

      FREE ZCL_CCT_CONTROL_NF.
      CREATE OBJECT ZCL_CCT_CONTROL_NF.
      DATA(_ATRIB) = ZCL_CCT_CONTROL_NF->ATRIBUIR_NF_ROM( I_CH_REFERENCIA = WA_ZSDT0001-CH_REFERENCIA ).
      IF _ATRIB EQ ABAP_TRUE.
        ZCL_CCT_CONTROL_NF->DISP_NF_CCT( ).
      ENDIF.

    ENDLOOP.
  ENDIF.

* Chamar função assíncrona de retorno, confirmando a gravação
* de dados na tabela ZSDT0001
  IF NOT IT_LOG[] IS INITIAL.
*--> 26.09.2023 18:01:54 - Migração S4 – ML - Início

*    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETURN'
*      TABLES
*        OUTRETURN = IT_LOG.

    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        AS SEPARATE UNIT
        TABLES
          outreturn = IT_LOG.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          outreturn = IT_LOG.
    ENDIF.
*<-- 26.09.2023 18:01:54 - Migração S4 – ML – Fim
  ENDIF.

  COMMIT WORK.

ENDFUNCTION.

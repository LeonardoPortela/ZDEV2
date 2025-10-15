*&---------------------------------------------------------------------*
*& Report  ZMMR020MIRO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR020MIRO.


PARAMETERS: PREFKEY TYPE J_1BREFKEY NO-DISPLAY.


START-OF-SELECTION.

  DATA: VL_MESSAGE_V1       TYPE SYMSGV,
        VL_MESSAGE_V2       TYPE SYMSGV,
        VL_MESSAGE_V3       TYPE SYMSGV,
        VL_WERKS            TYPE T001W-J_1BBRANCH,
        VG_REFKEY	          TYPE J_1BREFKEY,
        VG_AWKEY            TYPE AWKEY,
        WA_J_1BNFLIN        TYPE J_1BNFLIN,
        WA_J_1BNFDOC        TYPE J_1BNFDOC,
        P_DATA_ENT          TYPE DATUM,
        P_DATA_VAL          TYPE DATUM,
        WA_RBKP             TYPE RBKP,
        WA_BKPF             TYPE BKPF,
        IT_BSAK             TYPE TABLE OF BSAK WITH HEADER LINE,
        IT_NOTAS            TYPE TABLE OF ZIB_NOTA_FISCAL_SAP WITH HEADER LINE,
        WA_RETURN           TYPE BAPIRET2,
        WA_INVOICEDOCNUMBER TYPE BAPI_INCINV_FLD,
        WA_PSTNG_DATE       TYPE BAPI2017_GM_HEAD_02-PSTNG_DATE,
        IT_RETURN           TYPE TABLE OF BAPIRET2.

  SELECT SINGLE * INTO @DATA(WA_ZMMT0098)
    FROM ZMMT0098
   WHERE ID_REFKEY EQ @PREFKEY.

  CHECK  SY-SUBRC IS INITIAL.

  SELECT SINGLE * INTO WA_RBKP
    FROM RBKP
   WHERE BELNR EQ WA_ZMMT0098-RE_BELNR
     AND GJAHR EQ WA_ZMMT0098-GJAHR.

  IF SY-SUBRC IS INITIAL.

    SELECT SINGLE * INTO WA_BKPF
      FROM BKPF
     WHERE BUKRS = WA_RBKP-BUKRS
       AND GJAHR = WA_RBKP-GJAHR
       AND AWTYP = 'RMRP'
       AND AWKEY = VG_AWKEY.

    IF SY-SUBRC IS INITIAL.

      SELECT * INTO TABLE IT_BSAK
        FROM BSAK
       WHERE GJAHR EQ WA_BKPF-GJAHR
         AND BUKRS EQ WA_BKPF-BUKRS
         AND BELNR EQ WA_BKPF-BELNR.

      IF SY-SUBRC IS INITIAL.
        WA_ZMMT0098-VG_ERRO_P = 'X'.
        WA_ZMMT0098-VG_ERRO   = 'X'.

        CONCATENATE 'Documento' WA_ZMMT0098-RE_BELNR WA_ZMMT0098-GJAHR 'compesando!' INTO DATA(VG_MESSAGE) SEPARATED BY SPACE.
        WA_RETURN-TYPE    = 'E'.
        WA_RETURN-ID      = 'MM'.
        WA_RETURN-NUMBER  = '899'.
        WA_RETURN-MESSAGE = VG_MESSAGE.
        WA_RETURN-MESSAGE_V1 = WA_ZMMT0098-RE_BELNR.
        WA_RETURN-MESSAGE_V2 = WA_ZMMT0098-GJAHR.
        PERFORM Z_PREPARA_MENSAGEM2  USING WA_ZMMT0098-OBJ_KEY
                                           WA_RETURN-TYPE
                                           WA_RETURN-MESSAGE
                                           WA_RETURN-MESSAGE_V1
                                           WA_RETURN-MESSAGE_V2
                                           WA_RETURN-MESSAGE_V3
                                           WA_ZMMT0098-ID_INTERFACE_MIR.
      ENDIF.
    ENDIF.

    MODIFY ZMMT0098 FROM WA_ZMMT0098.
    COMMIT WORK.
    CHECK WA_ZMMT0098-VG_ERRO_P IS INITIAL.

    SELECT SINGLE * INTO WA_J_1BNFLIN
      FROM J_1BNFLIN
     WHERE REFKEY EQ VG_REFKEY
       AND REFTYP EQ 'LI'.

    IF SY-SUBRC IS INITIAL.

      SELECT SINGLE * INTO WA_J_1BNFDOC FROM J_1BNFDOC WHERE DOCNUM EQ WA_J_1BNFLIN-DOCNUM.
      IF ( SY-SUBRC IS INITIAL ) AND ( WA_J_1BNFDOC-FORM IS NOT INITIAL ).

        SELECT SINGLE * INTO @DATA(WA_ZMMT_EE_ZGR) FROM ZMMT_EE_ZGR WHERE OBJ_KEY EQ @WA_ZMMT0098-OBJ_KEY.
        IF WA_ZMMT_EE_ZGR-ID_CARGA IS NOT INITIAL.

          CALL FUNCTION 'Z_NFE_CTE_AUTORIZADO'
            EXPORTING
              P_DOCNUM       = WA_J_1BNFLIN-DOCNUM
              P_USO          = 'N'
            EXCEPTIONS
              CANCELADO      = 1
              NAO_CANCELADO  = 2
              PENDENTE       = 3
              NAO_CONCLUIDO  = 4
              NAO_EXISTE     = 5
              AUTORIZADO_USO = 6
              DENEGADO       = 7
              OTHERS         = 8.

          IF SY-SUBRC IS NOT INITIAL.

            WA_ZMMT_EE_ZGR-ST_ESTORNO = ZIF_CARGA=>ST_STATUS_ESTORNO_BLOQUEIO.
            MODIFY ZMMT_EE_ZGR FROM WA_ZMMT_EE_ZGR.

            WA_ZMMT0098-VG_ERRO_P = 'X'.
            WA_ZMMT0098-VG_ERRO   = 'X'.
            MODIFY ZMMT0098 FROM WA_ZMMT0098.
            COMMIT WORK.

            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO VG_MESSAGE.
            WA_RETURN-TYPE       = 'E'.
            WA_RETURN-ID         = 'MM'.
            WA_RETURN-NUMBER     = '899'.
            WA_RETURN-MESSAGE    = VG_MESSAGE.
            WA_RETURN-MESSAGE_V1 = SY-MSGV1.
            WA_RETURN-MESSAGE_V2 = SY-MSGV2.
            WA_RETURN-MESSAGE_V3 = SY-MSGV3.
            PERFORM Z_PREPARA_MENSAGEM2  USING WA_ZMMT0098-OBJ_KEY
                                               WA_RETURN-TYPE
                                               WA_RETURN-MESSAGE
                                               WA_RETURN-MESSAGE_V1
                                               WA_RETURN-MESSAGE_V2
                                               WA_RETURN-MESSAGE_V3
                                               WA_ZMMT0098-ID_INTERFACE_MIR.
            EXIT.
          ENDIF.
        ENDIF.

        P_DATA_ENT = WA_J_1BNFDOC-PSTDAT.

        WA_ZMMT0098-VAL_FI = 'X'.
        MODIFY ZMMT0098 FROM WA_ZMMT0098.
        COMMIT WORK.

        CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
          EXPORTING
            P_DATA_ENT = P_DATA_ENT
            P_BUKRS    = WA_J_1BNFDOC-BUKRS
            P_VAL_FI   = WA_ZMMT0098-VAL_FI
            P_VAL_MM   = WA_ZMMT0098-VAL_MM
          IMPORTING
            P_DATA_VAL = P_DATA_VAL.

        IF ( SY-SUBRC IS INITIAL ) AND ( P_DATA_ENT EQ P_DATA_VAL ).
          CLEAR: IT_NOTAS[].
          IT_NOTAS-NU_DOCUMENTO_SAP = WA_J_1BNFDOC-DOCNUM.
          IT_NOTAS-TP_AUTHCOD       = '1'.
          IT_NOTAS-CODE             = '999'.
          CONCATENATE 'Estornado por' SY-UNAME INTO IT_NOTAS-MS_ERRO SEPARATED BY SPACE.
          APPEND IT_NOTAS.
          CALL FUNCTION 'Z_SD_INBOUND_NFE_XML'
            TABLES
              IT_NOTAS = IT_NOTAS.
        ELSE.
          WA_ZMMT0098-VG_ERRO_P = 'X'.
          WA_ZMMT0098-VG_ERRO   = 'X'.
          MODIFY ZMMT0098 FROM WA_ZMMT0098.
          COMMIT WORK.

          CONCATENATE 'Documento' WA_ZMMT0098-RE_BELNR WA_ZMMT0098-GJAHR 'mês fechado!' INTO VG_MESSAGE SEPARATED BY SPACE.
          WA_RETURN-TYPE       = 'E'.
          WA_RETURN-ID         = 'MM'.
          WA_RETURN-NUMBER     = '899'.
          WA_RETURN-MESSAGE    = VG_MESSAGE.
          WA_RETURN-MESSAGE_V1 = WA_ZMMT0098-RE_BELNR.
          WA_RETURN-MESSAGE_V2 = WA_ZMMT0098-GJAHR.
          PERFORM Z_PREPARA_MENSAGEM2  USING WA_ZMMT0098-OBJ_KEY
                                             WA_RETURN-TYPE
                                             WA_RETURN-MESSAGE
                                             WA_RETURN-MESSAGE_V1
                                             WA_RETURN-MESSAGE_V2
                                             WA_RETURN-MESSAGE_V3
                                             WA_ZMMT0098-ID_INTERFACE_MIR.
        ENDIF.
      ENDIF.
    ENDIF.

    MODIFY ZMMT0098 FROM WA_ZMMT0098.
    COMMIT WORK.
    CHECK WA_ZMMT0098-VG_ERRO_P IS INITIAL.

  ENDIF.

  WA_INVOICEDOCNUMBER-INV_DOC_NO = WA_ZMMT0098-RE_BELNR.
  WA_INVOICEDOCNUMBER-FISC_YEAR  = WA_ZMMT0098-GJAHR.
  WA_INVOICEDOCNUMBER-REASON_REV = WA_ZMMT0098-STGRD.

  WA_PSTNG_DATE = WA_ZMMT0098-BUDAT.

  SELECT SINGLE J_1BBRANCH
    INTO VL_WERKS
    FROM T001W
   WHERE WERKS EQ WA_ZMMT0098-WERKS.

  SET PARAMETER ID 'ZWERKS' FIELD VL_WERKS.

  CALL FUNCTION 'BAPI_INCOMINGINVOICE_CANCEL'
    EXPORTING
      INVOICEDOCNUMBER          = WA_INVOICEDOCNUMBER-INV_DOC_NO
      FISCALYEAR                = WA_INVOICEDOCNUMBER-FISC_YEAR
      REASONREVERSAL            = WA_INVOICEDOCNUMBER-REASON_REV
      POSTINGDATE               = WA_PSTNG_DATE
    IMPORTING
      INVOICEDOCNUMBER_REVERSAL = WA_ZMMT0098-BELNR
    TABLES
      RETURN                    = IT_RETURN.

  SET PARAMETER ID 'ZWERKS' FIELD ''.

  IF IT_RETURN[] IS NOT INITIAL.

    READ TABLE IT_RETURN INTO WA_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0.
      WA_ZMMT0098-VG_ERRO_P = 'X'.
      WA_ZMMT0098-VG_ERRO   = 'X'.
      MODIFY ZMMT0098 FROM WA_ZMMT0098.
      COMMIT WORK.
    ELSE.
      CLEAR : WA_ZMMT0098-VG_ERRO_P, WA_ZMMT0098-VG_ERRO.
      MODIFY ZMMT0098 FROM WA_ZMMT0098.
      COMMIT WORK.
    ENDIF.

  ELSE.
    CLEAR : WA_ZMMT0098-VG_ERRO_P, WA_ZMMT0098-VG_ERRO.
    MODIFY ZMMT0098 FROM WA_ZMMT0098.
    COMMIT WORK.
*Cria mensagem de retorno em caso de sucesso
    CLEAR WA_RETURN.
*Mensagem: O documento <nr_doc> para o exercício <ano>
*          foi estornado com sucesso.

    CONCATENATE 'O Documento' WA_INVOICEDOCNUMBER-INV_DOC_NO
                'para o exercício' WA_INVOICEDOCNUMBER-FISC_YEAR
                'foi estornado com sucesso.'
                INTO VG_MESSAGE
                SEPARATED BY SPACE.

    CLEAR: VL_MESSAGE_V1,
           VL_MESSAGE_V1.

    VL_MESSAGE_V1 = WA_INVOICEDOCNUMBER-INV_DOC_NO.
    VL_MESSAGE_V2 = WA_INVOICEDOCNUMBER-INV_DOC_NO.
    VL_MESSAGE_V3 = WA_INVOICEDOCNUMBER-FISC_YEAR.

    WA_RETURN-TYPE    = 'S'.
    WA_RETURN-ID      = 'MM'.
    WA_RETURN-NUMBER  = '899'.
    WA_RETURN-MESSAGE = VG_MESSAGE.
    WA_RETURN-MESSAGE_V1 = VL_MESSAGE_V1.
    WA_RETURN-MESSAGE_V2 = VL_MESSAGE_V2.
    WA_RETURN-MESSAGE_V3 = VL_MESSAGE_V3.
    APPEND WA_RETURN TO IT_RETURN.

    MODIFY ZMMT0098 FROM WA_ZMMT0098.
    COMMIT WORK.
  ENDIF.

  LOOP AT IT_RETURN INTO WA_RETURN.
    PERFORM Z_PREPARA_MENSAGEM2   USING WA_ZMMT0098-OBJ_KEY
                                       WA_RETURN-TYPE
                                       WA_RETURN-MESSAGE
                                       WA_RETURN-MESSAGE_V1
                                       WA_RETURN-MESSAGE_V2
                                       WA_RETURN-MESSAGE_V3
                                       WA_ZMMT0098-ID_INTERFACE_MIR.
  ENDLOOP.


FORM Z_PREPARA_MENSAGEM2  USING P_OBJ_KEY    TYPE ANY
                                P_TYPE       TYPE ANY
                                P_MESSAGE    TYPE ANY
                                P_MESSAGE_V1 TYPE ANY
                                P_MESSAGE_V2 TYPE ANY
                                P_MESSAGE_V3 TYPE ANY
                                P_INTERFACE  TYPE ANY.

  DATA: WA_ZOB_MENSAGEM TYPE ZOB_MENSAGEM_98.

  WA_ZOB_MENSAGEM-ID_REFKEY   = PREFKEY.
  WA_ZOB_MENSAGEM-OBJ_KEY        = P_OBJ_KEY.
  WA_ZOB_MENSAGEM-INTERFACE      = P_INTERFACE.
  WA_ZOB_MENSAGEM-DT_ATUALIZACAO = SY-DATUM.
  WA_ZOB_MENSAGEM-HR_ATUALIZACAO = SY-UZEIT.
  WA_ZOB_MENSAGEM-TYPE           = P_TYPE.
  WA_ZOB_MENSAGEM-ID             = 'MM'.
  WA_ZOB_MENSAGEM-NUM            = '899'.
  WA_ZOB_MENSAGEM-MESSAGE        = P_MESSAGE.
  WA_ZOB_MENSAGEM-MESSAGE_V1     = P_MESSAGE_V1.
  WA_ZOB_MENSAGEM-MESSAGE_V2     = P_MESSAGE_V2.
  WA_ZOB_MENSAGEM-MESSAGE_V3     = P_MESSAGE_V3.
  MODIFY ZOB_MENSAGEM_98 FROM WA_ZOB_MENSAGEM.
  COMMIT WORK.

ENDFORM.                    " Z_PREPARA_MENSAGEM2

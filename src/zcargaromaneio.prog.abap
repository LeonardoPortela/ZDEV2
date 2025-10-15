*&---------------------------------------------------------------------*
*& Report  ZCARGAROMANEIO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZCARGAROMANEIO.

DATA: C_X TYPE C         VALUE 'X',
      IT_ROMA        TYPE TABLE OF ZCARGA_ROMANEIO WITH HEADER LINE,
      IT_CIOT        TYPE TABLE OF ZCTE_CIOT WITH HEADER LINE,
      WA_ZSDT0001    TYPE ZSDT0001,
      WA_J_1BNFLIN   TYPE J_1BNFLIN,
      WA_ACT_NOTA    TYPE J_1BNFE_ACTIVE,
      WA_ZCTE_CIOT   TYPE ZCTE_CIOT,
      WA_ZCTE_VIAGEM TYPE ZCTE_VIAGEM,
      WA_INFO_CTE    type ZCARGA_ROM_CTE.

DATA: VIAGEM TYPE REF TO ZCL_CIOT_VIAGEM.


SELECTION-SCREEN BEGIN OF BLOCK ADDATA WITH FRAME TITLE TEXT-004.
PARAMETERS: CK_PROC TYPE C AS CHECKBOX,
            CK_CTE  TYPE C AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK ADDATA.


START-OF-SELECTION.

  CASE CK_CTE.
      "" CT-e """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    WHEN C_X.
      CASE CK_PROC.
          "Ajustar Número Doumento CT-e com seu número
        WHEN C_X.

          SELECT *
            INTO TABLE IT_ROMA
            FROM ZCARGA_ROMANEIO
           WHERE ENVIADO    EQ C_X
             AND CHAVE_NFE  NE ''
             AND ENVIADOCTE EQ ''.

          LOOP AT IT_ROMA.

            IF ( IT_ROMA-CTEDOCNUM GT 0 ).

              SELECT SINGLE * INTO WA_INFO_CTE FROM ZCARGA_ROM_CTE WHERE CHAVE_NFE EQ IT_ROMA-CHAVE_NFE.

              IF SY-SUBRC IS INITIAL.

                IT_ROMA-CTENUM           = WA_INFO_CTE-CTENUM.
                IT_ROMA-NUCONTRATO       = WA_INFO_CTE-NUCONTRATO.
                IT_ROMA-NR_CIOT          = WA_INFO_CTE-NR_CIOT.
                IT_ROMA-ID_OP_VIAGEM_ADM = WA_INFO_CTE-ID_OP_VIAGEM_ADM.
                IT_ROMA-LINK_CONTRATO    = WA_INFO_CTE-LINK_CONTRATO.
                IT_ROMA-LINK_RESUMO      = WA_INFO_CTE-LINK_RESUMO.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    INPUT  = IT_ROMA-CTENUM
                  IMPORTING
                    OUTPUT = IT_ROMA-CTENUM.

                UPDATE J_1BNFDOC
                   SET NFENUM = IT_ROMA-CTENUM
                 WHERE DOCNUM EQ IT_ROMA-CTEDOCNUM.

                UPDATE J_1BNFE_ACTIVE
                   SET NFNUM9 = IT_ROMA-CTENUM
                 WHERE DOCNUM EQ IT_ROMA-CTEDOCNUM.

                CALL FUNCTION 'Z_SD_INFO_CTE_AVULSO'
                  EXPORTING
                    P_CTE_AVULSO   = IT_ROMA-CTEDOCNUM
                    P_CHAMAR_TELA  = SPACE
                    P_GRAVAR_DADOS = C_X
                 TABLES
                   IT_CIOT         = IT_CIOT[].

                READ TABLE IT_CIOT INDEX 1.
                IF SY-SUBRC IS NOT INITIAL.
                  CONTINUE.
                ENDIF.

                SELECT SINGLE * INTO WA_ZCTE_CIOT FROM ZCTE_CIOT WHERE DOCNUM EQ IT_ROMA-CTEDOCNUM.
                IF SY-SUBRC IS NOT INITIAL.
                  CONTINUE.
                ENDIF.

                WA_ZCTE_CIOT-NUCONTRATO	       = IT_ROMA-NUCONTRATO.
                WA_ZCTE_CIOT-NR_CIOT   	       = IT_ROMA-NR_CIOT.
                WA_ZCTE_CIOT-ID_OP_VIAGEM_ADM  = IT_ROMA-ID_OP_VIAGEM_ADM.
                WA_ZCTE_CIOT-LINK_CONTRATO     = IT_ROMA-LINK_CONTRATO.
                WA_ZCTE_CIOT-LINK_RESUMO       = IT_ROMA-LINK_RESUMO.
                WA_ZCTE_CIOT-ST_CIOT           = '5'.
                MODIFY ZCTE_CIOT FROM WA_ZCTE_CIOT.

                CALL FUNCTION 'J_1B_NFE_SEND_C_NFE'
                  EXPORTING
                    IV_DOCNUM           = IT_ROMA-CTEDOCNUM
                  EXCEPTIONS
                    NOT_SENT            = 1
                    NOT_ALLOWED_TO_SEND = 2
                    OTHERS              = 3.

                IF SY-SUBRC <> 0.
                  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                ELSE.
                  IT_ROMA-ENVIADOCTE = C_X.
                  MODIFY ZCARGA_ROMANEIO FROM IT_ROMA.
                ENDIF.

              ENDIF.

            ENDIF.

          ENDLOOP.


          "Busca Chave NF-e gerada para pesquisar CT-e na Integração CT-e
        WHEN SPACE.
          "" Busca Registros NF-e Enviado p/ Pegar Chave e DOCNUM CTE """"""""""""""""""""""""""""""""""""""""""""""
          "" Nota deve estar Autorizada                               """"""""""""""""""""""""""""""""""""""""""""""
          "*********************************************************************************************************
          SELECT *
            INTO TABLE IT_ROMA
            FROM ZCARGA_ROMANEIO
           WHERE ENVIADO    EQ C_X
             AND CHAVE_NFE  EQ ''
             AND ENVIADOCTE EQ ''.
          "*********************************************************************************************************

          LOOP AT IT_ROMA.
            IF IT_ROMA-DOCNUM IS NOT INITIAL.

              "" Busca Chave NF-e """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              "*********************************************************************************************************
              SELECT SINGLE * INTO WA_ACT_NOTA
                FROM J_1BNFE_ACTIVE
               WHERE DOCNUM EQ IT_ROMA-DOCNUM
                 AND DOCSTA EQ '1'.

              IF SY-SUBRC IS INITIAL.
                CONCATENATE WA_ACT_NOTA-REGIO
                            WA_ACT_NOTA-NFYEAR
                            WA_ACT_NOTA-NFMONTH
                            WA_ACT_NOTA-STCD1
                            WA_ACT_NOTA-MODEL
                            WA_ACT_NOTA-SERIE
                            WA_ACT_NOTA-NFNUM9
                            WA_ACT_NOTA-DOCNUM9
                            WA_ACT_NOTA-CDV INTO IT_ROMA-CHAVE_NFE.
              ELSE.
                CONTINUE.
              ENDIF.
              "*********************************************************************************************************

              "" Busca DOCNUM CT-e Gerado """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              "*********************************************************************************************************
              CLEAR: WA_ZSDT0001.
              CONCATENATE IT_ROMA-CH_REFERENCIA '%' INTO WA_ZSDT0001-CH_REFERENCIA.

              SELECT SINGLE *
                INTO WA_ZSDT0001
                FROM ZSDT0001
               WHERE CH_REFERENCIA LIKE WA_ZSDT0001-CH_REFERENCIA.

              IF ( SY-SUBRC IS INITIAL ) AND ( WA_ZSDT0001-FATURA_FRETE IS NOT INITIAL ).

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    INPUT  = WA_ZSDT0001-FATURA_FRETE
                  IMPORTING
                    OUTPUT = WA_ZSDT0001-FATURA_FRETE.

                CONCATENATE WA_ZSDT0001-FATURA_FRETE '%' INTO WA_J_1BNFLIN-REFKEY.

                SELECT SINGLE * INTO WA_J_1BNFLIN
                  FROM J_1BNFLIN
                 WHERE REFTYP IN ('BI','MD')
                   AND REFKEY LIKE WA_J_1BNFLIN-REFKEY.

                IF ( SY-SUBRC IS INITIAL ) AND ( WA_J_1BNFLIN-DOCNUM IS NOT INITIAL ).
                  IT_ROMA-CTEDOCNUM = WA_J_1BNFLIN-DOCNUM.
                  MODIFY ZCARGA_ROMANEIO FROM IT_ROMA.
                ENDIF.

              ENDIF.
              "*********************************************************************************************************
            ENDIF.
          ENDLOOP.
      ENDCASE.
      "*******************************************************************************************************************


      "" Nota Fiscal """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "*******************************************************************************************************************
    WHEN SPACE.

      CASE CK_PROC.

        WHEN C_X.

          "" Busca Notas Enviadas p/ Autorização de Uso """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          "*********************************************************************************************************
          SELECT *
            INTO TABLE IT_ROMA
            FROM ZCARGA_ROMANEIO
           WHERE DOCNUM  NE ''
             AND ENVIADO EQ ''.

          LOOP AT IT_ROMA.

            CALL FUNCTION 'J_1B_NFE_SEND_C_NFE'
              EXPORTING
                IV_DOCNUM           = IT_ROMA-DOCNUM
              EXCEPTIONS
                NOT_SENT            = 1
                NOT_ALLOWED_TO_SEND = 2
                OTHERS              = 3.

            IF SY-SUBRC <> 0.
              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            ELSE.
              IT_ROMA-ENVIADO = C_X.
              MODIFY ZCARGA_ROMANEIO FROM IT_ROMA.
            ENDIF.

          ENDLOOP.
          "*********************************************************************************************************

        WHEN SPACE.

          "" Busca Notas Não Enviadas Bucar Ligação Romaneio/Nota """""""""""""""""""""""""""""""""""""""""""""""""""
          "*********************************************************************************************************
          SELECT *
            INTO TABLE IT_ROMA
            FROM ZCARGA_ROMANEIO
           WHERE ENVIADO EQ ''
             AND DOCNUM  EQ ''.

          LOOP AT IT_ROMA.

            CONCATENATE IT_ROMA-CH_REFERENCIA '%' INTO WA_ZSDT0001-CH_REFERENCIA.

            SELECT SINGLE *
              INTO WA_ZSDT0001
              FROM ZSDT0001
             WHERE CH_REFERENCIA LIKE WA_ZSDT0001-CH_REFERENCIA.

            IF ( SY-SUBRC IS INITIAL ) AND ( WA_ZSDT0001-DOC_REM IS NOT INITIAL ) AND ( WA_ZSDT0001-FATURA_PROD IS NOT INITIAL ).

              IT_ROMA-DOC_REM     = WA_ZSDT0001-DOC_REM.
              IT_ROMA-FATURA_PROD = WA_ZSDT0001-FATURA_PROD.
              MODIFY ZCARGA_ROMANEIO FROM IT_ROMA.

              CONCATENATE IT_ROMA-FATURA_PROD '%' INTO WA_J_1BNFLIN-REFKEY.

              SELECT SINGLE * INTO WA_J_1BNFLIN
                FROM J_1BNFLIN
               WHERE REFTYP IN ('BI','MD')
                 AND REFKEY LIKE WA_J_1BNFLIN-REFKEY.

              IF ( SY-SUBRC IS INITIAL ) AND ( WA_J_1BNFLIN-DOCNUM IS NOT INITIAL ).
                IT_ROMA-DOCNUM = WA_J_1BNFLIN-DOCNUM.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    INPUT  = IT_ROMA-NFENUM
                  IMPORTING
                    OUTPUT = IT_ROMA-NFENUM.

                UPDATE J_1BNFDOC
                   SET NFENUM = IT_ROMA-NFENUM
                 WHERE DOCNUM EQ WA_J_1BNFLIN-DOCNUM.

                UPDATE J_1BNFE_ACTIVE
                   SET NFNUM9 = IT_ROMA-NFENUM
                 WHERE DOCNUM EQ WA_J_1BNFLIN-DOCNUM.

                COMMIT WORK.
                MODIFY ZCARGA_ROMANEIO FROM IT_ROMA.
              ENDIF.

            ENDIF.

          ENDLOOP.
          "*********************************************************************************************************

      ENDCASE.
      "*******************************************************************************************************************
  ENDCASE.

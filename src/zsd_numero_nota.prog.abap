*&---------------------------------------------------------------------*
*& Report  ZSD_NUMERO_NOTA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZSD_NUMERO_NOTA.


*----------------------------------------------------------------------*
* Tabela Interna
*----------------------------------------------------------------------*
DATA: T_ZSD_NR_NOTA TYPE TABLE OF ZSD_NR_NOTA.


*----------------------------------------------------------------------*
* Work Area
*----------------------------------------------------------------------*
DATA: WA_ZSD_NR_NOTA TYPE ZSD_NR_NOTA.


PARAMETERS POBJKEY TYPE ZSD_NR_NOTA-OBJ_KEY NO-DISPLAY.

START-OF-SELECTION.
  PERFORM: ZSELECIONA_DADOS,
           ZPROCESSA.


*&---------------------------------------------------------------------*
*&      Form  ZSELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Seleciona Dados
*----------------------------------------------------------------------*
FORM ZSELECIONA_DADOS .

  DATA : VG_JOB         TYPE I,
         POBJKEY .

  REFRESH T_ZSD_NR_NOTA.

  SELECT SINGLE COUNT(*) INTO VG_JOB
    FROM TBTCO
   WHERE JOBNAME EQ 'NUMERO_NOTA_SIGAM'
     AND STATUS EQ 'R'.


  IF POBJKEY IS NOT INITIAL.

    SELECT *
      INTO TABLE T_ZSD_NR_NOTA
      FROM ZSD_NR_NOTA
     WHERE OBJ_KEY = POBJKEY.

  ELSEIF ( VG_JOB EQ 1 ). "and ( raw_enq[] is initial ).

    "Solicita numero obtido para uma posterior solicitação
    SELECT *
      INTO TABLE T_ZSD_NR_NOTA
      FROM ZSD_NR_NOTA
     WHERE ZRG_ATLZ = '0'.

  ENDIF.

ENDFORM.                    " ZSELECIONA_DADOS


*&---------------------------------------------------------------------*
*&      Form  ZPROCESSA
*&---------------------------------------------------------------------*
FORM ZPROCESSA .

  DATA: P_FORM     LIKE J_1BAA-FORM,
        P_SUBOBJ   LIKE J_1BB2-SUBOBJ,
        DOC_NUMBER LIKE  J_1BNFDOC-DOCNUM.

  LOOP AT T_ZSD_NR_NOTA INTO WA_ZSD_NR_NOTA.
    TRY.

        IF ( WA_ZSD_NR_NOTA-NFENUM IS INITIAL ) or ( WA_ZSD_NR_NOTA-NFENUM eq space ).

          WA_ZSD_NR_NOTA-DT_ATUALIZACAO     = SY-DATUM.
          WA_ZSD_NR_NOTA-HR_ATUALIZACAO     = SY-UZEIT.

          SELECT SINGLE FORM INTO P_FORM
            FROM J_1BAA
           WHERE NFTYPE EQ WA_ZSD_NR_NOTA-NFTYPE.

          IF SY-SUBRC EQ 0.

            SELECT SINGLE SUBOBJ
              INTO P_SUBOBJ
              FROM J_1BB2
             WHERE BUKRS  EQ WA_ZSD_NR_NOTA-BUKRS
               AND BRANCH EQ WA_ZSD_NR_NOTA-BRANCH
               AND FORM   EQ P_FORM.

            IF SY-SUBRC EQ 0.

              SELECT SINGLE SUBOBJECT
                INTO P_SUBOBJ
                FROM NRIV
               WHERE OBJECT    = 'J_1BNFENUM'
                 AND SUBOBJECT = P_SUBOBJ
                 AND NRRANGENR = '55'
                 AND TOYEAR    = '0000'.

              IF SY-SUBRC EQ 0.

                CALL FUNCTION 'NUMBER_GET_NEXT'
                  EXPORTING
                    NR_RANGE_NR             = '55'
                    OBJECT                  = 'J_1BNFENUM'
                    SUBOBJECT               = P_SUBOBJ
                  IMPORTING
                    NUMBER                  = DOC_NUMBER
                  EXCEPTIONS
                    INTERVAL_NOT_FOUND      = 1
                    NUMBER_RANGE_NOT_INTERN = 2
                    OBJECT_NOT_FOUND        = 3
                    QUANTITY_IS_0           = 4
                    QUANTITY_IS_NOT_1       = 5
                    INTERVAL_OVERFLOW       = 6
                    BUFFER_OVERFLOW         = 7
                    OTHERS                  = 8.

                IF SY-SUBRC EQ 0.
                  WA_ZSD_NR_NOTA-NFENUM         = DOC_NUMBER.
                ELSE.

                  IF SY-SUBRC EQ 1.
                    CONCATENATE 'Call function: Intervalo não encontrado:' 'Objeto: J_1BNFENUM' 'Range: 02' 'SubObejto:' P_SUBOBJ INTO WA_ZSD_NR_NOTA-MSG_LOG SEPARATED BY SPACE.
                  ELSEIF SY-SUBRC EQ 2.
                    CONCATENATE 'Call function: Intervalo não é interno:' 'Objeto: J_1BNFENUM' 'Range: 02' 'SubObejto:' P_SUBOBJ  INTO WA_ZSD_NR_NOTA-MSG_LOG SEPARATED BY SPACE.
                  ELSEIF SY-SUBRC EQ 3.
                    CONCATENATE 'Call function: Objeto não encontrato:' 'Objeto: J_1BNFENUM' 'Range: 02' 'SubObejto:' P_SUBOBJ    INTO WA_ZSD_NR_NOTA-MSG_LOG SEPARATED BY SPACE.
                  ELSEIF SY-SUBRC EQ 4.
                    CONCATENATE 'Call function: Quantidade é zero (0):' 'Objeto: J_1BNFENUM' 'Range: 02' 'SubObejto:' P_SUBOBJ    INTO WA_ZSD_NR_NOTA-MSG_LOG SEPARATED BY SPACE.
                  ELSEIF SY-SUBRC EQ 5.
                    CONCATENATE 'Call function: Quantidade não é um (1):' 'Objeto: J_1BNFENUM' 'Range: 02' 'SubObejto:' P_SUBOBJ  INTO WA_ZSD_NR_NOTA-MSG_LOG SEPARATED BY SPACE.
                  ELSEIF SY-SUBRC EQ 6.
                    CONCATENATE 'Call function: Intervalo overflow:' 'Objeto: J_1BNFENUM' 'Range: 02' 'SubObejto:' P_SUBOBJ       INTO WA_ZSD_NR_NOTA-MSG_LOG SEPARATED BY SPACE.
                  ELSEIF SY-SUBRC EQ 7.
                    CONCATENATE 'Call function: Buffer overflow:' 'Objeto: J_1BNFENUM' 'Range: 02' 'SubObejto:' P_SUBOBJ          INTO WA_ZSD_NR_NOTA-MSG_LOG SEPARATED BY SPACE.
                  ELSEIF SY-SUBRC EQ 8.
                    CONCATENATE 'Call function: Outros Erros:' 'Objeto: J_1BNFENUM' 'Range: 02' 'SubObejto:' P_SUBOBJ             INTO WA_ZSD_NR_NOTA-MSG_LOG SEPARATED BY SPACE.
                  ENDIF.

                ENDIF.
              ELSE.

                CONCATENATE 'Não foi encontrato intervalo de numeração para:'
                            'Objeto: J_1BNFENUM'
                            'Range: 02'
                            'SubObejto:' P_SUBOBJ
                       INTO WA_ZSD_NR_NOTA-MSG_LOG SEPARATED BY SPACE.

              ENDIF.
            ELSE.
              CONCATENATE 'Não localizado Grupo nºs nota fiscal (interv.numeração)'
                          'Empresa:'    WA_ZSD_NR_NOTA-BUKRS
                          'Filial:'     WA_ZSD_NR_NOTA-BRANCH
                          'Formulário:' P_FORM
                     INTO WA_ZSD_NR_NOTA-MSG_LOG SEPARATED BY SPACE.

            ENDIF.

          ELSE.
            CONCATENATE 'Não foi localizado formulario para a categoria'  WA_ZSD_NR_NOTA-NFTYPE INTO WA_ZSD_NR_NOTA-MSG_LOG SEPARATED BY SPACE.
          ENDIF.

        ENDIF.

        WA_ZSD_NR_NOTA-ZRG_ATLZ           = '1'.
        WA_ZSD_NR_NOTA-ZRG_ATLZ_INTERFACE = '0'.
      CATCH CX_ROOT.
        WA_ZSD_NR_NOTA-MSG_LOG = 'Erro Genérico ao buscar o numero da nota'.
    ENDTRY.

    MODIFY ZSD_NR_NOTA FROM WA_ZSD_NR_NOTA.

    CLEAR WA_ZSD_NR_NOTA.

  ENDLOOP.

  COMMIT WORK.

ENDFORM.                    " ZSELECIONA_DADOS

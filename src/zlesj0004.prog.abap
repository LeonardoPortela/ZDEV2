************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
************************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                             *
* Data desenv ...: 14.11.2011                                                      *
* Objetivo    ...: Job de Validação - Comparativo de Saída e Chegada Ferroviário   *
************************************************************************************

REPORT  ZLESJ0004.
*----------------------------------------------------------------------*
* ESTRUTURA DAS TABELAS
*----------------------------------------------------------------------*

TYPES:

  BEGIN OF TY_ZLEST0019,
    IDINTER      TYPE ZLEST0019-IDINTER,
    TP_MOVI      TYPE ZLEST0019-TP_MOVI,
    TP_REG       TYPE ZLEST0019-TP_REG,
    IDVAGAO      TYPE ZLEST0019-IDVAGAO,
    PESOVAGAO    TYPE ZLEST0019-PESOVAGAO,
    DTADECARGA   TYPE ZLEST0019-DTADECARGA,
    HORADESCARGA TYPE ZLEST0019-HORADESCARGA,
  END OF TY_ZLEST0019.

*----------------------------------------------------------------------*
* TABELAS INTERNAS
*----------------------------------------------------------------------*
DATA: IT_ZLEST0019            TYPE TABLE OF TY_ZLEST0019,
      IT_ZLEST0019_DUPLICACAO TYPE TABLE OF TY_ZLEST0019.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WA_ZLEST0019            TYPE TY_ZLEST0019,
      WA_ZLEST0019_DUPLICACAO TYPE TY_ZLEST0019.

*----------------------------------------------------------------------*
* PROCESSAMENTO
*----------------------------------------------------------------------*
DATA: QTD TYPE P.
SELECT SINGLE COUNT(*) INTO @DATA(VG_JOB)
    FROM TBTCO
   WHERE JOBNAME EQ 'ATUALIZA_COMP_J4'
     AND STATUS EQ 'R'.

IF ( VG_JOB EQ 1 ).
  SELECT IDINTER TP_MOVI TP_REG IDVAGAO PESOVAGAO DTADECARGA HORADESCARGA
    FROM ZLEST0019
    INTO TABLE IT_ZLEST0019
  WHERE IDINTER        EQ 'L2'
    AND TP_MOVI        EQ 'S'
    AND TP_REG         EQ '20'
    AND IDVAGAO        NE SPACE
    AND PESOVAGAO      NE SPACE
    AND DTADECARGA     NE SPACE
    AND STATUS_DUPLICA EQ SPACE.

  IF ( SY-SUBRC EQ 0 ).
    PERFORM: VALIDA_DUPLICACAO.
  ENDIF.
ENDIF.

*&---------------------------------------------------------------------*
*&      Form  VALIDA_DUPLICACAO
*&---------------------------------------------------------------------*
FORM VALIDA_DUPLICACAO .

  SORT: IT_ZLEST0019 BY IDVAGAO DTADECARGA.

  LOOP AT IT_ZLEST0019 INTO WA_ZLEST0019.
    APPEND WA_ZLEST0019 TO IT_ZLEST0019_DUPLICACAO.
  ENDLOOP.

  SORT: IT_ZLEST0019 BY IDVAGAO DTADECARGA.

  LOOP AT IT_ZLEST0019 INTO WA_ZLEST0019.
    QTD = 0.
    LOOP AT IT_ZLEST0019_DUPLICACAO INTO WA_ZLEST0019_DUPLICACAO WHERE IDVAGAO EQ WA_ZLEST0019-IDVAGAO
                                                                 AND DTADECARGA EQ WA_ZLEST0019-DTADECARGA.
      QTD = QTD + 1.
      IF ( QTD >= 2 ).

        UPDATE ZLEST0019 SET STATUS_DUPLICA = 2 WHERE IDVAGAO    EQ WA_ZLEST0019_DUPLICACAO-IDVAGAO
                                                  AND DTADECARGA EQ WA_ZLEST0019_DUPLICACAO-DTADECARGA.

      ENDIF.

      IF ( QTD <= 1 ).
        UPDATE ZLEST0019 SET STATUS_DUPLICA = 1 WHERE IDVAGAO    EQ WA_ZLEST0019_DUPLICACAO-IDVAGAO
                                                  AND DTADECARGA EQ WA_ZLEST0019_DUPLICACAO-DTADECARGA.

      ENDIF.
    ENDLOOP.
    CLEAR: WA_ZLEST0019, WA_ZLEST0019_DUPLICACAO.
  ENDLOOP.
ENDFORM.                    " VALIDA_DUPLICACAO

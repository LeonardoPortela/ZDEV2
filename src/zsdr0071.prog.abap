*&---------------------------------------------------------------------*
*& Report  ZSDR0071
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZSDR0071.





***********************************************************************
* TABELAS Internas
***********************************************************************
DATA: IT_ZSDT0145    TYPE TABLE OF ZSDT0145,
      IT_IB_ROMANEIO TYPE TABLE OF ZSDS001.

***********************************************************************
* WORK AREAS
***********************************************************************
DATA: WA_ZSDT0145    TYPE ZSDT0145,
      WA_IB_ROMANEIO TYPE ZSDS001.


***********************************************************************
* VARIÁVEIS
***********************************************************************



*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
DATA: VG_JOB TYPE I.

SELECT SINGLE COUNT(*) INTO VG_JOB
  FROM TBTCO
 WHERE JOBNAME EQ 'ROMANEIO_CONT_SIGAM'
   AND STATUS EQ 'R'.

IF ( VG_JOB EQ 1 ).

  PERFORM : SELECIONA_DADOS,
            PROCESSA.

ENDIF.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS .

  REFRESH: IT_ZSDT0145.

  SELECT *
    FROM ZSDT0145
    INTO TABLE IT_ZSDT0145
   WHERE ZRG_ATLZ EQ 'N'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESSA .

  "CLEAR WA_ZSDT0145.

  "MOVE-CORRESPONDING : IT_ZSDT0145[] to IT_IB_ROMANEIO[].

  LOOP AT IT_ZSDT0145 INTO WA_ZSDT0145.

    MOVE-CORRESPONDING WA_ZSDT0145 TO WA_IB_ROMANEIO.
    "MOVE WA_ZSDT0145 INTO CORRESPONDING  WA_IB_ROMANEIO .


    APPEND WA_IB_ROMANEIO TO IT_IB_ROMANEIO.

    WA_ZSDT0145-ZRG_ATLZ = 'S'.

    MODIFY ZSDT0145 FROM WA_ZSDT0145.

    CLEAR WA_ZSDT0145.
  ENDLOOP.

  CALL FUNCTION 'ZSD_INBOUND_REMESSA'
    TABLES
      IB_ROMANEIO = IT_IB_ROMANEIO.

  COMMIT WORK.


ENDFORM.

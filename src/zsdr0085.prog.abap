*&---------------------------------------------------------------------*
*& Report  ZSDR0085
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZSDR0085.

DATA: TG_ZCCE TYPE TABLE OF ZCARTA_CORRECAO WITH HEADER LINE.

INITIALIZATION.

  DATA: VG_JOB  TYPE I.

  SELECT SINGLE COUNT( * ) INTO VG_JOB
    FROM TBTCO
   WHERE JOBNAME EQ 'ZSDR0085_JOB'
     AND STATUS EQ 'R'.

  IF ( VG_JOB EQ 1 ).
    PERFORM: F_SELECIONA_DADOS,
             F_PROCESSA_DADOS.
  ENDIF.


 FORM F_SELECIONA_DADOS.

   CLEAR: TG_ZCCE[].

   SELECT *
     FROM ZCARTA_CORRECAO INTO TABLE TG_ZCCE
    WHERE TRANSF_AUTO       EQ 'X'
      AND TRANSF_AUTO_EXEC  EQ ''
      AND AUTHCODE          NE ''.

 ENDFORM.

 FORM F_PROCESSA_DADOS.

   DATA: ZCL_CCE TYPE REF TO ZCL_CCE.

   LOOP AT TG_ZCCE.

     TG_ZCCE-TRANSF_AUTO_EXEC = 'X'.
     MODIFY ZCARTA_CORRECAO FROM TG_ZCCE.
     COMMIT WORK.

     FREE ZCL_CCE.
     CREATE OBJECT ZCL_CCE
       EXPORTING
        I_DOCNUM = TG_ZCCE-DOCNUM
        I_ID_CC  = TG_ZCCE-ID_CC.

     CALL METHOD ZCL_CCE->TRANSF_ESTOQUE
       EXPORTING
         I_TP_TRANSF        = TG_ZCCE-TP_TRANSF
         I_CENTRO_ORIGEM    = TG_ZCCE-WERKS_O
         I_DEPOSITO_ORIGEM  = TG_ZCCE-LGORT_O
         I_CENTRO_DESTINO   = TG_ZCCE-WERKS_D
         I_DEPOSITO_DESTINO = TG_ZCCE-LGORT_D
         I_JOB              = 'X'
       RECEIVING
         E_GRAVOU           = DATA(_GRAVOU).

   ENDLOOP.

 ENDFORM.

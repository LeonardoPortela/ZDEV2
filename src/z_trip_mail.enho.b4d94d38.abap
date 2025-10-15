"Name: \PR:SAPMFITP\FO:SAVE_REQUEST\SE:END\EI
ENHANCEMENT 0 Z_TRIP_MAIL.
 data: WA_ZFIT0008  type ZFIT0008,
        BSMTP_ADDR   TYPE ADR6-SMTP_ADDR,
        W_CAMPO(40).

* Objetos para enviar email
  DATA: OBJPACK     LIKE SOPCKLSTI1 OCCURS  2 WITH HEADER LINE.
  DATA: OBJHEAD     LIKE SOLISTI1   OCCURS  1 WITH HEADER LINE.
  DATA: OBJBIN_ORD  LIKE SOLISTI1   OCCURS 10 WITH HEADER LINE.
  DATA: OBJBIN_LOG  LIKE SOLISTI1   OCCURS 10 WITH HEADER LINE.
  DATA: OBJBIN_ANN  TYPE SOLISTI1.
  DATA: OBJBIN      LIKE SOLISTI1   OCCURS 10 WITH HEADER LINE,
        OBJBIN1     TYPE SOLI_TAB, "   OCCURS 10 WITH HEADER LINE.
  WA_OBJBIN   LIKE LINE OF OBJBIN.
  DATA: CONTENT_HEX TYPE STANDARD TABLE OF SOLIX WITH HEADER LINE.
  DATA: OBJTXT      LIKE SOLISTI1   OCCURS 10 WITH HEADER LINE.
  DATA: RECLIST     LIKE SOMLRECI1  OCCURS  5 WITH HEADER LINE.
  DATA: DOC_CHNG    LIKE SODOCCHGI1.
  DATA: TAB_LINES   LIKE SY-TABIX.
  DATA: L_ANEX      TYPE STRING.
  DATA: L_LENG      TYPE I.
  DATA: L_ARQ       TYPE STRING.
  DATA: L_TAM       TYPE I.
  DATA: L_TAM_ORD   TYPE I.
  DATA: L_TAM_LOG   TYPE I.
  DATA: L_EMAIL(300) TYPE C.
  DATA: VLINHA      TYPE I.
  DATA: VUSER       TYPE SY-UNAME.
  DATA: IT_SHORTCUT_PARAM LIKE ZST_SHORTCUT_PAR OCCURS 0 WITH HEADER LINE.
  DATA: CONTENT TYPE STRING.

*  ** Pass the required parameters and create the shortcut
  CLEAR IT_SHORTCUT_PARAM.
  REFRESH IT_SHORTCUT_PARAM.


  FIELD-SYMBOLS: <VKOSTL> TYPE ANY,
                 <VNAME> type ANY,
                 <DATE_BEG> type any,
                 <DATE_END> type any.

  if T_REQ_ADVANCE[] is not INITIAL.
*   W_CAMPO = '(SAPLHRTRV_UTIL)BAPI0012_2-COSTCENTER'.
*   ASSIGN (W_CAMPO) TO <VKOSTL>.
*
*   W_CAMPO = '(SAPLHRTRV_UTIL)ADDR3_VAL-NAME_TEXT'.
*   ASSIGN  (W_CAMPO) TO <VNAME>.

   W_CAMPO = '(SAPMFITP)4100_DATE_BEG'.
   ASSIGN (W_CAMPO) TO <DATE_BEG>.

   W_CAMPO = '(SAPMFITP)4100_DATE_END'.
   ASSIGN (W_CAMPO) TO <DATE_END>.



   select single *
     from ZFIT0008
     into wa_ZFIT0008
     where KOSTL eq P0001-KOSTL.

   if sy-subrc = 0.
       SELECT SINGLE ADR6~SMTP_ADDR INTO BSMTP_ADDR
         FROM USR21
           INNER JOIN ADR6
              ON  USR21~ADDRNUMBER = ADR6~ADDRNUMBER
             AND USR21~PERSNUMBER = ADR6~PERSNUMBER
                 WHERE USR21~BNAME = wa_ZFIT0008-APROVADOR.
        if sy-subrc = 0.

*      Criação do documento de Email
       DOC_CHNG-OBJ_NAME = 'LOG_TRIP'.

*      Assunto do Email
       DOC_CHNG-OBJ_DESCR = 'Aprovação Adiantamento de Viagem'.
*      Texto
       OBJTXT-LINE = 'Esta disponível no SAP transação PR05 ou clicando no link em anexo PR05,'.
       APPEND OBJTXT.
       CLEAR OBJTXT.
       APPEND OBJTXT.

       OBJTXT-LINE = 'a solicitação de adiantamento para viagem conforme abaixo :' .
       APPEND OBJTXT.
       CLEAR OBJTXT.

       OBJTXT-LINE = '-------------------------------------------------------------------------------------------------------' .
       APPEND OBJTXT.
       CLEAR OBJTXT.

       READ TABLE T_REQ_ADVANCE INTO WA_REQ_ADVANCE INDEX 1.

       CONCATENATE 'Nro.Pessoal    = ' wa_head-PERNR into OBJTXT SEPARATED BY space.
       APPEND OBJTXT.
       CLEAR OBJTXT.

       CONCATENATE 'Colaborador    = '  P0002-CNAME into OBJTXT SEPARATED BY space.
       APPEND OBJTXT.
       CLEAR OBJTXT.

       CONCATENATE 'Nro. Viagem    = ' WA_REQ_ADVANCE-REINR into OBJTXT SEPARATED BY space.
       APPEND OBJTXT.
       CLEAR OBJTXT.

       data vdata(10).
       CONCATENATE <DATE_BEG>+6(2) <DATE_BEG>+4(2) <DATE_BEG>+0(4) INTO VDATA SEPARATED BY '.'.
       CONCATENATE 'Data da Viagem = ' vdata into OBJTXT SEPARATED BY space.
       CONCATENATE <DATE_END>+6(2) <DATE_END>+4(2) <DATE_END>+0(4) INTO VDATA SEPARATED BY '.'.
       CONCATENATE OBJTXT 'a' vdata into OBJTXT SEPARATED BY space.
       APPEND OBJTXT.
       CLEAR OBJTXT.


       CONCATENATE wa_req_advance-datvs+6(2) wa_req_advance-datvs+4(2) wa_req_advance-datvs+0(4) INTO VDATA SEPARATED BY '.'.
       CONCATENATE 'Data do Adto   = ' vdata into OBJTXT SEPARATED BY space.
       APPEND OBJTXT.
       CLEAR OBJTXT.

       CONCATENATE 'Moeda          = ' wa_req_advance-waers into OBJTXT SEPARATED BY space.
       APPEND OBJTXT.
       CLEAR OBJTXT.


*      Setar tamanho da mensagem
       DESCRIBE TABLE OBJTXT LINES TAB_LINES.
       READ TABLE OBJTXT INDEX TAB_LINES.
       DOC_CHNG-DOC_SIZE = ( TAB_LINES - 1 ) * 255 + STRLEN( OBJTXT ).

*      Criar entrada de documento comprimido
       CLEAR OBJPACK-TRANSF_BIN.
       "OBJPACK-TRANSF_BIN = 'X'.
       OBJPACK-HEAD_START = 1.
       OBJPACK-HEAD_NUM   = 0.
       OBJPACK-BODY_START = 1.
       OBJPACK-BODY_NUM   = TAB_LINES.
       OBJPACK-DOC_TYPE   = 'RAW'.
       APPEND OBJPACK.

       CALL FUNCTION 'ZFM_CREATE_SHORTCUT'
         EXPORTING
           RECIPIENT_USER_ID = wa_ZFIT0008-APROVADOR
           TRANSACTION       = 'PR05'
         IMPORTING
           CONTENT           = CONTENT
         TABLES
           SHORTCUT_PARAM    = IT_SHORTCUT_PARAM.

       CLEAR : TAB_LINES, OBJBIN.
       CONCATENATE CONTENT WA_OBJBIN-LINE INTO WA_OBJBIN-LINE.
       APPEND  WA_OBJBIN TO OBJBIN.

       DESCRIBE TABLE OBJBIN LINES TAB_LINES.
       OBJHEAD = 'PR05.SAP'.
       APPEND OBJHEAD.

*      * Creation of the entry for the compressed attachment
        OBJPACK-TRANSF_BIN = 'X'.
        OBJPACK-HEAD_START = 1.
        OBJPACK-HEAD_NUM   = 1.
        OBJPACK-BODY_START = 1.
        OBJPACK-BODY_NUM   = TAB_LINES.
        OBJPACK-DOC_TYPE   = 'EXT'." SAP
        OBJPACK-OBJ_NAME   = 'SAPSHORTCUTMAIL'.
        OBJPACK-OBJ_DESCR  = 'PR05.SAP'.
        OBJPACK-DOC_SIZE   = TAB_LINES * 255.
        APPEND OBJPACK.

*      Alimentar destinatários do email
       IF BSMTP_ADDR IS INITIAL.
         MESSAGE 'O aprovador seguinte não tem e-mail cadastrado, por favor contacte a T.I.' TYPE 'I'.
         EXIT.
       ENDIF.

       RECLIST-RECEIVER = BSMTP_ADDR.
       RECLIST-REC_TYPE = 'U'.                    "Define email externo
       APPEND RECLIST.

*      Enviar email
       VUSER = SY-UNAME.
       SY-UNAME = 'R3JOB'.
       CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
         EXPORTING
           DOCUMENT_DATA              = DOC_CHNG
           PUT_IN_OUTBOX              = 'X'
           COMMIT_WORK                = 'X'
         TABLES
           PACKING_LIST               = OBJPACK
           OBJECT_HEADER              = OBJHEAD
           CONTENTS_BIN               = OBJBIN
           CONTENTS_TXT               = OBJTXT      "CONTENTS_HEX = CONTENT_HEX
           RECEIVERS                  = RECLIST
         EXCEPTIONS
           TOO_MANY_RECEIVERS         = 1
           DOCUMENT_NOT_SENT          = 2
           OPERATION_NO_AUTHORIZATION = 4
           OTHERS                     = 99.

       SY-UNAME = VUSER.
*       CASE sy-subrc.
*         WHEN 0.
*           WRITE: / 'Result of the send process:'.
*
*           LOOP AT reclist.
*             WRITE: / reclist-receiver(48), ':'.
*
*             IF reclist-retrn_code = 0.
*               WRITE 'The document was sent'.
*             ELSE.
*               WRITE 'The document could not be sent'.
*             ENDIF.
*
*           ENDLOOP.
*
*         WHEN 1.
*           WRITE: / 'No authorization for sending to the specified number',
*                    'of recipients'.
*
*         WHEN 2.
*           WRITE: / 'Document could not be sent to any recipient'.
*
*         WHEN 4.
*           WRITE: / 'No send authorization'.
*
*         WHEN OTHERS.
*           WRITE: / 'Error occurred while sending'.
*
*       ENDCASE.

        endif.
   endif.
 endif.



ENDENHANCEMENT.

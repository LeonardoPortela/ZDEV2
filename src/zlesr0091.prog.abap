*/===========================================================================\*
*|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
*|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
*|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
*|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
*|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
*|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
*| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
*/===========================================================================\*

*/===========================================================================\*
*| Descrição:                                                                |*
*| Este é um JOB (serviço) para encerramento de MDF-e(s)                     |*
*|                                                                           |*
*/===========================================================================\*

REPORT  ZLESR0091.

**----------------------------------------------------------------------*
** Objetos para enviar email
**----------------------------------------------------------------------*

DATA: OBJPACK     LIKE SOPCKLSTI1 OCCURS  2 WITH HEADER LINE.
DATA: OBJHEAD     LIKE SOLISTI1   OCCURS  1 WITH HEADER LINE.
DATA: OBJBIN_ORD  LIKE SOLISTI1   OCCURS 10 WITH HEADER LINE.
DATA: OBJBIN_LOG  LIKE SOLISTI1   OCCURS 10 WITH HEADER LINE.
DATA: OBJBIN_ANN  TYPE SOLISTI1.
DATA: OBJBIN      LIKE SOLISTI1   OCCURS 10 WITH HEADER LINE,
      WA_OBJBIN   LIKE LINE OF OBJBIN.
DATA: CONTENT_HEX TYPE STANDARD TABLE OF SOLIX WITH HEADER LINE.
DATA: OBJTXT      LIKE SOLISTI1   OCCURS 10 WITH HEADER LINE.
DATA: RECLIST     LIKE SOMLRECI1  OCCURS  5 WITH HEADER LINE.
DATA: DOC_CHNG    LIKE SODOCCHGI1.
DATA: TAB_LINES   LIKE SY-TABIX.
DATA: IT_HTML     TYPE TABLE OF W3HTML INITIAL SIZE 0 WITH HEADER LINE.

**----------------------------------------------------------------------*
** Objetos
**----------------------------------------------------------------------*
DATA: ZCL_MDFE TYPE REF TO ZCL_MDFE.

**----------------------------------------------------------------------*
** TABELAS INTERNAS AND WORK AREA
**----------------------------------------------------------------------*

DATA: IT_ZSDT0102 TYPE TABLE OF ZSDT0102,
      WA_ZSDT0102 TYPE ZSDT0102,
      IT_ZSDT0102_AUX TYPE TABLE OF ZSDT0102,
      WA_ZSDT0102_AUX TYPE ZSDT0102,
      IT_ZSDT0105 TYPE TABLE OF ZSDT0105,
      WA_ZSDT0105 TYPE ZSDT0105.

**----------------------------------------------------------------------*
** PROCESSAMENTO
**----------------------------------------------------------------------*

* Atribuição de Emails
 REFRESH RECLIST.

 SELECT *
   FROM TVARVC INTO TABLE @DATA(LIT_EMAILS)
  WHERE NAME EQ 'ZLESR0091_EMAILS'.

 LOOP AT LIT_EMAILS INTO DATA(LWA_EMAIL).
   TRANSLATE LWA_EMAIL-low TO LOWER CASE.
   RECLIST-RECEIVER = LWA_EMAIL-low.
   RECLIST-REC_TYPE = 'U'.
   APPEND RECLIST.
 ENDLOOP.

* 1º Etapa ----------------------------------------------------------*
* Nessa etapa, é solicitado encerramento para todos os MDF-e(s)
* para qual já houve email de aviso de encerramento automático
* na etapa 2.
* -------------------------------------------------------------------*
PERFORM ENC_MDFE_C_AVISO.

* 2º Etapa ----------------------------------------------------------*
* Nessa etapa, é enviado um email de aviso de encerramento automático
* para todos os MDF-e(s) que:
*  1º Não foram encerrados no prazo de 10 dias a contar da data de autorização.
*  2º MDF-e que já possui CT-e vinculado à uma NF-e com L1
* -------------------------------------------------------------------*
PERFORM EMAIL_AVISO_ENC.

* 3º Etapa ----------------------------------------------------------*
* Nessa etapa, é enviado um email de aviso de alerta para todos
* os MDF-e(s) que não estão sendo encerrados automaticamente(por alguma
* reijeição/erro) no prazo de 10 dias a contar da data de autorização.
* -------------------------------------------------------------------*
PERFORM EMAIL_AVISO_ALERTA.

*&---------------------------------------------------------------------*
*&      Form  EMAIL_AVISO_ENC
*&---------------------------------------------------------------------*
FORM EMAIL_AVISO_ENC.

  DATA: VL_DATA_LIM   TYPE D,
        VL_DATA_AVISO TYPE D,
        VL_LINES      TYPE I,
        VL_DATA_PREV  TYPE D,
        VL_HORA_PREV  TYPE STRING,
        VL_DATA       TYPE STRING,
        VL_DHPREV     TYPE STRING.

  DATA: VL_NMDFE       TYPE STRING,
        VL_DOCNUM      TYPE STRING,
        VL_DOC_CTE     TYPE STRING,
        VL_DATA_AUT    TYPE STRING,
        VL_TITULO      TYPE STRING,
        VL_MSG_RET     TYPE STRING.

  DEFINE CONC_HTML.
    CALL FUNCTION 'ZHTML_ADD'
      EXPORTING
        I_TEXTO = &1
      TABLES
        IT_HTML = IT_HTML.
  END-OF-DEFINITION.

  REFRESH: IT_ZSDT0102, IT_HTML.
  CLEAR: WA_ZSDT0102, VL_LINES.

  "Não foram encerrados no prazo de 10 dias a contar da data de autorização.
  PERFORM GET_ENC_10_DIAS_AUTH TABLES IT_ZSDT0102.

  "Notas que já possuem L1
  PERFORM GET_ENC_DESCARGA_L1 TABLES IT_ZSDT0102.

  "Notas Entrada Transferencia.
  PERFORM GET_ENC_DESCARGA_TRANSF TABLES IT_ZSDT0102.

  CHECK IT_ZSDT0102[] IS NOT INITIAL.

  LOOP AT IT_ZSDT0102 INTO WA_ZSDT0102.
    WA_ZSDT0102-AVISO_ENC_AUT = 'X'.
    MODIFY ZSDT0102 FROM WA_ZSDT0102.
    CLEAR: WA_ZSDT0102.
  ENDLOOP.

  SELECT SINGLE *
    FROM SETLEAF INTO @DATA(LWA_SET_ENV_EMAIL_01)
   WHERE SETNAME EQ 'ZCL_MDFE'
     AND VALFROM EQ 'EMAIL_ENC_002'.

  CHECK SY-SUBRC EQ 0.

  VL_DATA_PREV = SY-DATUM + 1.

  VL_HORA_PREV = 'às 23:00'.
  CONCATENATE VL_DATA_PREV+6(2) '/' VL_DATA_PREV+4(2) '/' VL_DATA_PREV(4) INTO VL_DATA.
  CONCATENATE VL_DATA VL_HORA_PREV INTO VL_DHPREV SEPARATED BY SPACE.

  CLEAR: VL_TITULO.
  CONCATENATE 'MDF-e(s) com previsão de encerramento automático dia:' VL_DHPREV INTO VL_TITULO SEPARATED BY SPACE.

  "Monta Corpo Email
  CONC_HTML '<html>'.
  CONC_HTML '<head><title>Encerramento Automático MDF-e</title><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1"></head>'.
  CONC_HTML '<body bgcolor="#f5f1ff" leftmargin="0" topmargin="0" marginwidth="0" marginheight="0">'.
  CONC_HTML '<DIV align=center><FONT face=Verdana color=#ff0000 size=4><STRONG>'.
  CONC_HTML VL_TITULO.
  CONC_HTML '</STRONG></FONT></DIV><BR>'.
  CONC_HTML '<FONT face=Verdana color=#0000ff size=2>'.
  CONC_HTML '<BR>'.

  CONC_HTML '<table cellspacing="0" border="1" bordercolor="FFFFFF" width="100%">'.

  CONC_HTML    '<tr bgcolor="B0C4DE">'.
  CONC_HTML       '<td width="20%"><p align="center"><font color="#000030" size=1><b>Num. MDF-e</b></font></p></td>'.
  CONC_HTML       '<td width="20%"><p align="center"><font color="#000030" size=1><b>Doc. Num. MDF-e </b></font></p></td>'.
  CONC_HTML       '<td width="20%"><p align="center"><font color="#000030" size=1><b>Data Autorização</b></font></p></td>'.
  CONC_HTML       '<td width="30%"><p align="center"><font color="#000030" size=1><b>Msg. Retorno</b></font></p></td>'.
  CONC_HTML       '<td width="20%"><p align="center"><font color="#000030" size=1><b>CT-e(s) Vinculados</b></font></p></td>'.
  CONC_HTML    '</tr>'.

  LOOP AT IT_ZSDT0102 INTO WA_ZSDT0102.

    REFRESH: IT_ZSDT0105.

    SELECT *
      FROM ZSDT0105
      INTO TABLE IT_ZSDT0105
     WHERE DOCNUM_REF = WA_ZSDT0102-DOCNUM
       AND NMDFE      = WA_ZSDT0102-NMDFE.

    VL_NMDFE      = WA_ZSDT0102-NMDFE.
    VL_DOCNUM     = WA_ZSDT0102-DOCNUM.
    VL_DATA_AUT   = WA_ZSDT0102-DT_AUTHCOD.
    VL_MSG_RET    = WA_ZSDT0102-MSG.

    CLEAR VL_DATA.
    CONCATENATE VL_DATA_AUT+6(2) '/' VL_DATA_AUT+4(2) '/' VL_DATA_AUT(4) INTO VL_DATA.

    CONC_HTML  '<tr bordercolor="black">'.

    CONC_HTML     '<td width="20%"><p align="left"> <font color="000" size=1><b>'.
    CONC_HTML        VL_NMDFE.
    CONC_HTML     '</b></font></p></td>'.

    CONC_HTML     '<td width="20%"><p align="left"> <font color="000" size=1><b>'.
    CONC_HTML        VL_DOCNUM.
    CONC_HTML     '</b></font></p></td>'.

    CONC_HTML     '<td width="20%"><p align="left"> <font color="000" size=1><b>'.
    CONC_HTML        VL_DATA.
    CONC_HTML     '</b></font></p></td>'.

    CONC_HTML     '<td width="30%"><p align="left"> <font color="000" size=1><b>'.
    CONC_HTML        VL_MSG_RET.
    CONC_HTML     '</b></font></p></td>'.

    CONC_HTML     '<td width="20%"><p align="left"> <font color="000" size=1><b>'.

    CONC_HTML         '<table>'.

    LOOP AT IT_ZSDT0105 INTO WA_ZSDT0105.

      VL_DOC_CTE = WA_ZSDT0105-DOCNUM.

      CONC_HTML           '<tr bordercolor="black">'.

      CONC_HTML              '<td width="100%"><p align="left"> <font color="000" size=1><b>'.
      CONC_HTML                 VL_DOC_CTE.
      CONC_HTML              '</b></font></p></td>'.

      CONC_HTML           '</tr>'.

    ENDLOOP.

    CONC_HTML         '</table>'.

    CONC_HTML     '</b></font></p></td>'.


    CONC_HTML  '</tr>'.

  ENDLOOP.

  CONC_HTML '</table>'.

  CONC_HTML '<BR>'.
  CONC_HTML '<BR>'.
  CONC_HTML '<DIV align=left>'.

  CONC_HTML '<DIV align=center><FONT face=Verdana color=#ffaaaa size=1><STRONG>E-mail gerado automáticamente pelo sistema</STRONG></FONT></DIV>'.
  CONC_HTML '<DIV align=center><FONT face=Verdana color=#3CB371 size=2><STRONG>Grupo André Maggi</STRONG></FONT></DIV>'.

  CONC_HTML '</DIV>'.
  CONC_HTML '<BR>'.
  CONC_HTML '</body>'.
  CONC_HTML '</html>'.

  "Corpo
  DOC_CHNG-OBJ_NAME = 'Encerramento MDF-e'.
  DOC_CHNG-OBJ_DESCR = 'Aviso de Encerramento de MDF-e(s)'.
  DOC_CHNG-NO_CHANGE = 'X'.

  CLEAR OBJPACK-TRANSF_BIN.
  OBJPACK-HEAD_START = 1.
  OBJPACK-HEAD_NUM = 0.
  OBJPACK-BODY_START = 1.
  OBJPACK-BODY_NUM = 99999.
  OBJPACK-DOC_TYPE = 'HTM'.
  APPEND OBJPACK.

  "Enviar
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      DOCUMENT_DATA              = DOC_CHNG
      PUT_IN_OUTBOX              = 'X'
      COMMIT_WORK                = 'X'
    TABLES
      PACKING_LIST               = OBJPACK
      CONTENTS_TXT               = IT_HTML
      RECEIVERS                  = RECLIST
    EXCEPTIONS
      TOO_MANY_RECEIVERS         = 1
      DOCUMENT_NOT_SENT          = 2
      OPERATION_NO_AUTHORIZATION = 4
      OTHERS                     = 99.



ENDFORM.                    " EMAIL_AVISO_ENC

*&---------------------------------------------------------------------*
*&      Form  ENC_MDFE_C_AVISO
*&---------------------------------------------------------------------*
FORM ENC_MDFE_C_AVISO.

  DATA: VL_DATA_LIM   TYPE D,
        VL_DATA_AVISO TYPE D,
        VL_LINES      TYPE I,
        VL_DATA_PREV  TYPE D,
        VL_HORA_PREV  TYPE STRING,
        VL_DATA       TYPE STRING,
        VL_DHPREV     TYPE STRING.

  DATA: VL_NMDFE       TYPE STRING,
        VL_DOCNUM      TYPE STRING,
        VL_DOC_CTE     TYPE STRING,
        VL_DATA_AUT    TYPE STRING,
        VL_TITULO      TYPE STRING,
        VL_MSG_RET     TYPE STRING.

  DEFINE CONC_HTML.
    CALL FUNCTION 'ZHTML_ADD'
      EXPORTING
        I_TEXTO = &1
      TABLES
        IT_HTML = IT_HTML.
  END-OF-DEFINITION.

  REFRESH: IT_ZSDT0102, IT_ZSDT0102_AUX, IT_HTML.
  CLEAR: WA_ZSDT0102, VL_LINES.

  VL_DATA_LIM = SY-DATUM - 30.

  SELECT *
    FROM ZSDT0102
    INTO TABLE IT_ZSDT0102
  WHERE AUTORIZADO EQ 'X'
    AND ENCERRADO  NE 'X'
    AND CANCEL     NE 'X'
    AND DT_AUTHCOD NE ''
    AND DATA_EMI   NE ''
    AND DATA_EMI   >= VL_DATA_LIM
    AND DT_AUTHCOD >= VL_DATA_LIM
    AND AVISO_ENC_AUT = 'X'.

  CHECK IT_ZSDT0102[] IS NOT INITIAL.

  LOOP AT IT_ZSDT0102 INTO WA_ZSDT0102.

    FREE: ZCL_MDFE.

    CREATE OBJECT ZCL_MDFE
      EXPORTING
        I_NMDFE  = WA_ZSDT0102-NMDFE
        I_DOCNUM = WA_ZSDT0102-DOCNUM.

    ZCL_MDFE->ENCERRAR_MDFE( I_NO_MSG_CONFIRM = 'X'
                             I_ENC_AUT        = 'X' ).


    MOVE-CORRESPONDING WA_ZSDT0102 TO WA_ZSDT0102_AUX.
    APPEND WA_ZSDT0102_AUX TO IT_ZSDT0102_AUX.

    CLEAR: WA_ZSDT0102, WA_ZSDT0102_AUX.

  ENDLOOP.

  SELECT SINGLE *
    FROM SETLEAF INTO @DATA(LWA_SET_ENV_EMAIL_01)
   WHERE SETNAME EQ 'ZCL_MDFE'
     AND VALFROM EQ 'EMAIL_ENC_001'.

  CHECK SY-SUBRC EQ 0.

  REFRESH IT_ZSDT0102.

  CHECK IT_ZSDT0102_AUX[] IS NOT INITIAL.

  SELECT *
    FROM ZSDT0102
    INTO TABLE IT_ZSDT0102
    FOR ALL ENTRIES IN IT_ZSDT0102_AUX
   WHERE AUTORIZADO    EQ 'X'
     AND CANCEL        NE 'X'
     AND AVISO_ENC_AUT EQ 'X'
     AND SOL_ENC_AUT   EQ 'X'
     AND DT_AUTHCOD    NE ''
     AND DATA_EMI      NE ''
     AND DATA_EMI      >= VL_DATA_LIM
     AND DT_AUTHCOD    >= VL_DATA_LIM
     AND DOCNUM        EQ IT_ZSDT0102_AUX-DOCNUM
     AND NMDFE         EQ IT_ZSDT0102_AUX-NMDFE.

  CHECK IT_ZSDT0102[] IS NOT INITIAL.

  CLEAR: VL_TITULO.
  VL_TITULO = 'Aviso! Houve Solicitação de Encerramento Automático para o(s) MDF-e(s) abaixo!'.

  "Monta Corpo Email
  CONC_HTML '<html>'.
  CONC_HTML '<head><title>Encerramento Automático MDF-e</title><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1"></head>'.
  CONC_HTML '<body bgcolor="#f5f1ff" leftmargin="0" topmargin="0" marginwidth="0" marginheight="0">'.
  CONC_HTML '<DIV align=center><FONT face=Verdana color=#ff0000 size=4><STRONG>'.
  CONC_HTML VL_TITULO.
  CONC_HTML '</STRONG></FONT></DIV><BR>'.
  CONC_HTML '<FONT face=Verdana color=#0000ff size=2>'.
  CONC_HTML '<BR>'.

  CONC_HTML '<table cellspacing="0" border="1" bordercolor="FFFFFF" width="100%">'.

  CONC_HTML    '<tr bgcolor="B0C4DE">'.
  CONC_HTML       '<td width="20%"><p align="center"><font color="#000030" size=1><b>Num. MDF-e</b></font></p></td>'.
  CONC_HTML       '<td width="20%"><p align="center"><font color="#000030" size=1><b>Doc. Num. MDF-e </b></font></p></td>'.
  CONC_HTML       '<td width="20%"><p align="center"><font color="#000030" size=1><b>Data Autorização</b></font></p></td>'.
  CONC_HTML       '<td width="30%"><p align="center"><font color="#000030" size=1><b>Msg. Retorno</b></font></p></td>'.
  CONC_HTML       '<td width="20%"><p align="center"><font color="#000030" size=1><b>CT-e(s) Vinculados</b></font></p></td>'.
  CONC_HTML    '</tr>'.

  LOOP AT IT_ZSDT0102 INTO WA_ZSDT0102.

    REFRESH: IT_ZSDT0105.

    SELECT *
      FROM ZSDT0105
      INTO TABLE IT_ZSDT0105
     WHERE DOCNUM_REF = WA_ZSDT0102-DOCNUM
       AND NMDFE      = WA_ZSDT0102-NMDFE.

    VL_NMDFE      = WA_ZSDT0102-NMDFE.
    VL_DOCNUM     = WA_ZSDT0102-DOCNUM.
    VL_DATA_AUT   = WA_ZSDT0102-DT_AUTHCOD.
    VL_MSG_RET    = WA_ZSDT0102-MSG.

    CLEAR VL_DATA.
    CONCATENATE VL_DATA_AUT+6(2) '/' VL_DATA_AUT+4(2) '/' VL_DATA_AUT(4) INTO VL_DATA.

    CONC_HTML  '<tr bordercolor="black">'.

    CONC_HTML     '<td width="20%"><p align="left"> <font color="000" size=1><b>'.
    CONC_HTML        VL_NMDFE.
    CONC_HTML     '</b></font></p></td>'.

    CONC_HTML     '<td width="20%"><p align="left"> <font color="000" size=1><b>'.
    CONC_HTML        VL_DOCNUM.
    CONC_HTML     '</b></font></p></td>'.

    CONC_HTML     '<td width="20%"><p align="left"> <font color="000" size=1><b>'.
    CONC_HTML        VL_DATA.
    CONC_HTML     '</b></font></p></td>'.

    CONC_HTML     '<td width="30%"><p align="left"> <font color="000" size=1><b>'.
    CONC_HTML        VL_MSG_RET.
    CONC_HTML     '</b></font></p></td>'.

    CONC_HTML     '<td width="20%"><p align="left"> <font color="000" size=1><b>'.

    CONC_HTML         '<table>'.

    LOOP AT IT_ZSDT0105 INTO WA_ZSDT0105.

      VL_DOC_CTE = WA_ZSDT0105-DOCNUM.

      CONC_HTML           '<tr bordercolor="black">'.

      CONC_HTML              '<td width="100%"><p align="left"> <font color="000" size=1><b>'.
      CONC_HTML                 VL_DOC_CTE.
      CONC_HTML              '</b></font></p></td>'.

      CONC_HTML           '</tr>'.

    ENDLOOP.

    CONC_HTML         '</table>'.

    CONC_HTML     '</b></font></p></td>'.


    CONC_HTML  '</tr>'.

  ENDLOOP.

  CONC_HTML '</table>'.

  CONC_HTML '<BR>'.
  CONC_HTML '<BR>'.
  CONC_HTML '<DIV align=left>'.

  CONC_HTML '<DIV align=center><FONT face=Verdana color=#ffaaaa size=1><STRONG>E-mail gerado automáticamente pelo sistema</STRONG></FONT></DIV>'.
  CONC_HTML '<DIV align=center><FONT face=Verdana color=#3CB371 size=2><STRONG>Grupo André Maggi</STRONG></FONT></DIV>'.

  CONC_HTML '</DIV>'.
  CONC_HTML '<BR>'.
  CONC_HTML '</body>'.
  CONC_HTML '</html>'.

  "Corpo
  DOC_CHNG-OBJ_NAME = 'Encerramento MDF-e'.
  DOC_CHNG-OBJ_DESCR = 'Solicitação de Encerramento de MDF-e(s)'.
  DOC_CHNG-NO_CHANGE = 'X'.

  CLEAR OBJPACK-TRANSF_BIN.
  OBJPACK-HEAD_START = 1.
  OBJPACK-HEAD_NUM = 0.
  OBJPACK-BODY_START = 1.
  OBJPACK-BODY_NUM = 99999.
  OBJPACK-DOC_TYPE = 'HTM'.
  APPEND OBJPACK.

  "Enviar
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      DOCUMENT_DATA              = DOC_CHNG
      PUT_IN_OUTBOX              = 'X'
      COMMIT_WORK                = 'X'
    TABLES
      PACKING_LIST               = OBJPACK
      CONTENTS_TXT               = IT_HTML
      RECEIVERS                  = RECLIST
    EXCEPTIONS
      TOO_MANY_RECEIVERS         = 1
      DOCUMENT_NOT_SENT          = 2
      OPERATION_NO_AUTHORIZATION = 4
      OTHERS                     = 99.



ENDFORM.                    " ENC_MDFE_C_AVISO

*&---------------------------------------------------------------------*
*&      Form  EMAIL_AVISO_ALERTA
*&---------------------------------------------------------------------*
FORM EMAIL_AVISO_ALERTA.

  DATA: VL_DATA_LIM   TYPE D,
        V_DT_EMISSAO_INI TYPE D,
        VL_DATA_AVISO TYPE D,
        VL_LINES      TYPE I,
        VL_DATA_PREV  TYPE D,
        VL_HORA_PREV  TYPE STRING,
        VL_DATA       TYPE STRING,
        VL_DHPREV     TYPE STRING.

  DATA: VL_NMDFE       TYPE STRING,
        VL_DOCNUM      TYPE STRING,
        VL_DOC_CTE     TYPE STRING,
        VL_DATA_AUT    TYPE STRING,
        VL_TITULO      TYPE STRING,
        VL_MSG_RET     TYPE STRING.

  DEFINE CONC_HTML.
    CALL FUNCTION 'ZHTML_ADD'
      EXPORTING
        I_TEXTO = &1
      TABLES
        IT_HTML = IT_HTML.
  END-OF-DEFINITION.

  REFRESH: IT_ZSDT0102, IT_HTML.
  CLEAR: WA_ZSDT0102, VL_LINES.

  VL_DATA_LIM       = SY-DATUM - 12.
  V_DT_EMISSAO_INI  = SY-DATUM - 35.

  SELECT *
    FROM ZSDT0102
    INTO TABLE IT_ZSDT0102
   WHERE AUTORIZADO EQ 'X'
     AND ENCERRADO  NE 'X'
     AND CANCEL     NE 'X'
     AND DT_AUTHCOD NE ''
     AND DT_AUTHCOD <= VL_DATA_LIM
     AND DT_AUTHCOD >= V_DT_EMISSAO_INI.

  VL_LINES = LINES( IT_ZSDT0102 ).

  IF ( VL_LINES > 0 ).

    CLEAR: VL_TITULO.
    VL_TITULO = 'Atenção! MDF-e(s) autorizados a 12 dias e não encerrados!'.

    "Monta Corpo Email
    CONC_HTML '<html>'.
    CONC_HTML '<head><title>Encerramento Automático MDF-e</title><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1"></head>'.
    CONC_HTML '<body bgcolor="#f5f1ff" leftmargin="0" topmargin="0" marginwidth="0" marginheight="0">'.
    CONC_HTML '<DIV align=center><FONT face=Verdana color=#ff0000 size=4><STRONG>'.
    CONC_HTML VL_TITULO.
    CONC_HTML '</STRONG></FONT></DIV><BR>'.
    CONC_HTML '<FONT face=Verdana color=#0000ff size=2>'.
    CONC_HTML '<BR>'.

    CONC_HTML '<table cellspacing="0" border="1" bordercolor="FFFFFF" width="100%">'.

    CONC_HTML    '<tr bgcolor="B0C4DE">'.
    CONC_HTML       '<td width="20%"><p align="center"><font color="#000030" size=1><b>Num. MDF-e</b></font></p></td>'.
    CONC_HTML       '<td width="20%"><p align="center"><font color="#000030" size=1><b>Doc. Num. MDF-e </b></font></p></td>'.
    CONC_HTML       '<td width="20%"><p align="center"><font color="#000030" size=1><b>Data Autorização</b></font></p></td>'.
    CONC_HTML       '<td width="30%"><p align="center"><font color="#000030" size=1><b>Msg. Retorno</b></font></p></td>'.
    CONC_HTML       '<td width="20%"><p align="center"><font color="#000030" size=1><b>CT-e(s) Vinculados</b></font></p></td>'.
    CONC_HTML    '</tr>'.

    LOOP AT IT_ZSDT0102 INTO WA_ZSDT0102.

      REFRESH: IT_ZSDT0105.

      SELECT *
        FROM ZSDT0105
        INTO TABLE IT_ZSDT0105
       WHERE DOCNUM_REF = WA_ZSDT0102-DOCNUM
         AND NMDFE      = WA_ZSDT0102-NMDFE.

      VL_NMDFE      = WA_ZSDT0102-NMDFE.
      VL_DOCNUM     = WA_ZSDT0102-DOCNUM.
      VL_DATA_AUT   = WA_ZSDT0102-DT_AUTHCOD.
      VL_MSG_RET    = WA_ZSDT0102-MSG.

      CLEAR VL_DATA.
      CONCATENATE VL_DATA_AUT+6(2) '/' VL_DATA_AUT+4(2) '/' VL_DATA_AUT(4) INTO VL_DATA.

      CONC_HTML  '<tr bordercolor="black">'.

      CONC_HTML     '<td width="20%"><p align="left"> <font color="000" size=1><b>'.
      CONC_HTML        VL_NMDFE.
      CONC_HTML     '</b></font></p></td>'.

      CONC_HTML     '<td width="20%"><p align="left"> <font color="000" size=1><b>'.
      CONC_HTML        VL_DOCNUM.
      CONC_HTML     '</b></font></p></td>'.

      CONC_HTML     '<td width="20%"><p align="left"> <font color="000" size=1><b>'.
      CONC_HTML        VL_DATA.
      CONC_HTML     '</b></font></p></td>'.

      CONC_HTML     '<td width="30%"><p align="left"> <font color="000" size=1><b>'.
      CONC_HTML        VL_MSG_RET.
      CONC_HTML     '</b></font></p></td>'.

      CONC_HTML     '<td width="20%"><p align="left"> <font color="000" size=1><b>'.

      CONC_HTML         '<table>'.

      LOOP AT IT_ZSDT0105 INTO WA_ZSDT0105.

        VL_DOC_CTE = WA_ZSDT0105-DOCNUM.

        CONC_HTML           '<tr bordercolor="black">'.

        CONC_HTML              '<td width="100%"><p align="left"> <font color="000" size=1><b>'.
        CONC_HTML                 VL_DOC_CTE.
        CONC_HTML              '</b></font></p></td>'.

        CONC_HTML           '</tr>'.

      ENDLOOP.

      CONC_HTML         '</table>'.

      CONC_HTML     '</b></font></p></td>'.


      CONC_HTML  '</tr>'.

    ENDLOOP.

    CONC_HTML '</table>'.

    CONC_HTML '<BR>'.
    CONC_HTML '<BR>'.
    CONC_HTML '<DIV align=left>'.

    CONC_HTML '<DIV align=center><FONT face=Verdana color=#ffaaaa size=1><STRONG>E-mail gerado automáticamente pelo sistema</STRONG></FONT></DIV>'.
    CONC_HTML '<DIV align=center><FONT face=Verdana color=#3CB371 size=2><STRONG>Grupo André Maggi</STRONG></FONT></DIV>'.

    CONC_HTML '</DIV>'.
    CONC_HTML '<BR>'.
    CONC_HTML '</body>'.
    CONC_HTML '</html>'.

    "Corpo
    DOC_CHNG-OBJ_NAME = 'Encerramento MDF-e'.
    DOC_CHNG-OBJ_DESCR = 'MDF-e(s) não Encerrados'.
    DOC_CHNG-NO_CHANGE = 'X'.

    CLEAR OBJPACK-TRANSF_BIN.
    OBJPACK-HEAD_START = 1.
    OBJPACK-HEAD_NUM = 0.
    OBJPACK-BODY_START = 1.
    OBJPACK-BODY_NUM = 99999.
    OBJPACK-DOC_TYPE = 'HTM'.
    APPEND OBJPACK.

    "Enviar
    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        DOCUMENT_DATA              = DOC_CHNG
        PUT_IN_OUTBOX              = 'X'
        COMMIT_WORK                = 'X'
      TABLES
        PACKING_LIST               = OBJPACK
        CONTENTS_TXT               = IT_HTML
        RECEIVERS                  = RECLIST
      EXCEPTIONS
        TOO_MANY_RECEIVERS         = 1
        DOCUMENT_NOT_SENT          = 2
        OPERATION_NO_AUTHORIZATION = 4
        OTHERS                     = 99.

  ENDIF.  "IF ( VL_LINES > 0 ).

ENDFORM.                    " EMAIL_AVISO_ALERTA

FORM GET_ENC_10_DIAS_AUTH TABLES T_ZSDT0102 STRUCTURE ZSDT0102.

  DATA: IT_ZSDT0102_L TYPE TABLE OF ZSDT0102 WITH HEADER LINE.

  DATA: VL_DATA_LIM      TYPE D,
        V_DT_EMISSAO_INI TYPE D.

  VL_DATA_LIM      = SY-DATUM - 10.
  V_DT_EMISSAO_INI = SY-DATUM - 35.

  CLEAR: IT_ZSDT0102_L[].

  SELECT *
    FROM ZSDT0102
    INTO TABLE IT_ZSDT0102_L
   WHERE AUTORIZADO EQ 'X'
     AND ENCERRADO  NE 'X'
     AND CANCEL     NE 'X'
     AND DT_AUTHCOD NE ''
     AND DATA_EMI   NE ''
     AND DATA_EMI   <= VL_DATA_LIM
     AND DT_AUTHCOD <= VL_DATA_LIM
     AND DT_AUTHCOD >= V_DT_EMISSAO_INI
     AND AVISO_ENC_AUT NE 'X'.

  LOOP AT IT_ZSDT0102_L.

    IT_ZSDT0102_L-TP_ENCERRAMENTO = '1'.

    APPEND IT_ZSDT0102_L TO T_ZSDT0102.
  ENDLOOP.

ENDFORM.


FORM GET_ENC_DESCARGA_L1 TABLES T_ZSDT0102 STRUCTURE ZSDT0102.

  DATA: TG_ZLEST0039      TYPE TABLE OF ZLEST0039      WITH HEADER LINE,
        TG_ZCTE_INFO_NOTA TYPE TABLE OF ZCTE_INFO_NOTA WITH HEADER LINE,
        TG_ZSDT0105       TYPE TABLE OF ZSDT0105       WITH HEADER LINE,
        TG_ZSDT0102       TYPE TABLE OF ZSDT0102       WITH HEADER LINE.


  DATA: V_DT_EMISSAO_INI   TYPE SY-DATUM,
        V_DATATRANSB_NULL  TYPE ZLEST0039-DATATRANSB,
        V_DATACHEGADA_NULL TYPE ZLEST0039-DATACHEGADA.

  CLEAR: TG_ZLEST0039[], V_DATATRANSB_NULL, V_DATACHEGADA_NULL, TG_ZSDT0105[], TG_ZCTE_INFO_NOTA[].

  V_DT_EMISSAO_INI = SY-DATUM - 20.

  SELECT *
    FROM ZLEST0039 INTO TABLE TG_ZLEST0039
   WHERE DATASAIDA  GE V_DT_EMISSAO_INI
     AND DATATRANSB NE V_DATATRANSB_NULL
     AND DATATRANSB NE SPACE.

  SELECT *
    FROM ZLEST0039 APPENDING TABLE TG_ZLEST0039
   WHERE DATASAIDA   GE V_DT_EMISSAO_INI
     AND DATACHEGADA NE V_DATACHEGADA_NULL
     AND DATACHEGADA NE SPACE.

  DELETE TG_ZLEST0039 WHERE DOCNUM IS INITIAL.

  SORT TG_ZLEST0039 BY DOCNUM.
  DELETE ADJACENT DUPLICATES FROM TG_ZLEST0039 COMPARING DOCNUM.

  CHECK TG_ZLEST0039[] IS NOT INITIAL.

*----------------------------------------------------------------------*
* Carregar MDF-e's para os documentos de CT-e
*----------------------------------------------------------------------*
  SELECT *
    FROM ZCTE_INFO_NOTA INTO TABLE TG_ZCTE_INFO_NOTA
     FOR ALL ENTRIES IN TG_ZLEST0039
   WHERE DOCNUM_NF = TG_ZLEST0039-DOCNUM.

  DELETE TG_ZCTE_INFO_NOTA WHERE DOCNUM IS INITIAL.

  IF TG_ZCTE_INFO_NOTA[] IS NOT INITIAL.
    SELECT *
      FROM ZSDT0105 APPENDING TABLE TG_ZSDT0105
       FOR ALL ENTRIES IN TG_ZCTE_INFO_NOTA
     WHERE DOCNUM = TG_ZCTE_INFO_NOTA-DOCNUM.
  ENDIF.
*----------------------------------------------------------------------*
* Carregar MDF-e's para os documentos de NF-e
*----------------------------------------------------------------------*
  SELECT *
    FROM ZSDT0105 APPENDING TABLE TG_ZSDT0105
     FOR ALL ENTRIES IN TG_ZLEST0039
   WHERE DOCNUM = TG_ZLEST0039-DOCNUM.

  DELETE TG_ZSDT0105 WHERE DOCNUM_REF IS INITIAL.

  SORT TG_ZSDT0105 BY DOCNUM_REF.
  DELETE ADJACENT DUPLICATES FROM TG_ZSDT0105 COMPARING DOCNUM_REF.

  CHECK TG_ZSDT0105[] IS NOT INITIAL.

  SELECT *
    FROM ZSDT0102 INTO TABLE TG_ZSDT0102
     FOR ALL ENTRIES IN TG_ZSDT0105
   WHERE DOCNUM        EQ TG_ZSDT0105-DOCNUM_REF
     AND AUTORIZADO    EQ 'X'
     AND ENCERRADO     NE 'X'
     AND CANCEL        NE 'X'
     AND DT_AUTHCOD    NE ''
     AND AVISO_ENC_AUT NE 'X'.

  LOOP AT TG_ZSDT0102 ASSIGNING FIELD-SYMBOL(<FS_ZSDT0102>).

    READ TABLE TG_ZSDT0105 WITH KEY DOCNUM_REF = <FS_ZSDT0102>-DOCNUM.
    CHECK SY-SUBRC EQ 0.

    READ TABLE TG_ZCTE_INFO_NOTA WITH KEY DOCNUM = TG_ZSDT0105-DOCNUM.
    CHECK SY-SUBRC EQ 0.

    <FS_ZSDT0102>-TP_ENCERRAMENTO = '2'.

    APPEND <FS_ZSDT0102> TO T_ZSDT0102.
  ENDLOOP.


ENDFORM.

FORM GET_ENC_DESCARGA_TRANSF TABLES T_ZSDT0102 STRUCTURE ZSDT0102.

  TYPES: BEGIN OF TY_ROM_SAIDA_TRANSF,
           VBELN          TYPE ZSDT0001-VBELN,
           NRO_NF_PROD    TYPE ZSDT0001-NRO_NF_PROD,
           NRO_NF_FRETE   TYPE ZSDT0001-NRO_NF_FRETE,
           CHAVE_NFE      TYPE ZDE_CHAVE_NFE,
           TRANSFERENCIA  TYPE C,
         END OF TY_ROM_SAIDA_TRANSF.

  DATA: VL_DATA_LIM   TYPE D,

        V_CANDAT_NULL TYPE J_1BNFDOC-CANDAT.

  DATA: IT_ROM_ENT_TRANSF       TYPE TABLE OF ZSDT0001 WITH HEADER LINE,
        IT_ROM_ENT_TRANSF_AUX   TYPE TABLE OF ZSDT0001 WITH HEADER LINE,
        IT_ROM_SAI_TRANSF       TYPE TABLE OF TY_ROM_SAIDA_TRANSF WITH HEADER LINE,
        IT_ZSDT0105_L           TYPE TABLE OF ZSDT0105 WITH HEADER LINE,
        IT_ZSDT0102_L           TYPE TABLE OF ZSDT0102 WITH HEADER LINE.

  VL_DATA_LIM = SY-DATUM - 12.

  CLEAR: IT_ROM_SAI_TRANSF[], IT_ROM_ENT_TRANSF[], V_CANDAT_NULL.

  SELECT VBELN, NRO_NF_PROD, NRO_NF_FRETE
    FROM ZSDT0001 INTO CORRESPONDING FIELDS OF TABLE @IT_ROM_SAI_TRANSF
   WHERE DT_MOVIMENTO GE @VL_DATA_LIM
     AND TP_MOVIMENTO EQ 'S'.

  LOOP AT IT_ROM_SAI_TRANSF ASSIGNING FIELD-SYMBOL(<FS_ROM_SAI_TRANSF>).

    <FS_ROM_SAI_TRANSF>-NRO_NF_PROD = |{ <FS_ROM_SAI_TRANSF>-NRO_NF_PROD ALPHA = IN }|.

    CHECK ( <FS_ROM_SAI_TRANSF>-NRO_NF_PROD IS NOT INITIAL ) AND ( <FS_ROM_SAI_TRANSF>-VBELN IS NOT INITIAL ).

    SELECT SINGLE *
      FROM EKKO INTO @DATA(_WL_EKKO)
     WHERE EBELN = @<FS_ROM_SAI_TRANSF>-VBELN.

    CHECK ( SY-SUBRC EQ 0 ) AND ( _WL_EKKO-BSART = 'ZUB' ).

    SELECT SINGLE *
      FROM J_1BNFDOC INTO @DATA(_WL_DOC)
     WHERE DOCNUM EQ @<FS_ROM_SAI_TRANSF>-NRO_NF_PROD
       AND CANDAT EQ @V_CANDAT_NULL
       AND CANCEL EQ @SPACE.

    CHECK SY-SUBRC EQ 0.

    SELECT SINGLE *
      FROM J_1BNFE_ACTIVE INTO @DATA(_WL_ACTIVE)
     WHERE DOCNUM = @<FS_ROM_SAI_TRANSF>-NRO_NF_PROD
       AND DOCSTA = '1'.

    CHECK SY-SUBRC EQ 0.

    CONCATENATE _WL_ACTIVE-REGIO
                _WL_ACTIVE-NFYEAR
                _WL_ACTIVE-NFMONTH
                _WL_ACTIVE-STCD1
                _WL_ACTIVE-MODEL
                _WL_ACTIVE-SERIE
                _WL_ACTIVE-NFNUM9
                _WL_ACTIVE-DOCNUM9
                _WL_ACTIVE-CDV INTO <FS_ROM_SAI_TRANSF>-CHAVE_NFE.

    IF STRLEN( <FS_ROM_SAI_TRANSF>-CHAVE_NFE ) EQ 44.
      <FS_ROM_SAI_TRANSF>-TRANSFERENCIA = ABAP_TRUE.
    ENDIF.

  ENDLOOP.

  DELETE IT_ROM_SAI_TRANSF WHERE ( TRANSFERENCIA EQ ABAP_FALSE ) OR ( CHAVE_NFE IS INITIAL ).

  CHECK IT_ROM_SAI_TRANSF[] IS NOT INITIAL.

  SELECT *
    FROM ZSDT0001 INTO TABLE IT_ROM_ENT_TRANSF
     FOR ALL ENTRIES IN IT_ROM_SAI_TRANSF
   WHERE CHAVE_NFE    = IT_ROM_SAI_TRANSF-CHAVE_NFE
     AND TP_MOVIMENTO = 'E'.

  CHECK IT_ROM_ENT_TRANSF[] IS NOT INITIAL.

  CLEAR: IT_ZSDT0102_L[].

  LOOP AT IT_ROM_SAI_TRANSF.

    READ TABLE IT_ROM_ENT_TRANSF WITH KEY CHAVE_NFE = IT_ROM_SAI_TRANSF-CHAVE_NFE. "Possui Romaneio de entrada para a NF-e de Saida de Transferencia
    CHECK SY-SUBRC EQ 0.

    "Checar MDF-e NF-e
    PERFORM APPEND_MDFE_DOC TABLES IT_ZSDT0102_L
                             USING IT_ROM_SAI_TRANSF-NRO_NF_PROD.

    ""Checar MDF-e CT-e
    PERFORM APPEND_MDFE_DOC TABLES IT_ZSDT0102_L
                             USING IT_ROM_SAI_TRANSF-NRO_NF_FRETE.

  ENDLOOP.

  LOOP AT IT_ZSDT0102_L.

    IT_ZSDT0102_L-TP_ENCERRAMENTO = '3'.

    APPEND IT_ZSDT0102_L TO T_ZSDT0102.
  ENDLOOP.

ENDFORM.

FORM APPEND_MDFE_DOC TABLES T_ZSDT0102 STRUCTURE ZSDT0102
                      USING P_DOCNUM.

  DATA: V_DT_AUTHCOD_NULL TYPE ZSDT0102-DT_AUTHCOD,
        V_DATA_EMI_NULL   TYPE ZSDT0102-DATA_EMI.

  CLEAR: V_DT_AUTHCOD_NULL, V_DATA_EMI_NULL.

  CHECK P_DOCNUM IS NOT INITIAL.

  SELECT SINGLE *
    FROM ZSDT0105 AS A INTO @DATA(_WL_ZSDT0105_L)
   WHERE DOCNUM EQ @P_DOCNUM
     AND EXISTS ( SELECT *
                    FROM ZSDT0102 AS B
                   WHERE B~DOCNUM        EQ A~DOCNUM_REF
                     AND B~AUTORIZADO    EQ 'X'
                     AND B~ENCERRADO     NE 'X'
                     AND B~CANCEL        NE 'X'
                     AND DT_AUTHCOD      NE @V_DT_AUTHCOD_NULL
                     AND DATA_EMI        NE @V_DATA_EMI_NULL
                     AND B~AVISO_ENC_AUT NE 'X' ).

  CHECK ( SY-SUBRC EQ 0 ) AND ( _WL_ZSDT0105_L-DOCNUM_REF IS NOT INITIAL ).

  SELECT SINGLE *
    FROM ZSDT0102 INTO @DATA(_WL_ZSDT0102_L)
   WHERE DOCNUM        EQ @_WL_ZSDT0105_L-DOCNUM_REF
     AND AUTORIZADO    EQ 'X'
     AND ENCERRADO     NE 'X'
     AND CANCEL        NE 'X'
     AND DT_AUTHCOD    NE @V_DT_AUTHCOD_NULL
     AND DATA_EMI      NE @V_DATA_EMI_NULL
     AND AVISO_ENC_AUT NE 'X'.

  CHECK SY-SUBRC EQ 0.

  APPEND _WL_ZSDT0102_L TO T_ZSDT0102.

ENDFORM.

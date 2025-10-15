*&---------------------------------------------------------------------*
*& Report  ZFIR075
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZFIR075.

TYPES: BEGIN OF TY_SAIDA,

         BUKRS                 TYPE BSID-BUKRS, " EMPRESA
         VKBUR                 TYPE VBAK-VKBUR, " ESCRITORIO_VENDA
         DESC_ESCRITORIO_VENDA TYPE TVKBT-BEZEI,
         VKGRP                 TYPE VBAK-VKGRP, " EQUIPE_VENDA
         DESC_GRUPO_VENDA      TYPE TVGRT-BEZEI,
         VBEL2                 TYPE BSID-VBEL2, " OV
         KUNNR                 TYPE BSID-KUNNR, " COD_CLIENTE
         NAME1                 TYPE KNA1-NAME1, " NOME_CLIENTE
         SPART                 TYPE VBAK-SPART, " SETOR_ATIVIDADE
         DESC_SETOR_ATIVIDADE  TYPE TSPAT-VTEXT,
         ZFBDT                 TYPE BSID-ZFBDT, " DT_VENCIMENTO
         ZBD1T                 TYPE BSID-ZBD1T, " DT_VENCIMENTO
         WAERS                 TYPE BSID-WAERS, " MOEDA
         XBLNR                 TYPE BSID-XBLNR, " FATURA
         DMBTR                 TYPE BSID-DMBTR, " VALOR_BRL
         DMBE2                 TYPE BSID-DMBE2, " VALOR_USD
         AUART                 TYPE VBAK-AUART,
       END OF TY_SAIDA.


TYPES: BEGIN OF TY_BSID,
         BUKRS TYPE BSID-BUKRS,
         BUDAT TYPE BSID-BUDAT,
         KUNNR TYPE BSID-KUNNR,
         XBLNR TYPE BSID-XBLNR,
         WAERS TYPE BSID-WAERS,
         DMBTR TYPE BSID-DMBTR,
         DMBE2 TYPE BSID-DMBE2,
         GSBER TYPE BSID-GSBER,
         VBEL2 TYPE BSID-VBEL2,
       END OF TY_BSID.

TYPES: BEGIN OF TY_KNA1,
         KUNNR TYPE  KNA1-KUNNR,
         NAME1 TYPE  KNA1-NAME1,
       END OF TY_KNA1.

TYPES: BEGIN OF TY_VBAK,
         VTWEG TYPE VBAK-VTWEG,
         SPART TYPE VBAK-SPART,
         VKBUR TYPE VBAK-VKBUR,
         VKGRP TYPE VBAK-VKGRP,
       END OF TY_VBAK.

TYPES: BEGIN OF TY_ZFIT0157,

         BUKRS TYPE ZFIT0157-BUKRS,
         SPART TYPE ZFIT0157-SPART,
         EMAIL TYPE ZFIT0157-EMAIL,

       END OF TY_ZFIT0157.



DATA: T_SAIDA    TYPE TABLE OF TY_SAIDA,
      W_SAIDA    TYPE TY_SAIDA,
      T_BSID     TYPE TABLE OF TY_BSID,
      W_BSID     TYPE TY_BSID,
      T_KNA1     TYPE TABLE OF TY_KNA1,
      W_KNA1     TYPE TY_KNA1,
      T_VBAK     TYPE TABLE OF TY_VBAK,
      W_VBAK     TYPE TY_VBAK,
      T_ZFIT0157 TYPE TABLE OF TY_ZFIT0157,
      W_ZFIT0157 TYPE TY_ZFIT0157.

DATA: LT_MAILSUBJECT     TYPE SODOCCHGI1,
      LT_MAILRECIPIENTES TYPE STANDARD TABLE OF SOMLREC90 WITH HEADER LINE,
      LT_MAILTXT         TYPE STANDARD TABLE OF SOLI WITH HEADER LINE,
      T_SET              TYPE STANDARD TABLE OF SETLEAF  WITH HEADER LINE,
      T_LAY              TYPE STANDARD TABLE OF SETLINET WITH HEADER LINE,
      T_RANGE            TYPE RANGE OF BSID-BUKRS.

DATA: VG_JOB      TYPE I.
DATA: XV_JOBNM TYPE BTCJOB.
DATA: XV_STEPC TYPE BTCSTEPCNT.

START-OF-SELECTION.

  CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
    IMPORTING
      JOBNAME         = XV_JOBNM
      STEPCOUNT       = XV_STEPC
    EXCEPTIONS
      NO_RUNTIME_INFO = 1
      OTHERS          = 2.

  IF XV_JOBNM = 'JOB_PARTIDAS_VENCIDAS_CLIENTES'.
    SELECT SINGLE COUNT(*) INTO VG_JOB
     FROM TBTCO
    WHERE JOBNAME EQ 'JOB_PARTIDAS_VENCIDAS_CLIENTES'
      AND STATUS EQ 'R'.
  ENDIF.

  IF ( VG_JOB EQ 1 ).
    PERFORM BUSCA_DADOS.
    PERFORM MONTAR_EMAIL.
  ENDIF.

END-OF-SELECTION.

FORM BUSCA_DADOS.
  SELECT
      BUKRS
      SPART
      EMAIL
    FROM
      ZFIT0157
   INTO TABLE
      T_ZFIT0157.

  IF T_ZFIT0157 IS NOT INITIAL.

    SELECT
        A~UMSKS,
        A~BSCHL,
        A~BUKRS ,
        A~VBEL2 ,
        A~KUNNR ,
        A~ZFBDT ,
        A~ZBD1T ,
        A~WAERS ,
        A~XBLNR ,
        A~DMBTR ,
        A~DMBE2 ,
        C~NAME1 ,
        B~VKBUR ,
        D~BEZEI AS DESC_ESCRITORIO_VENDA,
        B~VKGRP,
        E~BEZEI AS DESC_GRUPO_VENDA,
        B~SPART,
        F~VTEXT AS DESC_SETOR_ATIVIDADE
        FROM  BSID AS A
        INNER JOIN  VBAK  AS B ON A~VBEL2 = B~VBELN
        INNER JOIN  KNA1 AS C ON A~KUNNR = C~KUNNR
        LEFT JOIN  TVKBT AS D ON B~VKBUR = D~VKBUR
        LEFT JOIN  TVGRT AS E ON B~VKGRP = E~VKGRP
        LEFT JOIN  TSPAT AS F ON B~SPART = F~SPART AND F~SPRAS = 'P' "and F~MANDT = '300'
        INTO CORRESPONDING FIELDS OF TABLE @T_SAIDA
        FOR ALL ENTRIES IN @T_ZFIT0157
        WHERE
        A~BUKRS = @T_ZFIT0157-BUKRS
        AND A~BSCHL IN ('01', '04')
        AND A~BLART  <> 'VC'
        AND A~ZFBDT < @SY-DATUM
        AND B~AUART <> 'ZPER' "tipo da OV diferente de ZPER
        AND B~SPART = @T_ZFIT0157-SPART
        AND C~KTOKD NOT IN ('ZCIC', 'ZCEX')."diferente de intercompany e estrangeiro

  ENDIF.
ENDFORM.

FORM MONTAR_EMAIL.

  PERFORM ASSUNTO01.
  CLEAR: W_SAIDA, LT_MAILRECIPIENTES, LT_MAILTXT.
  FREE: LT_MAILRECIPIENTES, LT_MAILTXT.


ENDFORM.


FORM ASSUNTO01.


  DATA: OV                  TYPE STRING,
        ESCRITORIO          TYPE STRING,
        ESCRITORIO_ANTERIOR TYPE VBAK-VKGRP,
        DATA_FORMATADA      TYPE CHAR10,
        KUNNR_ANTERIOR      TYPE BSID-KUNNR,
        DATA_C_CARENCIA     TYPE BSID-ZFBDT,
        ENVIAR              TYPE C.



  LOOP AT  T_ZFIT0157 INTO W_ZFIT0157.

    LT_MAILRECIPIENTES-REC_TYPE = 'U'.
    LT_MAILRECIPIENTES-RECEIVER =  W_ZFIT0157-EMAIL.
    APPEND LT_MAILRECIPIENTES.


    LT_MAILSUBJECT-OBJ_LANGU = SY-LANGU.
    LT_MAILSUBJECT-OBJ_DESCR = 'Partidas Vencidas de Clientes'.

    LT_MAILTXT = '<!DOCTYPE html>'.
    APPEND LT_MAILTXT.

    LT_MAILTXT = '<html><head>'.
    APPEND LT_MAILTXT.

    LT_MAILTXT = '<style type="text/css">'.
    APPEND LT_MAILTXT.

    "Tabela do E-mail
    LT_MAILTXT = '#tabela table { font-size: 12px; padding: 0px; border-collapse: collapse; border: 1px solid #BEBEBE; }'.
    APPEND LT_MAILTXT.
    LT_MAILTXT = '#tabela th { width: 90px; font-size: 12px; background-color: #95DF8D; color000000: #; font-style: "Tahoma, Geneva, sans-serif"; border: 1px solid #BEBEBE; }'.
    APPEND LT_MAILTXT.
    LT_MAILTXT = '#tabela td { font-size: 12px; font-style: "Tahoma, Geneva, sans-serif"; border: 1px solid #BEBEBE; text-align: center; }'.
    APPEND LT_MAILTXT.
    LT_MAILTXT = '#msg { font-size: 14px; font-style: "Tahoma, Geneva, sans-serif"; }'.
    APPEND LT_MAILTXT.


*    LT_MAILTXT = '</style>'.
*    APPEND LT_MAILTXT.
*    LT_MAILTXT = '</head>'.

*    LT_MAILTXT = '<div id="msg">'.
*    APPEND LT_MAILTXT.

*    LT_MAILTXT = 'Prezados, segue composição de contas a receber. Favor tomarem as devidas providências.'.
*    APPEND LT_MAILTXT.
*
*    LT_MAILTXT = '<br>'.
*    APPEND LT_MAILTXT.

*    LT_MAILTXT = 'Partidas em Aberto :'.
*    APPEND LT_MAILTXT.




    DATA NEW_DATE TYPE BSID-ZFBDT.
    DATA TESTE TYPE I.



    LT_MAILTXT ='</style>'.
    APPEND LT_MAILTXT.
    LT_MAILTXT ='<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.4.0/css/bootstrap.min.css">'.
    APPEND LT_MAILTXT.
    LT_MAILTXT ='<link href="https://fonts.googleapis.com/css?family=Montserrat" rel="stylesheet" type="text/css">'.
    APPEND LT_MAILTXT.
    LT_MAILTXT ='<link href="https://fonts.googleapis.com/css?family=Lato" rel="stylesheet" type="text/css">'.
    APPEND LT_MAILTXT.
    LT_MAILTXT = '<script src="https://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js"></script>'.
    APPEND LT_MAILTXT.
    LT_MAILTXT = '<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.4.0/js/bootstrap.min.js"></script>'.
    APPEND LT_MAILTXT.


    LT_MAILTXT = '</head>'.
    APPEND LT_MAILTXT.
    LT_MAILTXT = '<div id="msg">Prezados, segue composição de contas a receber. Favor tomarem as devidas providências.'.
    APPEND LT_MAILTXT.
    "LT_MAILTXT = '<br>'.
    "APPEND LT_MAILTXT.
    "LT_MAILTXT = 'Partidas em Aberto :'.
    "APPEND LT_MAILTXT.
    LT_MAILTXT = '</div>'.
    APPEND LT_MAILTXT.
    LT_MAILTXT = '<div id="tabela">'.
    APPEND LT_MAILTXT.
    LT_MAILTXT = '<table>'.
    APPEND LT_MAILTXT.
    LT_MAILTXT = '<tr>'.
    APPEND LT_MAILTXT.
    LT_MAILTXT = '<th style="text-align:center;">Empresa</th>'.
    APPEND LT_MAILTXT.
    LT_MAILTXT = '<th style="text-align:center;">Escritório Venda</th>'.
    APPEND LT_MAILTXT.
    LT_MAILTXT = '<th style="text-align:center;">Equipe Venda</th>'.
    APPEND LT_MAILTXT.
    LT_MAILTXT = '<th style="text-align:center;">Nro.OV</th>'.
    APPEND LT_MAILTXT.
    LT_MAILTXT = '<th style="text-align:center;">Cod. Cliente</th>'.
    APPEND LT_MAILTXT.
    LT_MAILTXT ='<th style="text-align:center;">Nome do Cliente</th>'.
    APPEND LT_MAILTXT.
    LT_MAILTXT ='<th style="text-align:center;">Setor de Atividade</th>'.
    APPEND LT_MAILTXT.
    LT_MAILTXT ='<th style="text-align:center;">Dt.Vencimento</th>'.
    APPEND LT_MAILTXT.
    LT_MAILTXT ='<th style="text-align:center;">Moeda</th>'.
    APPEND LT_MAILTXT.
    LT_MAILTXT ='<th style="text-align:center;">Referência</th>'.
    APPEND LT_MAILTXT.
    LT_MAILTXT ='<th style="text-align:center;">Valor BRL</th>'.
    APPEND LT_MAILTXT.
    LT_MAILTXT ='<th style="text-align:center;">Valor USD</th>'.
    APPEND LT_MAILTXT.
    LT_MAILTXT ='</tr>'.

    DATA: VLR_REAIS                     TYPE CHAR20,
          VLR_DOLAR                     TYPE CHAR20,
          TOTALIZADOR_BRL               TYPE BSID-DMBTR,
          TOTALIZADOR_DOLAR             TYPE BSID-DMBE2,
          TOTALIZADOR_BRL_FORMATADO     TYPE CHAR20,
          TOTALIZADOR_DOLAR_FORMATADO   TYPE CHAR20,
          TOTAL_BRL_CLIENTE             TYPE BSID-DMBTR,
          TOTAL_DOLAR_CLIENTE           TYPE BSID-DMBE2,
          TOTAL_BRL_CLIENTE_FORMATADO   TYPE CHAR20,
          TOTAL_DOLAR_CLIENTE_FORMATADO TYPE CHAR20.

    SORT T_SAIDA BY KUNNR ZFBDT ASCENDING.

    DATA(T_CLIENTE) = T_SAIDA.

    SORT T_CLIENTE BY KUNNR.

    DELETE ADJACENT DUPLICATES FROM T_CLIENTE COMPARING KUNNR.

    LOOP AT T_CLIENTE INTO DATA(W_CLIENTE).

      LOOP AT T_SAIDA INTO W_SAIDA WHERE BUKRS EQ W_ZFIT0157-BUKRS AND SPART EQ W_ZFIT0157-SPART AND KUNNR EQ W_CLIENTE-KUNNR.  "ORDER BY W_SAIDA-KUNNR.

        "IF W_SAIDA-KUNNR = KUNNR_ANTERIOR.

        "DATA DATA_C_CARENCIA TYPE BSID-ZFBDT.

        DATA_C_CARENCIA =  W_SAIDA-ZFBDT + W_SAIDA-ZBD1T.

        IF  DATA_C_CARENCIA > SY-DATUM.
          CONTINUE.
        ENDIF.

        LT_MAILTXT = '<tr>'.
        APPEND LT_MAILTXT.

        CONCATENATE '<td style="text-align:left;">' W_SAIDA-BUKRS '</td>' INTO LT_MAILTXT.
        APPEND LT_MAILTXT.

        IF  W_SAIDA-VKBUR IS NOT INITIAL.
          CONCATENATE '<td style="text-align:left;">' W_SAIDA-VKBUR '-' W_SAIDA-DESC_ESCRITORIO_VENDA  '</td>' INTO LT_MAILTXT SEPARATED BY SPACE.
          APPEND LT_MAILTXT.
        ELSE.
          CONCATENATE '<td style="text-align:left;">' W_SAIDA-VKBUR '</td>' INTO LT_MAILTXT.
          APPEND LT_MAILTXT.
        ENDIF.

        IF W_SAIDA-VKGRP IS NOT INITIAL.
          CONCATENATE '<td style="text-align:left;">' W_SAIDA-VKGRP '-' W_SAIDA-DESC_GRUPO_VENDA '</td>' INTO LT_MAILTXT SEPARATED BY SPACE.
          APPEND LT_MAILTXT.
        ELSE.
          CONCATENATE '<td style="text-align:left;">' W_SAIDA-VKGRP '</td>' INTO LT_MAILTXT.
          APPEND LT_MAILTXT.
        ENDIF.

        CONCATENATE '<td style="text-align:left;">' W_SAIDA-VBEL2 '</td>' INTO LT_MAILTXT.
        APPEND LT_MAILTXT.

        CONCATENATE '<td style="text-align:left;">' W_SAIDA-KUNNR '</td>' INTO LT_MAILTXT.
        APPEND LT_MAILTXT.

        CONCATENATE '<td style="text-align:left;">' W_SAIDA-NAME1 '</td>' INTO LT_MAILTXT SEPARATED BY SPACE.
        APPEND LT_MAILTXT.

        IF W_SAIDA-SPART IS NOT INITIAL.
          CONCATENATE '<td style="text-align:left;">' W_SAIDA-SPART '-' W_SAIDA-DESC_SETOR_ATIVIDADE'</td>' INTO LT_MAILTXT SEPARATED BY SPACE.
          APPEND LT_MAILTXT.
        ELSE.
          CONCATENATE '<td style="text-align:left;">' W_SAIDA-SPART '</td>' INTO LT_MAILTXT .
          APPEND LT_MAILTXT.
        ENDIF.

        NEW_DATE = W_SAIDA-ZFBDT + W_SAIDA-ZBD1T.
        DATA_FORMATADA = |{ NEW_DATE+6(2) }/{ NEW_DATE+4(2) }/{ NEW_DATE+0(4) }|.
        CONCATENATE '<td style="text-align:left;">' DATA_FORMATADA '</td>' INTO LT_MAILTXT.
        APPEND LT_MAILTXT.

        CONCATENATE '<td style="text-align:left;">' W_SAIDA-WAERS '</td>' INTO LT_MAILTXT.
        APPEND LT_MAILTXT.

        CONCATENATE '<td style="text-align:left;">' W_SAIDA-XBLNR '</td>' INTO LT_MAILTXT.
        APPEND LT_MAILTXT.

        VLR_REAIS = W_SAIDA-DMBTR.
        VLR_DOLAR = W_SAIDA-DMBE2.

        PERFORM F_FORMAT_VALUE(ZFIR075) CHANGING  VLR_REAIS.

        CONCATENATE '<td style="text-align:right;">'  VLR_REAIS '</td>' INTO LT_MAILTXT.
        APPEND LT_MAILTXT.

        PERFORM F_FORMAT_VALUE(ZFIR075) CHANGING VLR_DOLAR.

        CONCATENATE '<td style="text-align:right;">' VLR_DOLAR '</td>' INTO LT_MAILTXT.
        APPEND LT_MAILTXT.

        LT_MAILTXT = '</tr>'.
        APPEND LT_MAILTXT.

        ADD W_SAIDA-DMBTR TO TOTAL_BRL_CLIENTE.
        ADD W_SAIDA-DMBE2 TO TOTAL_DOLAR_CLIENTE.


        CLEAR: VLR_REAIS, VLR_DOLAR .

      ENDLOOP.

      IF TOTAL_BRL_CLIENTE IS not INITIAL.

        TOTAL_BRL_CLIENTE_FORMATADO = TOTAL_BRL_CLIENTE.
        TOTAL_DOLAR_CLIENTE_FORMATADO = TOTAL_DOLAR_CLIENTE.


      TOTALIZADOR_BRL  = TOTALIZADOR_BRL + TOTAL_BRL_CLIENTE.
      TOTALIZADOR_DOLAR = TOTALIZADOR_DOLAR + TOTAL_DOLAR_CLIENTE.

        PERFORM F_FORMAT_VALUE(ZFIR075) CHANGING  TOTAL_BRL_CLIENTE_FORMATADO.
        PERFORM F_FORMAT_VALUE(ZFIR075) CHANGING TOTAL_DOLAR_CLIENTE_FORMATADO.
        LT_MAILTXT = '<tr>'.
        APPEND LT_MAILTXT.
        LT_MAILTXT = '<th style="text-align:right;" colspan="10">Subtotal:</th>'.
        APPEND LT_MAILTXT.

        CONCATENATE '<th style="text-align:right;">'TOTAL_BRL_CLIENTE_FORMATADO'</th>' INTO LT_MAILTXT.
        APPEND LT_MAILTXT.

        CONCATENATE '<th style="text-align:right;">'TOTAL_DOLAR_CLIENTE_FORMATADO'</th>' INTO LT_MAILTXT.
        APPEND LT_MAILTXT.

        LT_MAILTXT = '</tr>'.
        APPEND LT_MAILTXT.
      ENDIF.
      "ADD TOTAL_BRL_CLIENTE TO TOTALIZADOR_BRL.
      "ADD TOTAL_DOLAR_CLIENTE TO TOTALIZADOR_DOLAR.


      ENVIAR = 'X'.
      CLEAR: TOTAL_BRL_CLIENTE, TOTAL_DOLAR_CLIENTE, W_SAIDA.

    ENDLOOP.

    DATA(SALDO)      = REDUCE DMBTR(  INIT R TYPE DMBTR FOR LS IN T_SAIDA NEXT R = R + LS-DMBTR ).
    DATA(SALDO2)      = REDUCE DMBE2(  INIT R2 TYPE DMBE2 FOR LS2 IN T_SAIDA NEXT R2 = R2 + LS2-DMBE2 ).

    TOTALIZADOR_BRL_FORMATADO = TOTALIZADOR_BRL." SALDO. "TOTALIZADOR_BRL.
    TOTALIZADOR_DOLAR_FORMATADO = TOTALIZADOR_DOLAR."SALDO2.

    PERFORM F_FORMAT_VALUE(ZFIR075) CHANGING  TOTALIZADOR_BRL_FORMATADO.
    PERFORM F_FORMAT_VALUE(ZFIR075) CHANGING TOTALIZADOR_DOLAR_FORMATADO.


    LT_MAILTXT = '<tr>'.
    APPEND LT_MAILTXT.

    LT_MAILTXT = '<th style="text-align:right;" colspan="10">Total Geral:</th>'.
    APPEND LT_MAILTXT.

    CONCATENATE '<th style="text-align:right;">'TOTALIZADOR_BRL_FORMATADO'</th>' INTO LT_MAILTXT.
    APPEND LT_MAILTXT.

    CONCATENATE '<th style="text-align:right;">'TOTALIZADOR_DOLAR_FORMATADO'</th>' INTO LT_MAILTXT.
    APPEND LT_MAILTXT.
    LT_MAILTXT = '</tr>'.
    APPEND LT_MAILTXT.

    LT_MAILTXT = '</div>'.
    APPEND LT_MAILTXT.


    LT_MAILTXT = '</body></html>'.
    APPEND LT_MAILTXT.

    IF ENVIAR  = 'X'.
      "PERFORM ENVIAR.
      PERFORM MONTA_ENVIO TABLES LT_MAILTXT USING W_ZFIT0157-EMAIL.
*      W_ZFIT0157-EMAIL
    ENDIF.
    CLEAR: W_SAIDA, LT_MAILRECIPIENTES, LT_MAILTXT, TOTALIZADOR_BRL_FORMATADO, TOTALIZADOR_BRL, TOTALIZADOR_DOLAR_FORMATADO, TOTALIZADOR_DOLAR.
    FREE: LT_MAILRECIPIENTES, LT_MAILTXT.

  ENDLOOP.

ENDFORM.


FORM ENVIAR.
  " BREAK-POINT.
  DATA: VUSER         TYPE SY-UNAME.

  VUSER = SY-UNAME.
  SY-UNAME = 'JOBADM'.

  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      DOCUMENT_DATA              = LT_MAILSUBJECT
      DOCUMENT_TYPE              = 'HTM'
    TABLES
      OBJECT_CONTENT             = LT_MAILTXT
      RECEIVERS                  = LT_MAILRECIPIENTES
    EXCEPTIONS
      TOO_MANY_RECEIVERS         = 1
      DOCUMENT_NOT_SENT          = 2
      DOCUMENT_TYPE_NOT_EXIST    = 3
      OPERATION_NO_AUTHORIZATION = 4
      PARAMETER_ERROR            = 5
      X_ERROR                    = 6
      ENQUEUE_ERROR              = 7
      OTHERS                     = 8.

  SY-UNAME = VUSER.

  IF SY-SUBRC EQ 0.

    COMMIT WORK.

  ENDIF.
ENDFORM.

FORM MONTA_ENVIO TABLES V_HTML  USING V_EMAIL.

  DATA: LS_TYPE        TYPE SOOD-OBJTP,
        LV_DATE        TYPE CHAR10,
        WL_EMAIL       TYPE ADR6-SMTP_ADDR,
        LV_SUB         TYPE SO_OBJ_DES,
        LO_DOCUMENT    TYPE REF TO CL_DOCUMENT_BCS,
        LO_BCS         TYPE REF TO CL_BCS,
        LO_SAPUSER_BCS TYPE REF TO CL_SAPUSER_BCS,
        LO_RECIPIENT   TYPE REF TO IF_RECIPIENT_BCS,
        LO_EX_BCS      TYPE REF TO CX_BCS,
        LV_MESSAGE     TYPE STRING,
        MANDT          TYPE CHAR20,
        VUSER          TYPE SY-UNAME.

*  SELECT *
*    FROM ZMAIL
*    INTO TABLE @DATA(T_ZMAIL)
*    WHERE TCODE EQ @SY-TCODE
*      AND BUKRS EQ @V_BUKRS.

*  LOOP AT T_ZMAIL INTO DATA(W_ZMAIL).
  DATA Z_HTML TYPE ZSTRING.

  APPEND LINES OF V_HTML TO Z_HTML.

  MOVE: 'HTML'          TO LS_TYPE,
         V_EMAIL TO WL_EMAIL.

  CLEAR: LO_DOCUMENT, VUSER.

  MANDT = | #Ambiente de { SY-SYSID }#|.

  IF SY-SYSID EQ 'PRD'.
    CLEAR MANDT.
  ENDIF.

  LV_SUB = |Partidas Vencidas de Clientes { MANDT }|.



  LO_DOCUMENT =
  CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
                                    I_TYPE = 'HTM'
                                    I_SUBJECT = LV_SUB
                                    I_TEXT = Z_HTML
                                   ).

  LO_BCS = CL_BCS=>CREATE_PERSISTENT( ).
  LO_BCS->SET_DOCUMENT( LO_DOCUMENT ).
  LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( WL_EMAIL ).
  LO_BCS->SET_MESSAGE_SUBJECT( IP_SUBJECT = CONV #( LV_SUB ) ).

  TRY.
      CALL METHOD LO_BCS->ADD_RECIPIENT
        EXPORTING
          I_RECIPIENT = LO_RECIPIENT
          I_EXPRESS   = 'X'.
    CATCH CX_SEND_REQ_BCS.
  ENDTRY.

  VUSER = SY-UNAME.
  SY-UNAME = 'JOBADM'.

  LO_SAPUSER_BCS = CL_SAPUSER_BCS=>CREATE( SY-UNAME ).
  LO_BCS->SET_SENDER( I_SENDER = LO_SAPUSER_BCS ).
  LO_BCS->SET_SEND_IMMEDIATELY( 'X' ).

  TRY.

      CALL METHOD LO_BCS->SEND( ).
      COMMIT WORK.

      SY-UNAME = VUSER.
      "MESSAGE 'Mensagem enviada com Sucesso!' TYPE 'S'.

    CATCH CX_BCS INTO LO_EX_BCS.
      SY-UNAME = VUSER.
      LV_MESSAGE = LO_EX_BCS->GET_TEXT( ).
  ENDTRY.

*  ENDLOOP.
  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE 'Email não cadastrado!' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  f_format-value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VALOR    STRING
*----------------------------------------------------------------------*
FORM F_FORMAT_VALUE CHANGING P_VALOR.

  DATA:
    OBJ_JS    TYPE REF TO CL_JAVA_SCRIPT,
    MY_SCRIPT TYPE STRING.

  CONCATENATE 'function numeroParaMoeda(n, c, d, t)'
              '{'
              'c = isNaN(c = Math.abs(c)) ? 2 : c,'
              'd = d == undefined ? "," : d,'
              't = t == undefined ? "." : t,'
              's = n < 0 ? "-" : "",'
              'i = parseInt(n = Math.abs(+n || 0).toFixed(c)) + "",'
              'j = (j = i.length) > 3 ? j % 3 : 0;'
              'return s + (j ? i.substr(0, j) + t : "") +'
              'i.substr(j).replace(/(\d{3})(?=\d)/g, "$1" + t) +'
              '(c ? d + Math.abs(n - i).toFixed(c).slice(2) : "");'
              '}'
              'var vlr = numeroParaMoeda (' P_VALOR ')'
              INTO MY_SCRIPT SEPARATED BY CL_ABAP_CHAR_UTILITIES=>CR_LF.

  OBJ_JS = CL_JAVA_SCRIPT=>CREATE( ).

  OBJ_JS->COMPILE( EXPORTING SCRIPT_NAME = 'script.js'
                             SCRIPT      = MY_SCRIPT ).

  OBJ_JS->EXECUTE( EXPORTING SCRIPT_NAME = 'script.js' ).
  P_VALOR = OBJ_JS->GET( NAME = 'vlr').

ENDFORM.                    "f_format-value

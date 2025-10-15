*&---------------------------------------------------------------------*
*& Report  ZLESR0022
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZLESR0022.

TABLES: VTTK.

TYPES : BEGIN OF TY_REG,
          PARID TYPE KNA1-KUNNR,
          ERDAT TYPE VTTK-ERDAT,
        END OF TY_REG.

TYPES : BEGIN OF Y_FILE,
          LINHA(400),
        END OF Y_FILE.

DATA : T_VTTK  TYPE TABLE OF VTTK,
       T_VTPA  TYPE TABLE OF VTPA,
       T_LFA1  TYPE TABLE OF LFA1,
       T_KNA1  TYPE TABLE OF KNA1,
       T_ADR6  TYPE TABLE OF ADR6,
       T_T001K TYPE TABLE OF T001K,
       T_FILE  TYPE STANDARD TABLE OF Y_FILE WITH HEADER LINE INITIAL SIZE 0,
       T_REG   TYPE TABLE OF TY_REG WITH HEADER LINE.

DATA : WA_VTTK  TYPE VTTK,
       WA_VTPA  TYPE VTPA,
       WA_LFA1  TYPE LFA1,
       WA_KNA1  TYPE KNA1,
       WA_T001K TYPE T001K,
       WA_ADR6  TYPE ADR6,
       WA_FILE  TYPE Y_FILE .

*----------------------------------------------------------------------*
* Tela de Seleção                                                      *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK A WITH FRAME TITLE TEXT-001.

PARAMETERS: P_FILE TYPE STRING DEFAULT 'c:\Arquivos\'.
SELECT-OPTIONS: P_ERDAT FOR VTTK-ERDAT .

SELECTION-SCREEN END OF BLOCK A.

START-OF-SELECTION.
  PERFORM : ZSELECIONA_DADOS,
            ZSAIDA .
*&---------------------------------------------------------------------*
*&      Form  ZSELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZSELECIONA_DADOS .

  REFRESH : T_VTTK, T_VTPA, T_LFA1, T_KNA1, T_T001K,  T_ADR6.

  SELECT *
    FROM VTTK
    INTO TABLE T_VTTK
   WHERE ERDAT IN P_ERDAT.

  IF T_VTTK[] IS NOT INITIAL.

    SELECT *
      FROM T001K
      INTO TABLE T_T001K
       FOR ALL ENTRIES IN T_VTTK
     WHERE BWKEY EQ T_VTTK-TPLST.

    SELECT *
      FROM VTPA
      INTO TABLE T_VTPA
       FOR ALL ENTRIES IN T_VTTK
     WHERE VBELN EQ T_VTTK-TKNUM
       AND PARVW EQ 'PV' .

    IF T_VTPA[] IS NOT INITIAL.

      SELECT *
        FROM LFA1
        INTO TABLE T_LFA1
         FOR ALL ENTRIES IN T_VTPA
       WHERE LIFNR EQ T_VTPA-LIFNR.

      SELECT *
        FROM KNA1
        INTO TABLE T_KNA1
         FOR ALL ENTRIES IN T_VTPA
       WHERE KUNNR EQ T_VTPA-KUNNR.

      IF T_LFA1[] IS NOT INITIAL.
        SELECT *
          FROM ADR6 APPENDING TABLE T_ADR6
           FOR ALL ENTRIES IN T_LFA1
         WHERE ADDRNUMBER EQ T_LFA1-ADRNR.
      ENDIF.

      IF T_KNA1[] IS NOT INITIAL.

        SELECT *
          FROM ADR6 APPENDING TABLE T_ADR6
           FOR ALL ENTRIES IN T_KNA1
         WHERE ADDRNUMBER EQ T_KNA1-ADRNR.

      ENDIF.

    ENDIF.


  ENDIF.

ENDFORM.                    " SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  ZSAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZSAIDA .
  PERFORM : ZARQ_PARCEIRO,
            ZARQ_ENDERECO.

ENDFORM.                    " ZSAIDA


*&---------------------------------------------------------------------*
*&      Form  ZLPAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_VL_VALOR  text
*----------------------------------------------------------------------*
FORM ZLPAD  USING P_PADL
                  P_TAMANHO
         CHANGING P_VALOR.

  DATA : VL_LEN   TYPE I,
         VL_VALOR TYPE STRING.

  VL_VALOR = P_VALOR.

  VL_LEN =  STRLEN( VL_VALOR ).

  WHILE P_TAMANHO > VL_LEN .

    CONCATENATE P_PADL P_VALOR INTO VL_VALOR RESPECTING BLANKS.

    P_VALOR = VL_VALOR.

    VL_LEN =  STRLEN( VL_VALOR ).

  ENDWHILE.

ENDFORM.                    " ZLPAD

*&---------------------------------------------------------------------*
*&      Form  ZLPAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_VL_VALOR  text
*----------------------------------------------------------------------*
FORM ZRPAD  USING P_PADL
                  P_TAMANHO
         CHANGING P_VALOR.

  DATA : VL_LEN   TYPE I,
         VL_VALOR TYPE STRING.

  VL_VALOR = P_VALOR.

  VL_LEN =  STRLEN( VL_VALOR ).

  WHILE P_TAMANHO > VL_LEN .

    CONCATENATE P_VALOR P_PADL INTO VL_VALOR RESPECTING BLANKS.

    P_VALOR = VL_VALOR.

    VL_LEN =  STRLEN( VL_VALOR ).

  ENDWHILE.
  CONCATENATE VL_VALOR '"' INTO P_VALOR RESPECTING BLANKS.
ENDFORM.                    " ZLPAD


*&---------------------------------------------------------------------*
*&      Form  F_FILE_F4
*&---------------------------------------------------------------------*
FORM F_FILE_F4  CHANGING PO_PATH.

  DATA: LT_FILETABLE TYPE FILETABLE,
        LF_RC        TYPE I.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      MULTISELECTION          = ABAP_FALSE
    CHANGING
      FILE_TABLE              = LT_FILETABLE
      RC                      = LF_RC
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
               DISPLAY LIKE 'E'
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.
* Number of selected filed must be equal to one.
  CHECK LF_RC = 1.
* Access selected file
  DATA:
    LS_FILE TYPE FILE_TABLE.
  READ TABLE LT_FILETABLE INTO LS_FILE INDEX 1.
  CHECK SY-SUBRC = 0.

  PO_PATH = LS_FILE-FILENAME.

ENDFORM.                    "f_file_f4
*&---------------------------------------------------------------------*
*&      Form  ZARQ_PARCEIRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZARQ_PARCEIRO .

  DATA : VL_NOME    TYPE LFA1-NAME1,
         VL_CGC_CPF TYPE LFA1-STCD1,
         VL_LINHA   TYPE STRING,
         VL_VALOR   TYPE STRING,
         VL_PARID   LIKE T_REG-PARID,
         VL_ERDAT   LIKE T_REG-ERDAT.

  SORT : T_VTPA  BY VBELN,
         T_LFA1  BY LIFNR,
         T_KNA1  BY KUNNR,
         T_T001K BY BWKEY.

  REFRESH : T_FILE, T_REG.

  LOOP AT T_VTTK INTO WA_VTTK.

    READ TABLE T_T001K INTO WA_T001K WITH KEY BWKEY = WA_VTTK-TPLST BINARY SEARCH.

    LOOP AT T_VTPA INTO WA_VTPA WHERE VBELN = WA_VTTK-TKNUM.

      CLEAR: T_REG, VL_PARID, VL_ERDAT.

      "Empresa
      CONCATENATE WA_T001K-BUKRS '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      VL_LINHA = WA_FILE-LINHA.
      "Filial
      CONCATENATE VL_LINHA WA_VTTK-TPLST '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "Codigo do parceiro
      IF WA_VTPA-LIFNR IS NOT INITIAL.

        READ TABLE T_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_VTPA-LIFNR BINARY SEARCH.
        VL_LINHA = WA_FILE-LINHA.
        VL_VALOR = WA_VTPA-LIFNR.

        PERFORM RETIRA_ZEROS CHANGING VL_VALOR.

        PERFORM ZRPAD  USING ' ' 60 CHANGING VL_VALOR.
        CONCATENATE VL_LINHA VL_VALOR '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

        VL_NOME    = WA_LFA1-NAME1.
        VL_CGC_CPF = WA_LFA1-STCD1.

        VL_PARID   = WA_VTPA-LIFNR.

      ELSE.

        READ TABLE T_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_VTPA-KUNNR BINARY SEARCH.
        VL_LINHA = WA_FILE-LINHA.
        VL_VALOR = WA_VTPA-KUNNR.

        PERFORM RETIRA_ZEROS CHANGING VL_VALOR.

        PERFORM ZRPAD  USING ' ' 60 CHANGING VL_VALOR.
        CONCATENATE VL_LINHA VL_VALOR '|' INTO WA_FILE-LINHA RESPECTING BLANKS."Codigo do parceiro

        VL_NOME    = WA_KNA1-NAME1.
        VL_CGC_CPF = WA_KNA1-STCD1.

        VL_PARID   = WA_VTPA-KUNNR.

      ENDIF.

      "Data que o parceiro foi utilizado
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA  WA_VTTK-ERDAT+6(2) WA_VTTK-ERDAT+4(2) WA_VTTK-ERDAT(4) '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      VL_ERDAT = WA_VTTK-ERDAT.

      "Tipo de Pessoa Fisica/Juridica
      VL_LINHA = WA_FILE-LINHA.
      IF STRLEN( VL_CGC_CPF ) > 11 .
        CONCATENATE VL_LINHA 'J' '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      ELSE.
        CONCATENATE VL_LINHA 'F' '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      ENDIF.

      "Razão Social
      VL_LINHA = WA_FILE-LINHA.
      VL_VALOR = VL_NOME .
      PERFORM ZRPAD  USING ' ' 70 CHANGING VL_VALOR.
      CONCATENATE VL_LINHA VL_VALOR '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "Tp_Estabelecimento
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '06' '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "CNPJ / CPF Parceiro
      VL_LINHA = WA_FILE-LINHA.
      VL_VALOR = VL_NOME.
      PERFORM ZRPAD  USING ' ' 14  CHANGING VL_VALOR.
      CONCATENATE VL_LINHA VL_CGC_CPF '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "Nome Fantasia
      VL_LINHA = WA_FILE-LINHA.
      VL_VALOR = VL_NOME .
      PERFORM ZRPAD  USING ' ' 60  CHANGING VL_VALOR.
      CONCATENATE VL_LINHA VL_VALOR '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "Itens que não são necessários
      "10
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      "11
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      "12
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      "13
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      "14
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      "15
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      "16
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      "17
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      "18
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      "19
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      "20
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      "21
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      "22
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      "23
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      "24
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      "25
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      "26
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      "27
      VL_LINHA = WA_FILE-LINHA.

      "Verifica Duplicidade Linha Arquivo.
      READ TABLE T_REG WITH KEY PARID = VL_PARID.
      IF SY-SUBRC = 0.
        CONTINUE.
      ELSE.
        T_REG-PARID = VL_PARID.
        T_REG-ERDAT = VL_ERDAT.
        APPEND T_REG.
      ENDIF.

      REPLACE ALL OCCURRENCES OF '"' IN WA_FILE-LINHA  WITH SPACE.
      APPEND WA_FILE TO T_FILE.

    ENDLOOP.
  ENDLOOP.

  PERFORM ZGRAVA_ARQUIVO USING 'PARCVT03N.TXT'.

ENDFORM.                    " ZARQ_PARCEIRO
*&---------------------------------------------------------------------*
*&      Form  ZARQ_ENDERECO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZARQ_ENDERECO .

  DATA : VL_NOME    TYPE LFA1-NAME1,
         VL_CGC_CPF TYPE LFA1-STCD1,
         VL_LINHA   TYPE STRING,
         VL_VALOR   TYPE STRING,

         VL_TEL     TYPE LFA1-TELF1,
         VL_EMAIL   TYPE ADR6-SMTP_ADDR,

         VL_IE      TYPE LFA1-STCD3,
         VL_LOGADOURO TYPE LFA1-STRAS,
         VL_NUMERO  TYPE LFA1-ADRNR,
         VL_BAIRRO  TYPE LFA1-ORT02,
         VL_MUNICIPIO TYPE LFA1-ORT01,
         VL_CEP     TYPE LFA1-PSTLZ,
         VL_ESTADO  TYPE LFA1-REGIO,
         VL_PAIS    TYPE LFA1-LAND1,
         VL_COD_MUN TYPE LFA1-TXJCD,
         VL_PARID   LIKE T_REG-PARID,
         VL_ERDAT   LIKE T_REG-ERDAT,
         LS_TTXD    TYPE TTXD.


  SORT : T_VTPA  BY VBELN,
         T_LFA1  BY LIFNR,
         T_KNA1  BY KUNNR,
         T_T001K BY BWKEY.

  REFRESH : T_FILE,T_REG.

  LOOP AT T_VTTK INTO WA_VTTK.

    READ TABLE T_T001K INTO WA_T001K WITH KEY BWKEY = WA_VTTK-TPLST BINARY SEARCH.

    LOOP AT T_VTPA INTO WA_VTPA WHERE VBELN = WA_VTTK-TKNUM.

      CLEAR: T_REG, VL_PARID, VL_ERDAT, VL_EMAIL,
             VL_LINHA, VL_VALOR, VL_NOME, VL_CGC_CPF,
             VL_PAIS, VL_LOGADOURO, VL_NUMERO, VL_BAIRRO,
             VL_MUNICIPIO, VL_CEP, VL_ESTADO, VL_COD_MUN,
             VL_PARID,VL_IE,VL_TEL.

      "1 - Empresa
      CONCATENATE WA_T001K-BUKRS '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      VL_LINHA = WA_FILE-LINHA.
      "2 - Filial
      CONCATENATE VL_LINHA WA_VTTK-TPLST '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "3 - Codigo do parceiro
      IF WA_VTPA-LIFNR IS NOT INITIAL.

        READ TABLE T_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_VTPA-LIFNR BINARY SEARCH.
        VL_LINHA = WA_FILE-LINHA.
        VL_VALOR = WA_VTPA-LIFNR.

        PERFORM RETIRA_ZEROS CHANGING VL_VALOR.

        PERFORM ZRPAD  USING ' ' 60 CHANGING VL_VALOR.
        CONCATENATE VL_LINHA VL_VALOR '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

        VL_NOME      = WA_LFA1-NAME1.
        VL_CGC_CPF   = WA_LFA1-STCD1.

        VL_PAIS      = WA_LFA1-LAND1.
        VL_LOGADOURO = WA_LFA1-STRAS.
        "VL_NUMERO    = WA_LFA1-ADRNR.
        VL_BAIRRO    = WA_LFA1-ORT02.
        VL_MUNICIPIO = WA_LFA1-ORT01.
        VL_CEP       = WA_LFA1-PSTLZ.
        VL_ESTADO    = WA_LFA1-REGIO.
        VL_COD_MUN   = WA_LFA1-TXJCD+3(12).
        VL_PARID     = WA_VTPA-LIFNR.
        VL_IE        = WA_LFA1-STCD3.
        VL_TEL       = WA_LFA1-TELF1.

        READ TABLE T_ADR6 INTO WA_ADR6 WITH KEY ADDRNUMBER = WA_LFA1-ADRNR.
        IF SY-SUBRC = 0.
          VL_EMAIL = WA_ADR6-SMTP_ADDR.
        ENDIF.

      ELSE.

        READ TABLE T_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_VTPA-KUNNR BINARY SEARCH.
        VL_LINHA = WA_FILE-LINHA.
        VL_VALOR = WA_VTPA-KUNNR.

        PERFORM RETIRA_ZEROS CHANGING VL_VALOR.

        PERFORM ZRPAD  USING ' ' 60 CHANGING VL_VALOR.
        CONCATENATE VL_LINHA VL_VALOR '|' INTO WA_FILE-LINHA RESPECTING BLANKS."Codigo do parceiro

        VL_NOME      = WA_KNA1-NAME1.
        VL_CGC_CPF   = WA_KNA1-STCD1.


        VL_LOGADOURO = WA_KNA1-STRAS.
        "VL_NUMERO    = WA_KNA1-ADRNR.
        VL_BAIRRO    = WA_KNA1-ORT02.
        VL_MUNICIPIO = WA_KNA1-ORT01.
        VL_CEP       = WA_KNA1-PSTLZ.
        VL_ESTADO    = WA_KNA1-REGIO.
        VL_PAIS      = WA_KNA1-LAND1.
        VL_COD_MUN   = WA_KNA1-TXJCD+3(12).
        VL_PARID     = WA_VTPA-KUNNR.
        VL_IE        = WA_KNA1-STCD3.
        VL_TEL       = WA_KNA1-TELF1.

        READ TABLE T_ADR6 INTO WA_ADR6 WITH KEY ADDRNUMBER = WA_KNA1-ADRNR.
        IF SY-SUBRC = 0.
          VL_EMAIL = WA_ADR6-SMTP_ADDR.
        ENDIF.

      ENDIF.

      "4 - Data que o parceiro foi utilizado
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA  WA_VTTK-ERDAT+6(2) WA_VTTK-ERDAT+4(2) WA_VTTK-ERDAT(4) '|' INTO WA_FILE-LINHA RESPECTING BLANKS.
      VL_ERDAT = WA_VTTK-ERDAT.

      "5 - IPE_CODIGO
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA 'F' '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "6 - Logadouro
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA  VL_LOGADOURO '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "7 - Numero
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA  VL_NUMERO '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "8 - Bairro
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA  VL_BAIRRO '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "9 - Municipio
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA  VL_MUNICIPIO '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "10
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "11 - CEP
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA VL_CEP '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "12 - UF
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA  VL_ESTADO '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "13 - Pais
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA  VL_PAIS '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "14 - Codigo Municipio
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA VL_COD_MUN '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "15
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "16 - Inscrição Estadual
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA VL_IE '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "17
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "18
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "19 - Nro. Telefone
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA VL_TEL '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "20
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA '|' INTO WA_FILE-LINHA RESPECTING BLANKS.

      "21
      VL_LINHA = WA_FILE-LINHA.
      CONCATENATE VL_LINHA VL_EMAIL INTO WA_FILE-LINHA RESPECTING BLANKS.

      "Verifica Duplicidade Linha Arquivo.
      READ TABLE T_REG WITH KEY PARID = VL_PARID.
      IF SY-SUBRC = 0.
        CONTINUE.
      ELSE.
        T_REG-PARID = VL_PARID.
        T_REG-ERDAT = VL_ERDAT.
        APPEND T_REG.
      ENDIF.

      REPLACE ALL OCCURRENCES OF '"' IN WA_FILE-LINHA WITH SPACE.
      APPEND WA_FILE TO T_FILE.

    ENDLOOP.
  ENDLOOP.

  PERFORM ZGRAVA_ARQUIVO USING 'ENDPARCVT03N.TXT'.
ENDFORM.                    " ZARQ_ENDERECO

*&---------------------------------------------------------------------*
*&      Form  ZGRAVA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZGRAVA_ARQUIVO USING VL_FILE TYPE STRING.
  DATA : VL_FILENAME TYPE STRING.

  CONCATENATE P_FILE '\' VL_FILE  INTO VL_FILENAME .


  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                = VL_FILENAME
    TABLES
      DATA_TAB                = T_FILE[]
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      OTHERS                  = 22.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " ZGRAVA_ARQUIVO

FORM RETIRA_ZEROS CHANGING P_VALOR.

  DATA: VL_VALOR_AUX TYPE P.

  TRY.
    VL_VALOR_AUX = P_VALOR.
    P_VALOR      = VL_VALOR_AUX.
    CONDENSE P_VALOR NO-GAPS.
  CATCH CX_SY_CONVERSION_NO_NUMBER.
  CATCH CX_SY_CONVERSION_OVERFLOW.
  ENDTRY.

ENDFORM.

FUNCTION ZNFE_INBOUND_REVISAO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_USER) TYPE  UNAME
*"  EXPORTING
*"     REFERENCE(I_HTML) TYPE  STRING
*"     REFERENCE(I_URL) TYPE  STRING
*"----------------------------------------------------------------------

  CLEAR: I_HTML, I_URL.

  CHECK I_USER IS NOT INITIAL.

  SELECT * INTO TABLE @DATA(IT_REVISAO)
    FROM ZIB_NFE_DIST_REV
   WHERE CK_ULTIMO   EQ @ABAP_TRUE
     AND CK_REVISADO EQ @ABAP_FALSE.

  CHECK SY-SUBRC IS INITIAL.

  "SELECT * INTO TABLE @DATA(IT_NFE)
  "  FROM ZIB_NFE_DIST_TER
  "   FOR ALL ENTRIES IN @IT_REVISAO
  " WHERE CHAVE_NFE  EQ @IT_REVISAO-CHAVE_NFE
  "   AND E_TOMADORA NE @SPACE
  "   AND F_TOMADORA NE @SPACE.

  SELECT DISTINCT
         E~BUTXT,
         B~NAME,
         R~DT_REGISTRO,
         R~HR_REGISTRO,
         R~CHAVE_NFE,
         T~FORNE_RAZAO,
         P~EBELN,
         T~E_TOMADORA,
         T~F_TOMADORA,
         T~CD_DEPARTAMENTO,
         T~SE_RECORDID
    INTO TABLE @DATA(IT_NFE)
    FROM ZIB_NFE_DIST_REV AS R
   INNER JOIN ZIB_NFE_DIST_TER AS T ON R~CHAVE_NFE EQ T~CHAVE_NFE
   LEFT JOIN ZIB_NFE_DIST_PED AS P ON P~CHAVE_NFE EQ R~CHAVE_NFE
   LEFT JOIN J_1BBRANCH AS B ON B~BUKRS EQ T~E_TOMADORA AND B~BRANCH EQ T~F_TOMADORA
   LEFT JOIN T001 AS E ON E~BUKRS EQ T~E_TOMADORA
   WHERE R~CK_ULTIMO    = 'X'
     AND R~CK_REVISADO  = ' '
     AND T~E_TOMADORA  <> ' '
     AND T~F_TOMADORA  <> ' '
   ORDER BY T~E_TOMADORA, T~F_TOMADORA, R~DT_REGISTRO, R~HR_REGISTRO.

  CHECK SY-SUBRC IS INITIAL.

  DATA(IT_COPIA) = IT_NFE[].

  SORT IT_NFE BY E_TOMADORA F_TOMADORA CD_DEPARTAMENTO.
  DELETE ADJACENT DUPLICATES FROM IT_NFE COMPARING E_TOMADORA F_TOMADORA CD_DEPARTAMENTO.

  LOOP AT IT_NFE INTO DATA(WA_NFE).

    AUTHORITY-CHECK OBJECT 'ZNFE_INB'
     FOR USER I_USER
      ID 'ZANFETER'   FIELD '07'
      ID 'ZNFETERMEP' FIELD WA_NFE-E_TOMADORA
      ID 'ZNFETERFIL' FIELD WA_NFE-F_TOMADORA
      ID 'ZNFETERDEP' FIELD WA_NFE-CD_DEPARTAMENTO.

    IF SY-SUBRC IS NOT INITIAL.
      LOOP AT IT_NFE INTO DATA(WA_NFE1) WHERE E_TOMADORA = WA_NFE-E_TOMADORA AND F_TOMADORA = WA_NFE-F_TOMADORA AND CD_DEPARTAMENTO = WA_NFE-CD_DEPARTAMENTO.
        DELETE IT_REVISAO WHERE CHAVE_NFE EQ WA_NFE1-CHAVE_NFE.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

  CHECK IT_REVISAO[] IS NOT INITIAL.
  SORT IT_NFE BY CHAVE_NFE.
  SORT IT_REVISAO BY CHAVE_NFE.
  "I_HTML = '<h2>Documentos em Revisão</h2>'.

  DATA: LC_EMPRESA TYPE J_1BBRANCH-BUKRS,
        LC_FILIAL  TYPE J_1BBRANCH-BRANCH.

  CLEAR: LC_EMPRESA, LC_FILIAL.

  I_HTML = '<!DOCTYPE html> <html> <head> <meta name="viewport" content="width=device-width, initial-scale=1"> <style> ' &&
           ' .collapsible { background-color: #777; color: white; cursor: pointer; padding: 18px; width: 100%; border: none; text-align: left; outline: none; font-size: 15px; }' &&
           ' .active, .collapsible:hover { background-color: #555; } ' &&
           ' .content { padding: 0 18px; display: none; overflow: hidden; background-color: #f1f1f1; } ' &&
           ' </style> ' &&
           ' </head>  ' &&
           ' <body>   '.


  LOOP AT IT_COPIA INTO DATA(WA_COPIA).

    READ TABLE IT_REVISAO INTO DATA(WA_REVISAO) WITH KEY CHAVE_NFE = WA_COPIA-CHAVE_NFE BINARY SEARCH.
    IF SY-SUBRC IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    IF LC_EMPRESA IS INITIAL AND LC_FILIAL IS INITIAL.
      DATA(LC_QUEBRA) = ABAP_TRUE.
    ELSEIF LC_EMPRESA NE WA_COPIA-E_TOMADORA OR LC_FILIAL NE WA_COPIA-F_TOMADORA.
      LC_QUEBRA = ABAP_TRUE.
    ELSE.
      LC_QUEBRA = ABAP_FALSE.
    ENDIF.

    IF LC_QUEBRA EQ ABAP_TRUE.
      IF LC_EMPRESA IS NOT INITIAL.
        I_HTML = I_HTML && |</div> </p>|.
      ENDIF.
      LC_EMPRESA = WA_COPIA-E_TOMADORA.
      LC_FILIAL  = WA_COPIA-F_TOMADORA.

      DATA(LC_TEXTO) = |{ WA_COPIA-E_TOMADORA } - { WA_COPIA-BUTXT } > Filial: { WA_COPIA-F_TOMADORA } - { WA_COPIA-NAME }|.

      IT_NFE[] = IT_COPIA[].
      delete IT_NFE WHERE E_TOMADORA <> WA_COPIA-E_TOMADORA.
      delete IT_NFE WHERE F_TOMADORA <> WA_COPIA-F_TOMADORA.
      DESCRIBE TABLE IT_NFE LINES DATA(QT_REGISTROS).


      I_HTML = I_HTML && |<button type="button" class="collapsible">{ ZCL_STRING=>INITIALCAP( LC_TEXTO ) } - ({ QT_REGISTROS }) </button>|.
      I_HTML = I_HTML && |<div class="content">|.
    ENDIF.

    I_HTML = I_HTML && || && '<div>' &&
             '<h2><p>' && ZCL_STRING=>INITIALCAP( I_STR = CONV #( WA_COPIA-FORNE_RAZAO ) ) && '</p></h2>' &&
             '<p><a href="' && WA_COPIA-CHAVE_NFE && '">' &&
             '<A HREF=SAPEVENT:' && WA_COPIA-CHAVE_NFE && '>' && WA_COPIA-CHAVE_NFE && '</A></a></p>' &&
             '<p>Pedido de Compra: ' && WA_COPIA-EBELN && '</p>' &&
             '<p>Solicitação de Miro (SM): ' && WA_COPIA-SE_RECORDID && '</p>' &&
             '<p><b><i>' && WA_REVISAO-DS_MOTIVO && '</i></b></p></div>'.

  ENDLOOP.

  IF LC_EMPRESA IS NOT INITIAL.
    I_HTML = I_HTML && |</div>|.
  ENDIF.


  I_HTML = I_HTML && ' <script> var coll = document.getElementsByClassName("collapsible"); var i; ' &&
           ' for (i = 0; i < coll.length; i++) { coll[i].addEventListener("click", function() { this.classList.toggle("active"); var content = this.nextElementSibling; ' &&
           ' if (content.style.display === "block") { content.style.display = "none"; } else { content.style.display = "block"; } }); } </script> ' &&
           '</body>' &&
           '</html>'.

  "LOOP AT IT_REVISAO INTO DATA(WA_REVISAO).
  "  READ TABLE IT_COPIA INTO WA_NFE WITH KEY CHAVE_NFE = WA_REVISAO-CHAVE_NFE BINARY SEARCH."
  "
  "  IF SY-SUBRC IS NOT INITIAL.
  "    CONTINUE.
  "  ENDIF.
  "
  "  I_HTML = ZCL_STRING=>CONCAT(
  "           S1 = I_HTML
  "           S2 = '<div style="background-color:Orange;color:black;padding:20px;">' &&
  "                '  <h2><p>' && ZCL_STRING=>INITIALCAP( I_STR = CONV #( WA_NFE-FORNE_RAZAO ) ) && '</p></h2>' &&
  "                '  <p><a href="' && WA_NFE-CHAVE_NFE && '"><A HREF=SAPEVENT:'&& WA_NFE-CHAVE_NFE && '>' && WA_NFE-CHAVE_NFE && '</A></a></p>' &&
  "                '  <p><i>' && WA_REVISAO-DS_MOTIVO &&  '</i></p>' &&
  "                '</div> </b>' ).
  "ENDLOOP.

  "I_HTML = ZCL_STRING=>HTML_STRING_INTO_HTML( I_STR = I_HTML I_TXT_HEADER_TITLE = 'Documentos em Revisão' ).

  DATA:  HTML_TABLE TYPE TABLE OF W3HTML.

  CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
    EXPORTING
      I_STRING         = I_HTML
      I_TABLINE_LENGTH = 255
    TABLES
      ET_TABLE         = HTML_TABLE.

  DATA: LC_URL TYPE C LENGTH 200.

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      TYPE                 = 'text'
      SUBTYPE              = 'html'
    TABLES
      DATA                 = HTML_TABLE
    CHANGING
      URL                  = LC_URL
    EXCEPTIONS
      DP_INVALID_PARAMETER = 1
      DP_ERROR_PUT_TABLE   = 2
      DP_ERROR_GENERAL     = 3
      OTHERS               = 4.

  I_URL = LC_URL.

ENDFUNCTION.

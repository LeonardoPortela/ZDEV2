*=============================================================================*
* Report  ZSDR0198 cópia modificada do ZSDR0061                               *
*                                                                             *
*=============================================================================*
*&---------------------------------------------------------------------------&*
*&                    Histórico de Modificações                              &*
*& Autor ABAP |Request    |Data       |Descrição                             &*
*&---------------------------------------------------------------------------&*
*& RGARCIA   |DEVK9A2HNK |07/04/2025 |Ajuste diversos no cadastro de Roteiro.&*
*&                                   |Chamado: 171563.                       &*
*&---------------------------------------------------------------------------&*
*& NSEGATIN  |DEVK9A2RF1 |26/08/2025 |Ajst. na atribuição da Zona ao Roteiro.&*
*&                                   |Chamado: 187296.                       &*
*&---------------------------------------------------------------------------&*
REPORT zsdr0198.
*=============================================================================*
*TYPES                                                                        *
*=============================================================================*
TYPES: BEGIN OF ty_header,
         partn TYPE but000-partner,    "<<<------"187296 - NMS ------>>>
         kunnr TYPE kna1-kunnr,
         lifnr TYPE lfa1-lifnr,        "<<<------"187296 - NMS ------>>>
         name1 TYPE kna1-name1,
         city  TYPE kna1-ort01,
         cep   TYPE kna1-pstlz,
         bpzon TYPE adrc-transpzone,   "<<<------"187296 - NMS ------>>>
         lzone TYPE kna1-lzone,                             "FF #143815
         fnzon TYPE lfa1-lzone.        "<<<------"187296 - NMS ------>>>
TYPES: END OF ty_header.

TYPES: BEGIN OF ty_f4_filial,
         "CIDADE TYPE KNA1-ORT01,
         "CEP    TYPE KNA1-PSTLZ,
         "UF     TYPE kna1-regio,
         filial_resp TYPE tvkbt-vkbur,
         bezei       TYPE tvkbt-bezei.
TYPES: END OF ty_f4_filial.

TYPES: BEGIN OF ty_f4_grid,
         cep    TYPE kna1-pstlz,
         uf     TYPE kna1-regio,
         cidade TYPE kna1-ort01,
       END OF ty_f4_grid.

TYPES: BEGIN OF ty_zsdt0132_alv,

         aland             TYPE aland, "país
         azone             TYPE azone, "zona transp
         vtext             TYPE tzont-vtext, "desc. zona
         antig             TYPE char1,
         texto             TYPE char5,
         anexo             TYPE char5.
         INCLUDE STRUCTURE zsdt0132.
**<<<------"171563 - NMS - INI------>>>
TYPES:   zlatitude         TYPE zde_latitude,
         zlongitude        TYPE zde_longitude,
         z_url_localizacao TYPE zde_url_loc.
**<<<------"171563 - NMS - FIM------>>>
TYPES:   cellstyles TYPE lvc_t_styl.
TYPES: END OF ty_zsdt0132_alv.

TYPES: tty_itens TYPE TABLE OF ty_zsdt0132_alv.

"FF - inicio #143815

" Batch input nova estrutura do campo de tabela
TYPES: BEGIN OF ty_bdcdata,
         program  TYPE bdcdata-program,  " Pool de módulos BDC
         dynpro   TYPE bdcdata-dynpro,   " NÚmero de tela BDC
         dynbegin TYPE bdcdata-dynbegin, " Início BDC de uma tela
         fnam     TYPE bdcdata-fnam,     " Nome do campo
         fval     TYPE bdcdata-fval,     " Valor do campo BDC
       END OF ty_bdcdata,

       BEGIN OF ty_message,
         cliente TYPE rf02d-kunnr,        " Código do cliente
         msgty   TYPE message-msgty,      " Tipo da mensagem
         msgno   TYPE message-msgno,      " Numero da mensagem
         msgtx   TYPE message-msgtx,      " Descrição da mensagem
       END OF   ty_message
       .
" Estruturas ...
DATA:
      st_bdcdata TYPE ty_bdcdata.
.

" Tabelas Internas ....
DATA:
  it_bdcdata TYPE TABLE OF ty_bdcdata,
  it_msg     TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
  it_message TYPE TABLE OF ty_message.

" Variaveis ....
DATA: vg_mode(1)    TYPE c VALUE 'N', " informa o Modo do Call Transaction
      vg_texto(100) TYPE c,        " Texto para o Indicator
      vg_s          TYPE c VALUE 'S',       " Informa o Update do call Transaction
      mensg         LIKE message VALUE IS INITIAL, " variavel que recebe retorno
      msgno         LIKE sy-msgno,
      vg_erro       TYPE c.
"FF - fim

*=============================================================================*
*VARIÁVEIS                                                                    *
*=============================================================================*
DATA: it_zsdt0132  TYPE STANDARD TABLE OF ty_zsdt0132_alv,
      wa_zlest0153 TYPE zlest0153,
      wa_header    TYPE ty_header,
      vg_name1     TYPE kna1-name1,
      city         TYPE kna1-ort01,
      cep          TYPE kna1-pstlz,
      lzone        TYPE kna1-lzone.
*=============================================================================*
*SELECTION-SCREEN                                                             *
*=============================================================================*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_c RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND usr,
              p_f RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-001.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (15) vl_descc.
    PARAMETERS: p_kunnr LIKE kna1-kunnr MODIF ID c.
    SELECTION-SCREEN POSITION 35.
    SELECTION-SCREEN COMMENT (37) vl_namec.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (15) vl_descf.
    PARAMETERS: p_lifnr LIKE lfa1-lifnr MODIF ID f.
    SELECTION-SCREEN POSITION 35.
    SELECTION-SCREEN COMMENT (37) vl_namef.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.

  CLEAR: vl_namec, vl_namef, vl_descc, vl_descf.

  CASE abap_true.
    WHEN p_c.

      CLEAR p_lifnr.

      vl_descc = 'Cód. Cliente'.
      vl_descf = ''.

      SELECT SINGLE name1
        FROM kna1
        INTO vl_namec
        WHERE kunnr EQ p_kunnr.

      IF NOT sy-subrc IS INITIAL.
        vl_namec = ''.
      ELSE.

      ENDIF.

    WHEN p_f.

      CLEAR: p_kunnr.

      vl_descf = 'Cód. Fornecedor'.
      vl_descc = ''.

      SELECT SINGLE name1
        FROM lfa1
        INTO vl_namef
        WHERE lifnr EQ p_lifnr.

      IF NOT sy-subrc IS INITIAL.
        vl_namef = ''.
      ELSE.

      ENDIF.

  ENDCASE.

  LOOP AT SCREEN.
    CASE abap_true.
      WHEN p_c.
        CASE screen-name.
          WHEN 'VL_DESCC' OR 'P_KUNNR' OR 'VL_NAMEC'.
            screen-active = '1'.
            MODIFY SCREEN.
            CONTINUE.
          WHEN 'VL_DESCF' OR 'P_LIFNR' OR 'VL_NAMEF'.
            screen-active = '0'.
            MODIFY SCREEN.
            CONTINUE.
        ENDCASE.
      WHEN p_f.
        CASE screen-name.
          WHEN 'VL_DESCF' OR 'P_LIFNR' OR 'VL_NAMEF'.
            screen-active = '1'.
            MODIFY SCREEN.
            CONTINUE.
          WHEN 'VL_DESCC' OR 'P_KUNNR' OR 'VL_NAMEC'.
            screen-active = '0'.
            MODIFY SCREEN.
            CONTINUE.
        ENDCASE.
    ENDCASE.
  ENDLOOP.

  "PERFORM BUSCA_NOME_CLIENTE.
*=============================================================================*
*START OF SELECTION                                                           *
*=============================================================================*
START-OF-SELECTION.
  PERFORM valida_parametro.
  PERFORM busca_zona_parceiros.
  PERFORM enqueue.
  PERFORM busca_zonas.

  CALL SCREEN 5000.

*&---------------------------------------------------------------------*
*&      Form  VALIDA_PARAMETRO
*&---------------------------------------------------------------------*
FORM valida_parametro .

  DATA: name1 TYPE kna1-name1.

  CASE abap_true.
    WHEN p_c.

      SELECT SINGLE name1
          FROM kna1
          INTO name1
          WHERE kunnr EQ p_kunnr.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

    WHEN p_f.

      SELECT SINGLE name1
        FROM lfa1
        INTO name1
        WHERE lifnr EQ p_lifnr.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE
*&---------------------------------------------------------------------*
FORM enqueue .

*  CALL FUNCTION 'ENQUEUE_E_TABLE'
*    EXPORTING
*      mode_rstable   = 'E'
*      tabname        = 'ZLEST0153'
*    EXCEPTIONS
*      foreign_lock   = 1
*      system_failure = 2
*      OTHERS         = 3.
*
*
*  DATA(lv_msg) = |ZLEST0153|.
*  IF sy-subrc IS NOT INITIAL.
*    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 lv_msg DISPLAY LIKE 'E'.
*    STOP.
*  ELSE.


*   endif
**<<<------"187296 - NMS - INI------>>>
  CASE abap_on.
    WHEN p_c. "Cliente
      CLEAR p_lifnr.

    WHEN p_f. "Fornecedor
      CLEAR p_kunnr.

    WHEN OTHERS.
* Do nothing
  ENDCASE.
**<<<------"187296 - NMS - FIM------>>>
  CALL FUNCTION 'ENQUEUE_EZ_ZLEST0153'
    EXPORTING
      mode_zlest0153 = 'E'
      mandt          = sy-mandt
      kunnr          = p_kunnr
      lifnr          = p_lifnr     "<<<------"187296 - NMS ------->>>
      x_kunnr        = ' '
      _scope         = '2'
      _wait          = ' '
      _collect       = ' '
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  busca_zonas
*&---------------------------------------------------------------------*
FORM busca_zonas.

  DATA wa_saida TYPE ty_zsdt0132_alv.

  IF  p_c EQ abap_true.

    SELECT  kn~kunnr,
            kn~lzone,
            zl~lzone AS zzone,
            tz~land1,
            tz~zlatitude,
            tz~zlongitude,
            tz~z_url_localizacao,
            tx~vtext
     FROM kna1 AS kn
      INNER JOIN zlest0153 AS zl
        ON kn~kunnr = zl~kunnr
       LEFT OUTER JOIN tzone AS tz
        ON   zl~lzone = tz~zone1 AND zl~land1 = tz~land1
       LEFT OUTER JOIN tzont AS tx
         ON tz~land1 = tx~land1 AND tz~zone1 = tx~zone1 AND tx~spras = @sy-langu
       INTO TABLE @DATA(lt_zones_cust)
       WHERE kn~kunnr = @p_kunnr.

    IF sy-subrc EQ 0.

      SORT lt_zones_cust.

      LOOP AT lt_zones_cust INTO DATA(ls_zones_cust).

        MOVE-CORRESPONDING ls_zones_cust TO wa_saida.

        MOVE: ls_zones_cust-land1 TO wa_saida-aland.

        IF ls_zones_cust-zzone IS NOT INITIAL.
          MOVE ls_zones_cust-zzone TO wa_saida-lzone.
        ELSE.
          MOVE ls_zones_cust-lzone TO wa_saida-lzone.
        ENDIF.

        APPEND wa_saida TO it_zsdt0132.

        CLEAR wa_saida.

      ENDLOOP.

    ENDIF.

  ELSEIF p_f EQ abap_true.

    SELECT lf~lifnr,
           lf~lzone,
           zl~lzone AS zzone,
           tz~land1,
           tz~zlatitude,
           tz~zlongitude,
           tz~z_url_localizacao,
           tx~vtext
      FROM lfa1 AS lf
       INNER JOIN zlest0153 AS zl
         ON lf~lifnr = zl~lifnr
        LEFT OUTER JOIN tzone AS tz
         ON  zl~lzone = tz~zone1 AND zl~land1 = tz~land1
        LEFT OUTER JOIN tzont AS tx
         ON tz~land1 = tx~land1 AND tz~zone1 = tx~zone1 AND tx~spras = @sy-langu
        INTO TABLE @DATA(lt_zones_vend)
        WHERE lf~lifnr = @p_lifnr.

    IF sy-subrc EQ 0.

      SORT lt_zones_vend.

      DELETE ADJACENT DUPLICATES FROM lt_zones_vend COMPARING zzone.

      LOOP AT lt_zones_vend INTO DATA(ls_zones_vend).

        MOVE-CORRESPONDING ls_zones_vend TO wa_saida.

        MOVE ls_zones_vend-land1 TO wa_saida-aland.

        IF ls_zones_vend-zzone IS NOT INITIAL.
          MOVE ls_zones_vend-zzone TO wa_saida-lzone.
        ELSE.
          MOVE ls_zones_vend-lzone TO wa_saida-lzone.
        ENDIF.

        APPEND wa_saida TO it_zsdt0132.
        CLEAR wa_saida.

      ENDLOOP.

    ENDIF.

  ENDIF.

  wa_saida-antig = abap_on.
  MODIFY it_zsdt0132 FROM wa_saida TRANSPORTING antig WHERE antig IS INITIAL.

ENDFORM.

INCLUDE zsdr0198_5000.
*INCLUDE zsdr0061_5000.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_ZONA_PARCEIROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_zona_parceiros .

  IF p_c EQ abap_true.

    SELECT SINGLE name1, ort01, pstlz, lzone
        FROM kna1
        INTO ( @vg_name1, @city, @cep, @lzone )
        WHERE kunnr = @p_kunnr.

    IF sy-subrc IS NOT INITIAL.
      CLEAR: vg_name1, city, cep, lzone.
    ENDIF.

  ELSEIF p_f EQ abap_true.

    SELECT SINGLE name1, ort01, pstlz, lzone
        FROM lfa1 AS lf
        INTO ( @vg_name1, @city, @cep, @lzone )
        WHERE lifnr EQ @p_lifnr.

    IF sy-subrc IS NOT INITIAL.
      CLEAR: vg_name1, city, cep.
    ENDIF.

  ENDIF.

ENDFORM.
**<<<------"171563 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*& Form zf_check_field_initial
*&---------------------------------------------------------------------*
*& Verifica se o campo não está preenchido
*&---------------------------------------------------------------------*
*& -->PT_CELL   ALV Control: identificação de células
*& -->UV_COL_ID Índice de coluna das tabelas internas
*& -->UV_ROW_ID Índice de linhas das tabelas internas
*& -->UV_VALUE  Valor a ser validado
*& <--UV_FLAG   Indicador de divergência
*&---------------------------------------------------------------------*
FORM zf_check_field_initial   TABLES pt_cell   STRUCTURE lvc_s_ceno
                              USING  uv_col_id TYPE      int4
                                     uv_row_id TYPE      int4
                                     uv_value
                            CHANGING uv_flag   TYPE      c.

  IF uv_value IS INITIAL.
* Carrega a posição da célula de linha e coluna que foi criticada para destacar na Grid do ALV.
    pt_cell-col_id = uv_col_id.
    pt_cell-row_id = uv_row_id.
    APPEND pt_cell.
    uv_flag = abap_on.

  ENDIF.

ENDFORM.
**<<<------"171563 - NMS - FIM------>>>

*&---------------------------------------------------------------------*
*& Form zf_popup_get_value_zone
*&---------------------------------------------------------------------*
*& Popup dos valores de Latitude, Longitude e URL de Localização
*&---------------------------------------------------------------------*
*&      -->UV_ZONE1             Zona de transporte
*&      -->UV_ZLATITUDE         Latitude
*&      -->UV_ZLONGITUDE        Longitude
*&      -->UV_Z_URL_LOCALIZACAO Endereço de Localização
*&      -->UV_UCOMM             Ação acionada pelo usuário.
*&---------------------------------------------------------------------*
FORM zf_popup_get_value_zone USING uv_zone1             TYPE lzone
                                   uv_vtext             TYPE tzont-vtext
                                   uv_zlatitude         TYPE zde_latitude
                                   uv_zlongitude        TYPE zde_longitude
                                   uv_z_url_localizacao TYPE zde_url_loc
                                   uv_ucomm             TYPE syucomm
                                   uv_resp              TYPE c.

  DATA: tl_fields TYPE TABLE OF sval.

  DATA: el_fields TYPE          sval.
**<<<------"187296 - NMS - INI------>>>
  DATA: vl_zlatitude  TYPE spo_value,
        vl_zlongitude TYPE spo_value.
**<<<------"187296 - NMS - FIM------>>>

  REFRESH tl_fields.
*** Zona de transporte
  el_fields-tabname    = 'TZONE'.
  el_fields-fieldname  = 'ZONE1'.
  el_fields-value      = uv_zone1.
  el_fields-field_obl  = abap_off.
  el_fields-field_attr = '03'.
  APPEND el_fields TO tl_fields.
  CLEAR el_fields.

*** Descrição Zona
  el_fields-tabname    = 'TZONT'.
  el_fields-fieldname  = 'VTEXT'.
*  el_fields-value      = uv_zone1.
  el_fields-field_obl  = abap_on.
  APPEND el_fields TO tl_fields.
  CLEAR el_fields.


*** Latitide
  el_fields-tabname   = 'TZONE'.
  el_fields-fieldname = 'ZLATITUDE'.
  el_fields-value     = uv_zlatitude.
  el_fields-field_obl = abap_off.
  APPEND el_fields TO tl_fields.
  CLEAR el_fields.
*** Longitude
  el_fields-tabname   = 'TZONE'.
  el_fields-fieldname = 'ZLONGITUDE'.
  el_fields-value     = uv_zlongitude.
  el_fields-field_obl = abap_off.
  APPEND el_fields TO tl_fields.
  CLEAR el_fields.
*** Endereço de Localização
  el_fields-tabname   = 'TZONE'.
  el_fields-fieldname = 'Z_URL_LOCALIZACAO'.
  el_fields-value     = uv_z_url_localizacao.
  el_fields-field_obl = abap_off.
  APPEND el_fields TO tl_fields.
  CLEAR el_fields.

*  IF NOT uv_resp IS INITIAL.
*    SELECT SINGLE vtext FROM tzont INTO el_fields-value WHERE spras EQ 'P' AND zone1 EQ uv_zone1.
**** Descrição
*    el_fields-tabname   = 'TZONT'.
*    el_fields-fieldname = 'VTEXT'.
*    el_fields-field_obl = abap_off.
*    APPEND el_fields TO tl_fields.
*    el_fields-field_attr = '03'.
*    MODIFY tl_fields FROM el_fields TRANSPORTING field_attr WHERE field_attr NE '03'.
*    CLEAR el_fields.
*    DATA(vl_flag) = abap_on.
*
*  ENDIF.

  DATA(vl_ucomm) = uv_ucomm.

  DO.
    CLEAR uv_resp.

* Função de exibição de Popup.
    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Zona de transporte'
        start_column    = 20
        start_row       = 10
      IMPORTING
        returncode      = uv_resp
      TABLES
        fields          = tl_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    uv_ucomm = vl_ucomm.
    IF NOT sy-subrc IS INITIAL .
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ELSE.
*    CHECK vl_flag IS INITIAL.
      IF uv_resp EQ 'A'.
* Cancelada pela ação do usuário.
        MESSAGE 'Cancelada pela ação do usuário.' TYPE 'S'.
        RETURN.

      ELSE.
        LOOP AT tl_fields INTO el_fields.
          CASE el_fields-fieldname.
            WHEN 'ZONE1'.             "Zona de transporte
              uv_zone1 = el_fields-value.

            WHEN 'VTEXT'.             "Descrição
              uv_vtext = el_fields-value.

            WHEN 'ZLATITUDE'.         "Latitide
              DATA(dummy) = |{ el_fields-value ALPHA = OUT }|.
              CONDENSE el_fields-value NO-GAPS.
              uv_zlatitude = el_fields-value.

            WHEN 'ZLONGITUDE'.        "Longitude
              CONDENSE el_fields-value NO-GAPS.
              uv_zlongitude = el_fields-value.

            WHEN 'Z_URL_LOCALIZACAO'. "Endereço de Localização
              uv_z_url_localizacao = el_fields-value.

            WHEN OTHERS.
* Do nothing
          ENDCASE.

        ENDLOOP.

        CLEAR vg_erro.

        IF (     uv_zlatitude  IS INITIAL   AND
                 uv_zlongitude IS INITIAL ) OR
           ( NOT uv_zlatitude  IS INITIAL   AND
             NOT uv_zlongitude IS INITIAL ).
**<<<------"187296 - NMS - INI------>>>
*          PERFORM valida_coordenadas USING uv_zlatitude
*                                              2
*
*          IF vg_erro IS INITIAL.
*            PERFORM valida_coordenadas USING uv_zlongitude
*                                                       3
*                                                       0.
*          ENDIF.
          vl_zlatitude  = uv_zlatitude.
          CONDENSE vl_zlatitude NO-GAPS.
          vl_zlongitude = uv_zlongitude.
          CONDENSE vl_zlongitude NO-GAPS.
          CLEAR: uv_zlatitude, uv_zlongitude.
* Validar a condição de existência das cordenadas Latitude e Longitude decimais.
          PERFORM zf_cond_existe_lat_long_dec IN PROGRAM zsdr0061 USING    vl_zlatitude
                                                                           vl_zlongitude
                                                                           sy-abcde(1)   "A - Ambos
                                                                  CHANGING uv_zlatitude
                                                                           uv_zlongitude
                                                                           el_fields-value.

          vg_erro = COND #( WHEN el_fields-value EQ space THEN abap_off ELSE abap_on ).
          CLEAR el_fields-value.
**<<<------"187296 - NMS - FIM------>>>
        ELSE.

          MESSAGE |Os campos Latitude e Longitudo devem estar preenchidas ou em branco.| TYPE 'S' DISPLAY LIKE 'E'.
          vg_erro = abap_true.

        ENDIF.

        IF vg_erro IS INITIAL.
          EXIT.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDDO.

ENDFORM.
**<<<------"171563 - NMS - FIM------>>>

*&---------------------------------------------------------------------*
*&      Form  BUSCA_NOME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_nome .

  IF p_c EQ abap_true.

    "FF - inicio #143815
*    SELECT SINGLE name1
*        FROM kna1
*        INTO vg_name1
*        WHERE kunnr EQ p_kunnr.
*
*    SELECT SINGLE ort01
*        FROM kna1
*        INTO city
*        WHERE kunnr EQ p_kunnr.
*
*    SELECT SINGLE pstlz
*        FROM kna1
*        INTO cep
*        WHERE kunnr EQ p_kunnr.

    SELECT SINGLE name1, ort01, pstlz, lzone
        FROM kna1
        INTO ( @vg_name1, @city, @cep, @lzone )
        WHERE kunnr = @p_kunnr.
    "FF - fim

    IF sy-subrc IS NOT INITIAL.
      CLEAR: vg_name1, city, cep, lzone.
    ENDIF.

  ELSEIF p_f EQ abap_true.

    SELECT SINGLE name1
        FROM lfa1
        INTO vg_name1
        WHERE lifnr EQ p_lifnr.

    SELECT SINGLE ort01
      FROM lfa1
      INTO city
      WHERE lifnr EQ p_lifnr.

    SELECT SINGLE pstlz
        FROM lfa1
        INTO cep
        WHERE lifnr EQ p_lifnr.

    IF sy-subrc IS NOT INITIAL.
      CLEAR: vg_name1, city, cep.
    ENDIF.

  ENDIF.

ENDFORM.

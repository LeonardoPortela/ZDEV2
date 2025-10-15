*=============================================================================*
* Report  ZSDR0061                                                            *
*                                                                             *
*=============================================================================*
*&---------------------------------------------------------------------------&*
*&                    Histórico de Modificações                              &*
*& Autor ABAP |Request    |Data       |Descrição                             &*
*&---------------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2HNK |01/04/2025 |Ajuste diversos no cadastro de Roteiro&*
*&                                    |Chamado: 171563 e 177861.             &*
*&---------------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2MVY |26/06/2025 |Ajuste diversos no cadastro de Roteiro&*
*&                                    |Chamado: 182100.                      &*
*&---------------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2MVY |22/08/2025 |Ajst. na atribuição da Zona ao Roteiro&*
*&                                    |Chamado: 187199.                      &*
*&---------------------------------------------------------------------------&*
REPORT zsdr0061.
*=============================================================================*
*TYPES                                                                        *
*=============================================================================*
TYPES: BEGIN OF ty_header,
         partn TYPE but000-partner,    "<<<------"187199 - NMS ------>>>
         kunnr TYPE kna1-kunnr,
         lifnr TYPE lfa1-lifnr,        "<<<------"187199 - NMS ------>>>
         name1 TYPE kna1-name1,
         city  TYPE kna1-ort01,
         cep   TYPE kna1-pstlz,
         bpzon TYPE adrc-transpzone,   "<<<------"187199 - NMS ------>>>
         lzone TYPE kna1-lzone,                             "FF #143815
         fnzon TYPE lfa1-lzone.        "<<<------"187199 - NMS ------>>>
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
**<<<------"177861 - NMS - INI------>>>
*"FF - inicio #143815
*
*
*
*" Batch input nova estrutura do campo de tabela
*TYPES: BEGIN OF ty_bdcdata,
*         program  TYPE bdcdata-program,  " Pool de módulos BDC
*         dynpro   TYPE bdcdata-dynpro,   " NÚmero de tela BDC
*         dynbegin TYPE bdcdata-dynbegin, " Início BDC de uma tela
*         fnam     TYPE bdcdata-fnam,     " Nome do campo
*         fval     TYPE bdcdata-fval,     " Valor do campo BDC
*       END OF ty_bdcdata,
*
*       BEGIN OF ty_message,
*         cliente TYPE rf02d-kunnr,        " Código do cliente
*         msgty   TYPE message-msgty,      " Tipo da mensagem
*         msgno   TYPE message-msgno,      " Numero da mensagem
*         msgtx   TYPE message-msgtx,      " Descrição da mensagem
*       END OF   ty_message
*       .
*" Estruturas ...
*DATA:
*      st_bdcdata TYPE ty_bdcdata.
*.
*
*" Tabelas Internas ....
*DATA:
*  it_bdcdata TYPE TABLE OF ty_bdcdata,
*  it_msg     TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
*  it_message TYPE TABLE OF ty_message
*  .
*
*
*" Variaveis ....
*DATA: vg_mode(1)    TYPE c VALUE 'N', " informa o Modo do Call Transaction
*      vg_texto(100) TYPE c,        " Texto para o Indicator
*      vg_s          TYPE c VALUE 'S',       " Informa o Update do call Transaction
*      mensg         LIKE message VALUE IS INITIAL, " variavel que recebe retorno
*      msgno         LIKE sy-msgno
*      .
*
*"FF - fim
**<<<------"177861 - NMS - FIM------>>>

*=============================================================================*
*VARIÁVEIS                                                                    *
*=============================================================================*
DATA: it_zsdt0132       TYPE STANDARD TABLE OF ty_zsdt0132_alv,
      it_zsdt0132_safra TYPE STANDARD TABLE OF ty_zsdt0132_alv,
      wa_header         TYPE ty_header,
      vg_name1          TYPE kna1-name1,
      city              TYPE kna1-ort01,
      cep               TYPE kna1-pstlz,
      lzone             TYPE kna1-lzone.
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

*-US190444-10.09.2025-#190444-JT-inicio
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (15) vl_descr.
    PARAMETERS: p_nrrot LIKE zsdt0132-nr_rot.
    SELECTION-SCREEN POSITION 35.
    SELECTION-SCREEN COMMENT (37) vl_namer.
  SELECTION-SCREEN END OF LINE.
*-US190444-10.09.2025-#190444-JT-fim

SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.

  CLEAR: vl_namec, vl_namef, vl_descc, vl_descf.

  CASE abap_true.
    WHEN p_c.

*-US190444-10.09.2025-#190444-JT-inicio
      IF p_nrrot IS NOT INITIAL AND p_kunnr IS NOT INITIAL.
        CLEAR p_kunnr.
      ELSEIF p_nrrot IS NOT INITIAL.
        CLEAR p_kunnr.
      ELSEIF p_kunnr IS NOT INITIAL.
        CLEAR p_nrrot.
      ENDIF.
*-US190444-10.09.2025-#190444-JT-fim

      CLEAR p_lifnr.

      vl_descc = 'Cód. Cliente'.
      vl_descf = ''.
      vl_descr = 'Num. do Roteiro'.  "*-US190444-10.09.2025-#190444-JT-inicio

      SELECT SINGLE name1
        FROM kna1
        INTO vl_namec
        WHERE kunnr EQ p_kunnr.

      IF NOT sy-subrc IS INITIAL.
        vl_namec = ''.
      ELSE.

      ENDIF.

    WHEN p_f.

*-US190444-10.09.2025-#190444-JT-inicio
      IF p_nrrot IS NOT INITIAL AND p_lifnr IS NOT INITIAL.
        CLEAR p_lifnr.
      ELSEIF p_nrrot IS NOT INITIAL.
        CLEAR p_lifnr.
      ELSEIF p_lifnr IS NOT INITIAL.
        CLEAR p_nrrot.
      ENDIF.
*-US190444-10.09.2025-#190444-JT-fim

      CLEAR: p_kunnr.

      vl_descf = 'Cód. Fornecedor'.
      vl_descc = ''.
      vl_descr = 'Num. do Roteiro'.  "*-US190444-10.09.2025-#190444-JT-inicio

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
  PERFORM busca_nome.
  PERFORM enqueue.
  PERFORM busca_roteiros.

  CALL SCREEN 5000.

*&---------------------------------------------------------------------*
*&      Form  VALIDA_PARAMETRO
*&---------------------------------------------------------------------*
FORM valida_parametro .

  DATA: name1 TYPE kna1-name1.

*-US190444-10.09.2025-#190444-JT-inicio
  IF p_nrrot IS NOT INITIAL.
    SELECT SINGLE nr_rot, kunnr, lifnr
      INTO @DATA(_roteiro)
      FROM zsdt0132
     WHERE nr_rot = @p_nrrot.

    IF sy-subrc = 0.
      IF _roteiro-kunnr IS NOT INITIAL AND p_c = abap_true.
        p_kunnr = _roteiro-kunnr.
      ENDIF.
      IF _roteiro-lifnr IS NOT INITIAL AND p_f = abap_true.
        p_lifnr = _roteiro-lifnr.
      ENDIF.
    ENDIF.
  ENDIF.
*-US190444-10.09.2025-#190444-JT-fim

  CASE abap_true.
    WHEN p_c.

      SELECT SINGLE name1
          FROM kna1
          INTO name1
          WHERE kunnr EQ p_kunnr.

      IF sy-subrc IS NOT INITIAL.
        IF p_nrrot IS NOT INITIAL.  "*-US190444-10.09.2025-#190444-JT-inicio
          MESSAGE s024(sd) WITH 'Roteiro não Pertence a Cliente!' DISPLAY LIKE 'E'.
        ELSE.
          MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
        STOP.
      ENDIF.

    WHEN p_f.

      SELECT SINGLE name1
        FROM lfa1
        INTO name1
        WHERE lifnr EQ p_lifnr.

      IF sy-subrc IS NOT INITIAL.
        IF p_nrrot IS NOT INITIAL.  "*-US190444-10.09.2025-#190444-JT-inicio
          MESSAGE s024(sd) WITH 'Roteiro não Pertence a Fornecedor!' DISPLAY LIKE 'E'.
        ELSE.
          MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
        STOP.
      ENDIF.

  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE
*&---------------------------------------------------------------------*
FORM enqueue .

**<<<------"187199 - NMS - INI------>>>
  CASE abap_on.
    WHEN p_c. "Cliente
      DATA(vl_cod_id) = p_kunnr.

    WHEN p_f. "Fornecedor
      vl_cod_id = p_lifnr.

    WHEN OTHERS.
* Do nothing
  ENDCASE.
**<<<------"187199 - NMS - FIM------>>>

  CALL FUNCTION 'ZENQUEUE_SD_CAD_ROTEIRO'
    EXPORTING
**<<<------"187199 - NMS - INI------>>>
*     chave          = p_kunnr
      chave          = vl_cod_id
**<<<------"187199 - NMS - FIM------>>>
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_ROTEIROS
*&---------------------------------------------------------------------*
FORM busca_roteiros.

  FREE: it_zsdt0132_safra.

  DATA: wa_zsdt0132 TYPE ty_zsdt0132_alv.

  IF  p_c EQ abap_true.

    SELECT *
        FROM zsdt0132
        INTO CORRESPONDING FIELDS OF TABLE it_zsdt0132
        WHERE kunnr EQ p_kunnr.
**<<<------"171563 - NMS - INI------>>>
    IF sy-subrc IS INITIAL.
      SELECT SINGLE land1 INTO @DATA(vl_land1) FROM kna1 WHERE kunnr EQ @p_kunnr.

      IF sy-subrc IS INITIAL.
        SELECT * FROM tzone
          INTO TABLE @DATA(tl_tzone)
          FOR ALL ENTRIES IN @it_zsdt0132
        WHERE land1 EQ @vl_land1
          AND zone1 EQ @it_zsdt0132-lzone.

      ENDIF.

    ENDIF.
**<<<------"171563 - NMS - FIM------>>>
    LOOP AT it_zsdt0132 INTO wa_zsdt0132.
      MOVE abap_true TO wa_zsdt0132-antig.
**<<<------"171563 - NMS - INI------>>>
      DATA(vl_tabix) = sy-tabix.
      READ TABLE tl_tzone INTO DATA(el_tzone) WITH KEY zone1 = wa_zsdt0132-lzone.

      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING el_tzone TO wa_zsdt0132.

      ENDIF.

      sy-tabix = vl_tabix.
**<<<------"171563 - NMS - FIM------>>>
      MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX sy-tabix.
    ENDLOOP.

  ELSEIF p_f EQ abap_true.

    SELECT *
        FROM zsdt0132
        INTO CORRESPONDING FIELDS OF TABLE it_zsdt0132
        WHERE lifnr EQ p_lifnr.

    IF sy-subrc IS INITIAL.
      " Projeto Insumos 2025 - 16.06 - Inicio
      SELECT SINGLE land1 INTO @vl_land1 FROM lfa1 WHERE lifnr EQ @p_lifnr.

      IF sy-subrc IS INITIAL.
        SELECT * FROM tzone
          INTO TABLE @tl_tzone
          FOR ALL ENTRIES IN @it_zsdt0132
        WHERE land1 EQ @vl_land1
          AND zone1 EQ @it_zsdt0132-lzone.

      ENDIF.

    ENDIF.

    LOOP AT it_zsdt0132 INTO wa_zsdt0132.
      MOVE abap_true TO wa_zsdt0132-antig.

      DATA(vl_tabix1) = sy-tabix.
      READ TABLE tl_tzone INTO el_tzone WITH KEY zone1 = wa_zsdt0132-lzone.

      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING el_tzone TO wa_zsdt0132.

      ENDIF.

      sy-tabix = vl_tabix1.

      MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX sy-tabix.
    ENDLOOP.
    " Projeto Insumos 2025 - 16.06 - Fim

  ENDIF.

ENDFORM.

INCLUDE zsdr0061_5000.
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

    SELECT SINGLE name1, lzone
        FROM lfa1
        INTO  ( @vg_name1 , @lzone )
        WHERE lifnr EQ @p_lifnr.

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
*&---------------------------------------------------------------------*
*& Form zf_popup_get_value_zone
*&---------------------------------------------------------------------*
*& Popup dos valores de Latitude, Longitude e URL de Localização
*&---------------------------------------------------------------------*
*&      -->UV_ZONE1             Zona de transporte
*&      -->UV_ZLATITUDE         Latitude
*&      -->UV_ZLONGITUDE        Longitude
*&      -->UV_Z_URL_LOCALIZACAO Endereço de Localização
*&      -->UV_UCOMM             Ação acionada pelo usuário
*&      -->UV_RESP              Retorno da ação do botão da POPUP
*&---------------------------------------------------------------------*
FORM zf_popup_get_value_zone USING uv_zone1             TYPE lzone
                                   uv_zlatitude         TYPE zde_latitude
                                   uv_zlongitude        TYPE zde_longitude
                                   uv_z_url_localizacao TYPE zde_url_loc
                                   uv_ucomm             TYPE syucomm
                                   uv_resp              TYPE c.

  DATA: tl_fields TYPE TABLE OF sval.

  DATA: el_fields TYPE          sval.

  REFRESH tl_fields.
*** Zona de transporte
  el_fields-tabname    = 'TZONE'.
  el_fields-fieldname  = 'ZONE1'.
  el_fields-value      = uv_zone1.
  el_fields-field_obl  = abap_on.   "<<<------"182100 ------->>>
  el_fields-field_attr = '03'.
  APPEND el_fields TO tl_fields.
  CLEAR el_fields.
*** Latitude
  el_fields-tabname   = 'TZONE'.
  el_fields-fieldname = 'ZLATITUDE'.
  el_fields-value     = uv_zlatitude.
  el_fields-field_obl = abap_on.   "<<<------"182100 ------->>>
  APPEND el_fields TO tl_fields.
  CLEAR el_fields.
*** Longitude
  el_fields-tabname   = 'TZONE'.
  el_fields-fieldname = 'ZLONGITUDE'.
  el_fields-value     = uv_zlongitude.
  el_fields-field_obl = abap_on.   "<<<------"182100 ------->>>
  APPEND el_fields TO tl_fields.
  CLEAR el_fields.
*** Endereço de Localização
  el_fields-tabname   = 'TZONE'.
  el_fields-fieldname = 'Z_URL_LOCALIZACAO'.
  el_fields-value     = uv_z_url_localizacao.
  el_fields-field_obl = abap_on.   "<<<------"182100 ------->>>
  APPEND el_fields TO tl_fields.
  CLEAR el_fields.

  IF NOT uv_resp IS INITIAL.
    SELECT SINGLE vtext FROM tzont INTO el_fields-value WHERE spras EQ 'P' AND zone1 EQ uv_zone1.
*** Descrição
    el_fields-tabname   = 'TZONT'.
    el_fields-fieldname = 'VTEXT'.
    el_fields-field_obl = abap_off.
    APPEND el_fields TO tl_fields.
    el_fields-field_attr = '03'.
    MODIFY tl_fields FROM el_fields TRANSPORTING field_attr WHERE field_attr NE '03'.
    CLEAR el_fields.
    DATA(vl_flag) = abap_on.

  ENDIF.

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

    IF NOT sy-subrc IS INITIAL .
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ELSE.
      CHECK vl_flag IS INITIAL.
      IF uv_resp EQ 'A'.
* Cancelada pela ação do usuário.
        MESSAGE 'Cancelada pela ação do usuário.' TYPE 'S'.
        RETURN.

      ELSE.
        LOOP AT tl_fields INTO el_fields.
          CASE el_fields-fieldname.
            WHEN 'ZONE1'.             "Zona de transporte
              uv_zone1 = el_fields-value.

            WHEN 'ZLATITUDE'.         "Latitide
              DATA(dummy) = |{ el_fields-value ALPHA = OUT }|.
              CONDENSE el_fields-value NO-GAPS.
              DATA(vl_zlatitude)  = el_fields-value.

            WHEN 'ZLONGITUDE'.        "Longitude
              CONDENSE el_fields-value NO-GAPS.
              DATA(vl_zlongitude) = el_fields-value.

            WHEN 'Z_URL_LOCALIZACAO'. "Endereço de Localização
              uv_z_url_localizacao = el_fields-value.

            WHEN OTHERS.
* Do nothing
          ENDCASE.

        ENDLOOP.
**<<<------"182100 - NMS - INI------>>>
*        IF (     uv_zlatitude  IS INITIAL   AND
*                 uv_zlongitude IS INITIAL ) OR
*           ( NOT uv_zlatitude  IS INITIAL   AND
*             NOT uv_zlongitude IS INITIAL ).
*          EXIT.
*
*        ELSE.
*          MESSAGE |Os campos Latitude e Longitudo devem estar preenchidas ou em branco para salvar.| TYPE 'S' DISPLAY LIKE 'E'.
*
*        ENDIF.
        CLEAR el_fields-value.
* Validar a condição de existência das cordenadas Latitude e Longitude decimais.
        PERFORM zf_cond_existe_lat_long_dec USING    vl_zlatitude
                                                     vl_zlongitude
                                                     sy-abcde(1)   "A - Ambos
                                            CHANGING uv_zlatitude
                                                     uv_zlongitude
                                                     el_fields-value.

        IF NOT uv_zlatitude         IS INITIAL AND
           NOT uv_zlongitude        IS INITIAL AND
           NOT uv_z_url_localizacao IS INITIAL AND
               el_fields-value      IS INITIAL.
          EXIT.

        ELSE.
          MESSAGE el_fields-value TYPE 'S' DISPLAY LIKE 'E'.
          uv_resp = sy-abcde(1). "A - Abortar

        ENDIF.
**<<<------"182100 - NMS - FIM------>>>
      ENDIF.

    ENDIF.

  ENDDO.

  uv_ucomm = vl_ucomm.

ENDFORM.
**<<<------"171563 - NMS - FIM------>>>
**<<<------"182100 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*& Form zf_cond_existe_lat_long_dec
*&---------------------------------------------------------------------*
*& Validar a condição de existência das cordenadas Latitude e Longitude
*& decimais
*&---------------------------------------------------------------------*
*&      --> UV_ZLATIT_IN   Latitude Entrada
*&      --> UV_ZLONGI_IN   Longitude Entrada
*&      --> UV_TP_CHECK    Tipo da Validação A-Ambos/T-laTitude/G-lonGitude
*&      --> CV_ZLATIT_OUT  Latitude Saída
*&      --> CV_ZLONGI_OUT  Longitude Saída
*&      --> CV_MESSAGE     Mensagem de processmento
*&---------------------------------------------------------------------*
FORM zf_cond_existe_lat_long_dec USING    uv_zlatit_in  TYPE spo_value
                                          uv_zlongi_in  TYPE spo_value
                                          uv_tp_check   TYPE c
                                 CHANGING cv_zlatit_out TYPE zde_latitude
                                          cv_zlongi_out TYPE zde_longitude
                                          cv_message    TYPE spo_value.

  DATA: vl_zlatit_out TYPE jbfbeta,
        vl_zlongi_out TYPE jbfbeta.

  IF uv_tp_check EQ sy-abcde(1)   OR "A - Ambos
     uv_tp_check EQ sy-abcde+19(1).  "T - laTitude
* Valida se é valor numérico.
    TRY.
        cv_zlatit_out = vl_zlatit_out = uv_zlatit_in.
**<<<------"187199 - NMS - INI------>>>
* Verifica se o valor é negativo para formatar o sinal do lado Esquerdo do mesmo.
        IF vl_zlatit_out LT 0.
          REPLACE sy-uline(1) IN uv_zlatit_in WITH space.
          CONDENSE uv_zlatit_in NO-GAPS.
          uv_zlatit_in = sy-uline(1) && uv_zlatit_in.
          vl_zlatit_out = cv_zlatit_out = uv_zlatit_in.

        ENDIF.
**<<<------"187199 - NMS - FIM------>>>
      CATCH cx_sy_conversion_no_number INTO DATA(vl_error).
        cv_message = CONV #( vl_error->get_text( ) ).

    ENDTRY.

    CONDENSE cv_zlatit_out NO-GAPS.
*** Condição de existência para latitude
* Latitude deve estar entre -90 e +90 graus.
    IF NOT vl_zlatit_out BETWEEN -90 AND 90.
      cv_message = 'Latitude deve estar entre -90 e +90 graus.'.

    ENDIF.

  ENDIF.

  IF uv_tp_check EQ sy-abcde(1)  OR "A - Ambos
     uv_tp_check EQ sy-abcde+6(1).  "G - loGitude
* Valida se é valor numérico.
    TRY.
        cv_zlongi_out = vl_zlongi_out = uv_zlongi_in.
**<<<------"187199 - NMS - INI------>>>
* Verifica se o valor é negativo para formatar o sinal do lado Esquerdo do mesmo.
        IF uv_zlongi_in LT 0.
          REPLACE sy-uline(1) IN uv_zlongi_in WITH space.
          CONDENSE uv_zlongi_in NO-GAPS.
          uv_zlongi_in = sy-uline(1) && uv_zlongi_in.
          vl_zlongi_out = cv_zlongi_out = uv_zlongi_in.

        ENDIF.
**<<<------"187199 - NMS - FIM------>>>
      CATCH cx_sy_conversion_no_number INTO vl_error.
        cv_message = CONV #( vl_error->get_text( ) ).

    ENDTRY.

    CONDENSE cv_zlongi_out NO-GAPS.
*** Condição de existência para latitude
* Longitude deve estar entre -180 e +180 graus.
    IF NOT vl_zlongi_out BETWEEN -180 AND 180.
      IF cv_message IS INITIAL.
        cv_message = 'Longitude deve estar entre -180 e +180 graus.'.

      ELSE.
        cv_message = |{ cv_message } E Longitude deve estar entre -180 e +180 graus.|.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_check_zone_used
*&---------------------------------------------------------------------*
*& Verifica se a Zona de Transporte está sendo usada por outro Roteiro
*&---------------------------------------------------------------------*
*& -->  PT_CELL      ALV Contro: tabela de células
*& -->  UE_ZSDT0132  WA da tabela de exibição do ALV
*& -->  UV_LAND1     Chave do país/da região
*& -->  UV_TABIX     Índice de linhas das tabelas internas
*& <--  UV_ZONE_USED Valida Zona de Transporte se já usada ou não
*& <--  CV_IDPARC    Identificação do parceiro (Cliente/Fornecedor)
*&---------------------------------------------------------------------*
FORM zf_check_zone_used TABLES   pt_cell      STRUCTURE    lvc_s_ceno
                        USING    ue_zsdt0132  LIKE LINE OF it_zsdt0132
                                 uv_land1     TYPE         land1_gp
                                 uv_tabix     TYPE         sytabix
                        CHANGING cv_zone_used TYPE         c
                                 cv_nr_rot    TYPE         z_nr_rot
                                 cv_idparc    TYPE         kunnr.

  DATA: vl_qtlin TYPE i.

* Verifica se a Zona de Transporte está sendo usada por outro Roteiro.
  SELECT lzone, nr_rot, kunnr, lifnr, rot_desc, endereco, city1, uf, armazem
    FROM zsdt0132 AS a
    INNER JOIN tzone AS b
     ON a~lzone EQ b~zone1
    INTO TABLE @DATA(tl_0132_aux)
  WHERE lzone EQ @ue_zsdt0132-lzone
    AND land1 EQ @uv_land1.

  IF sy-subrc IS INITIAL.
    CASE abap_on.
      WHEN p_c. "Cliente
        READ TABLE tl_0132_aux INTO DATA(el_0132_aux) WITH KEY nr_rot = ue_zsdt0132-nr_rot
                                                               lifnr  = space.

      WHEN p_f. "Fornecedor
        READ TABLE tl_0132_aux INTO el_0132_aux WITH KEY nr_rot = ue_zsdt0132-nr_rot
                                                         kunnr  = space.

      WHEN OTHERS.
*     Do nothing
    ENDCASE.

    IF NOT sy-subrc IS INITIAL.
      CASE abap_on.
        WHEN p_c. "Cliente
          READ TABLE tl_0132_aux INTO el_0132_aux WITH KEY lzone = ue_zsdt0132-lzone
                                                           lifnr = space.

        WHEN p_f. "Fornecedor
          READ TABLE tl_0132_aux INTO el_0132_aux WITH KEY lzone = ue_zsdt0132-lzone
                                                           kunnr = space.

        WHEN OTHERS.
*     Do nothing
      ENDCASE.

      IF sy-subrc IS INITIAL.
        APPEND INITIAL LINE TO pt_cell ASSIGNING FIELD-SYMBOL(<fs_cell>).
        DATA(vl_tabix) = sy-tabix.
* Carrega a posição da célula de linha e coluna que foi criticada para destacar na Grid do ALV.
        <fs_cell>-col_id = 7.
        <fs_cell>-row_id = uv_tabix.
        cv_zone_used     = abap_on.
        cv_nr_rot        = el_0132_aux-nr_rot.

        CASE abap_on.
          WHEN p_c. "Cliente
            cv_idparc = el_0132_aux-kunnr.

          WHEN p_f. "Fornecedor
            cv_idparc = el_0132_aux-lifnr.

          WHEN OTHERS.
*     Do nothing
        ENDCASE.

      ENDIF.

    ENDIF.

  ELSE.
    LOOP AT it_zsdt0132 INTO DATA(el_zsdt0132) WHERE lzone  EQ ue_zsdt0132-lzone
                                                 AND nr_rot LT ue_zsdt0132-nr_rot.
      APPEND INITIAL LINE TO pt_cell ASSIGNING <fs_cell>.
      vl_tabix = sy-tabix.
* Carrega a posição da célula de linha e coluna que foi criticada para destacar na Grid do ALV.
      <fs_cell>-col_id = 7.
      <fs_cell>-row_id = sy-tabix.
      cv_zone_used     = abap_on.
      cv_nr_rot        = el_zsdt0132-nr_rot.

      CASE abap_on.
        WHEN p_c. "Cliente
          cv_idparc = p_kunnr.

        WHEN p_f. "Fornecedor
          cv_idparc = p_lifnr.

        WHEN OTHERS.
*     Do nothing
      ENDCASE.

      MOVE-CORRESPONDING el_zsdt0132 TO el_0132_aux.
      EXIT.

    ENDLOOP.

  ENDIF.

  IF NOT cv_zone_used IS INITIAL AND
     NOT p_f          IS INITIAL.
    LOOP AT tl_0132_aux INTO DATA(el_0132_aux2) WHERE rot_desc EQ ue_zsdt0132-rot_desc
                                                  AND endereco EQ ue_zsdt0132-endereco
                                                  AND city1    EQ ue_zsdt0132-city1
                                                  AND uf       EQ ue_zsdt0132-uf.
      CASE el_0132_aux2-armazem.
        WHEN abap_on.  "Com armazém
          vl_qtlin += 1.

        WHEN abap_off. "Sem armazém
          vl_qtlin += 1.

        WHEN OTHERS.
*       Do nothing
      ENDCASE.

    ENDLOOP.

    IF vl_qtlin GE 2.
      READ TABLE tl_0132_aux INTO el_0132_aux2 WITH KEY rot_desc = ue_zsdt0132-rot_desc
                                                        endereco = ue_zsdt0132-endereco
                                                        city1    = ue_zsdt0132-city1
                                                        uf       = ue_zsdt0132-uf
                                                        armazem  = ue_zsdt0132-armazem.

      IF sy-subrc IS INITIAL.
        cv_nr_rot = el_0132_aux2-nr_rot.
        cv_idparc = el_0132_aux2-lifnr.

      ENDIF.

    ENDIF.

    IF el_0132_aux-rot_desc EQ ue_zsdt0132-rot_desc AND
       el_0132_aux-endereco EQ ue_zsdt0132-endereco AND
       el_0132_aux-city1    EQ ue_zsdt0132-city1    AND
       el_0132_aux-uf       EQ ue_zsdt0132-uf       AND
       el_0132_aux-armazem  NE ue_zsdt0132-armazem  AND
       vl_qtlin             LT 2.
      DELETE pt_cell INDEX vl_tabix.
      CLEAR: cv_zone_used, cv_nr_rot, cv_idparc.

    ENDIF.

  ENDIF.

ENDFORM.
**<<<------"182100 - NMS - FIM------>>>

*----------------------------------------------------------------------*
***INCLUDE LZGFSD004F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_NF_VALIDA_ENVIO
*&---------------------------------------------------------------------*
FORM f_nf_valida_envio USING uv_docnum TYPE j_1bdocnum
                    CHANGING cv_id_ecom TYPE zsde_ecomm_id.

  DATA lv_vbeln TYPE vbeln.

  CLEAR cv_id_ecom.

  SELECT SINGLE refkey FROM j_1bnflin
    INTO lv_vbeln
      WHERE docnum = uv_docnum.

  IF lv_vbeln IS INITIAL.

    EXIT.

  ENDIF.

  SELECT SINGLE vbelv,posnv FROM vbfa
    INTO @DATA(ls_vbfa)
    WHERE vbeln = @lv_vbeln
      AND vbtyp_n = 'M'
      AND vbtyp_v = 'C'.

  IF ls_vbfa IS INITIAL.

    EXIT.

  ENDIF.

  SELECT SINGLE zsdt0040~doc_simulacao,id_order_ecommerce FROM zsdt0041
    INNER JOIN zsdt0040 ON zsdt0040~doc_simulacao = zsdt0041~doc_simulacao
    INTO @DATA(ls_0041)
       WHERE vbeln = @ls_vbfa-vbelv
         AND ecommerce = 'X'.

  IF sy-subrc NE 0.

    SELECT SINGLE vbelv,posnv,matnrv FROM zsdt0090
      INTO @DATA(ls_0090)
        WHERE vbeln = @ls_vbfa-vbelv
          AND posnn = @ls_vbfa-posnv
          AND estorno <> 'X'
          AND vbeln <> @space
          AND posnn <> @space
          AND vbelv <> @space
          AND posnv <> @space.

    IF sy-subrc NE 0.

      EXIT.

    ENDIF.

    SELECT SINGLE zsdt0040~doc_simulacao id_order_ecommerce FROM zsdt0041
      INNER JOIN zsdt0040 ON zsdt0040~doc_simulacao = zsdt0041~doc_simulacao
      INTO ls_0041
      WHERE vbeln = ls_0090-vbelv
        AND matnr = ls_0090-matnrv.

    IF sy-subrc NE 0.

      SELECT SINGLE vbelv,matnrv FROM zsdt0090
        INTO @DATA(ls_0090_aux)
          WHERE vbeln = @ls_0090-vbelv
            AND posnn = @ls_0090-posnv
            AND estorno <> 'X'
            AND vbeln <> @space
            AND posnn <> @space
            AND vbelv <> @space
            AND posnv <> @space.

      IF sy-subrc EQ 0.

        SELECT SINGLE zsdt0040~doc_simulacao id_order_ecommerce FROM zsdt0041
        INNER JOIN zsdt0040 ON zsdt0040~doc_simulacao = zsdt0041~doc_simulacao
          INTO ls_0041
          WHERE vbeln = ls_0090_aux-vbelv
            AND matnr = ls_0090_aux-matnrv
            AND ecommerce = 'X'.

      ENDIF.

    ENDIF.

  ENDIF.

  cv_id_ecom = ls_0041-id_order_ecommerce.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_INSERE
*&---------------------------------------------------------------------*
FORM f_mensagem_insere TABLES p_ret_tab STRUCTURE bapiret2
                        USING i_type TYPE bapi_mtype
                              i_id  TYPE  symsgid
                              i_number  TYPE  symsgno
                              i_mess_v1 TYPE any
                              i_mess_v2 TYPE any
                              i_mess_v3 TYPE any
                              i_mess_v4 TYPE any.

  APPEND INITIAL LINE TO p_ret_tab ASSIGNING FIELD-SYMBOL(<fs_ret>).

  <fs_ret>-type = i_type.
  <fs_ret>-id = i_id.
  <fs_ret>-number = i_number.
  <fs_ret>-message_v1 = i_mess_v1.
  <fs_ret>-message_v2 = i_mess_v2.
  <fs_ret>-message_v3 = i_mess_v3.
  <fs_ret>-message_v4 = i_mess_v4.
  <fs_ret>-system = sy-sysid.

  MESSAGE ID <fs_ret>-id TYPE <fs_ret>-type NUMBER <fs_ret>-number
    WITH <fs_ret>-message_v1 <fs_ret>-message_v2 <fs_ret>-message_v3
      <fs_ret>-message_v4 INTO <fs_ret>-message.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_MESSAGE
*&---------------------------------------------------------------------*
FORM f_get_message USING uv_ret TYPE string
                CHANGING cv_mess TYPE c.

  TYPES: BEGIN OF ty_mess,
           success TYPE abap_bool, "string,
           message TYPE string.
  TYPES: END OF ty_mess.

  DATA ls_form TYPE ty_mess.

  /ui2/cl_json=>deserialize( EXPORTING json = uv_ret CHANGING data = ls_form ).

  PERFORM f_remove_unicode CHANGING ls_form-message.

  cv_mess = ls_form-message.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_REMOVE_UNICODE
*&---------------------------------------------------------------------*
FORM f_remove_unicode CHANGING cv_string TYPE string.

  REPLACE ALL OCCURRENCES OF 'u00e1' IN cv_string WITH 'á'.
  REPLACE ALL OCCURRENCES OF 'u00e0' IN cv_string WITH 'à'.
  REPLACE ALL OCCURRENCES OF 'u00e2' IN cv_string WITH 'â'.
  REPLACE ALL OCCURRENCES OF 'u00e3' IN cv_string WITH 'ã'.
  REPLACE ALL OCCURRENCES OF 'u00c1' IN cv_string WITH 'Á'.
  REPLACE ALL OCCURRENCES OF 'u00c0' IN cv_string WITH 'À'.
  REPLACE ALL OCCURRENCES OF 'u00c2' IN cv_string WITH 'Â'.
  REPLACE ALL OCCURRENCES OF 'u00c3' IN cv_string WITH 'Ã'.
  REPLACE ALL OCCURRENCES OF 'u00e9' IN cv_string WITH 'é'.
  REPLACE ALL OCCURRENCES OF 'u00e8' IN cv_string WITH 'è'.
  REPLACE ALL OCCURRENCES OF 'u00ea' IN cv_string WITH 'ê'.
  REPLACE ALL OCCURRENCES OF 'u00ea' IN cv_string WITH 'ê'.
  REPLACE ALL OCCURRENCES OF 'u00c9' IN cv_string WITH 'É'.
  REPLACE ALL OCCURRENCES OF 'u00c8' IN cv_string WITH 'È'.
  REPLACE ALL OCCURRENCES OF 'u00ca' IN cv_string WITH 'Ê'.
  REPLACE ALL OCCURRENCES OF 'u00cb' IN cv_string WITH 'Ë'.
  REPLACE ALL OCCURRENCES OF 'u00ed' IN cv_string WITH 'í'.
  REPLACE ALL OCCURRENCES OF 'u00ec' IN cv_string WITH 'ì'.
  REPLACE ALL OCCURRENCES OF 'u00ee' IN cv_string WITH 'î'.
  REPLACE ALL OCCURRENCES OF 'u00ef' IN cv_string WITH 'ï'.
  REPLACE ALL OCCURRENCES OF 'u00cd' IN cv_string WITH 'Í'.
  REPLACE ALL OCCURRENCES OF 'u00cc' IN cv_string WITH 'Ì'.
  REPLACE ALL OCCURRENCES OF 'u00ce' IN cv_string WITH 'Î'.
  REPLACE ALL OCCURRENCES OF 'u00cf' IN cv_string WITH 'Ï'.
  REPLACE ALL OCCURRENCES OF 'u00f3' IN cv_string WITH 'ó'.
  REPLACE ALL OCCURRENCES OF 'u00f2' IN cv_string WITH 'ò'.
  REPLACE ALL OCCURRENCES OF 'u00f4' IN cv_string WITH 'ô'.
  REPLACE ALL OCCURRENCES OF 'u00f5' IN cv_string WITH 'õ'.
  REPLACE ALL OCCURRENCES OF 'u00f6' IN cv_string WITH 'ö'.
  REPLACE ALL OCCURRENCES OF 'u00d3' IN cv_string WITH 'Ó'.
  REPLACE ALL OCCURRENCES OF 'u00d2' IN cv_string WITH 'Ò'.
  REPLACE ALL OCCURRENCES OF 'u00d4' IN cv_string WITH 'Ô'.
  REPLACE ALL OCCURRENCES OF 'u00d5' IN cv_string WITH 'Õ'.
  REPLACE ALL OCCURRENCES OF 'u00d6' IN cv_string WITH 'Ö'.
  REPLACE ALL OCCURRENCES OF 'u00fa' IN cv_string WITH 'ú'.
  REPLACE ALL OCCURRENCES OF 'u00f9' IN cv_string WITH 'ù'.
  REPLACE ALL OCCURRENCES OF 'u00fb' IN cv_string WITH 'û'.
  REPLACE ALL OCCURRENCES OF 'u00fc' IN cv_string WITH 'ü'.
  REPLACE ALL OCCURRENCES OF 'u00da' IN cv_string WITH 'Ú'.
  REPLACE ALL OCCURRENCES OF 'u00d9' IN cv_string WITH 'Ù'.
  REPLACE ALL OCCURRENCES OF 'u00db' IN cv_string WITH 'Û'.
  REPLACE ALL OCCURRENCES OF 'u00e7' IN cv_string WITH 'ç'.
  REPLACE ALL OCCURRENCES OF 'u00c7' IN cv_string WITH 'Ç'.
  REPLACE ALL OCCURRENCES OF 'u00f1' IN cv_string WITH 'ñ'.
  REPLACE ALL OCCURRENCES OF 'u00d1' IN cv_string WITH 'Ñ'.
  REPLACE ALL OCCURRENCES OF 'u0026' IN cv_string WITH '&'.
  REPLACE ALL OCCURRENCES OF 'u0027' IN cv_string WITH ''''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_INTEG_MESS
*&---------------------------------------------------------------------*
FORM f_get_integ_mess USING us_integ TYPE zintegracao
                  CHANGING ct_ret TYPE bapiret2_t.

  DATA lv_type.
  DATA lv_mess TYPE c LENGTH 300.

  PERFORM f_get_message USING us_integ-ds_data_retorno CHANGING lv_mess.

  IF us_integ-nm_code < 205.

    lv_type = 'S'.

    lv_mess = 'Enviado com sucesso'. " sobrescreve a mensagem original

  ELSE.
    lv_type = 'E'.

  ENDIF.

  PERFORM f_mensagem_insere
   TABLES ct_ret
    USING lv_type 'ZSD' '001'
          lv_mess(40) lv_mess+40(40) lv_mess+80(40) ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DEBUG_MODE
*&---------------------------------------------------------------------*
FORM f_debug_mode .

  DO.

    SELECT SINGLE low FROM tvarvc INTO @DATA(lv_debug) WHERE name = 'ZDEBUG_NF_XML_ECOMM'.

    IF lv_debug IS INITIAL.

      EXIT.

    ENDIF.

  ENDDO.

ENDFORM.

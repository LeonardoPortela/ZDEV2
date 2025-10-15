*----------------------------------------------------------------------*
***INCLUDE LZSDG002F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  Z_INSERE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0012   text
*      -->P_0013   text
*      -->P_0014   text
*----------------------------------------------------------------------*
FORM z_insere_bdc  USING p_dynbegin
                         p_field
                         p_value.

  DATA: wa_bdc TYPE bdcdata.

  CLEAR wa_bdc.

  IF p_dynbegin EQ 'X'.
    wa_bdc-dynbegin = 'X'.
    wa_bdc-program  = p_field.
    wa_bdc-dynpro   = p_value.
  ELSE.
    wa_bdc-fnam = p_field.
    wa_bdc-fval = p_value.
  ENDIF.

  APPEND wa_bdc TO t_bdc.

ENDFORM.                    " Z_INSERE_BDC

*&---------------------------------------------------------------------*
*&      Form  SET_MSG_TO_BAPIRET2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RETURN  text
*      -->P_ITAB  text
*----------------------------------------------------------------------*
FORM set_msg_to_bapiret2  TABLES   p_return STRUCTURE bdcmsgcoll
                                   p_itab STRUCTURE bdcmsgcoll.

 clear p_return.
 LOOP AT p_itab.
   move-corresponding p_itab to p_return.
   append p_return.
 ENDLOOP.

ENDFORM.                    " SET_MSG_TO_BAPIRET2

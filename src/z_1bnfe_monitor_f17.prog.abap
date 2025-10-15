*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F17
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  contingency_central
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0051   text
*----------------------------------------------------------------------*
form contingency_central  using value(p_cont_level) type any.

* Check authorization
  if gf_authorization_nfe_85 is initial.
    message id 'J1B_NFE' type 'E' number '057'.
  endif.

  case p_cont_level.
    when 'REGION'.
      call function 'VIEW_MAINTENANCE_CALL'
        exporting
          action    = 'U'
          view_name = 'J_1BNFE_CONTIN1'.
      if sy-subrc <> 0.
        message id 'J1B_NFE' type 'S' number '043'.
      endif.
    when 'BUPLA'.
      call function 'VIEW_MAINTENANCE_CALL'
        exporting
          action    = 'U'
          view_name = 'J_1BNFE_CONTIN2'.
      if sy-subrc <> 0.
        message id 'J1B_NFE' type 'S' number '043'.
      endif.
  endcase.

endform.                    " contingency_central

class ZCL_USER definition
  public
  final
  create public .

public section.

  interfaces ZIF_USER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_USER IMPLEMENTATION.


  METHOD zif_user~get_mail_user.

    DATA: address  TYPE  bapiaddr3,
          bapiret2 TYPE TABLE OF bapiret2.

    CLEAR: r_mail_user.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username      = i_user
        cache_results = abap_false
      IMPORTING
        address       = address
      TABLES
        return        = bapiret2.

    r_mail_user = address-e_mail.

  ENDMETHOD.


  METHOD ZIF_USER~GET_NAME_USER.

    CLEAR: R_NOME_USER.

    SELECT SINGLE * INTO @DATA(WL_USER)
      FROM USER_ADDRP
     WHERE BNAME = @I_USER.

    IF SY-SUBRC IS NOT INITIAL.
      R_NOME_USER = I_USER.
    ELSE.
      IF WL_USER-NAME_FIRST IS NOT INITIAL.
        IF WL_USER-NAME_LAST IS NOT INITIAL.
          CONCATENATE WL_USER-NAME_FIRST WL_USER-NAME_LAST INTO R_NOME_USER SEPARATED BY SPACE.
        ELSE.
          R_NOME_USER = WL_USER-NAME_FIRST.
        ENDIF.
      ELSE.
        R_NOME_USER = I_USER.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

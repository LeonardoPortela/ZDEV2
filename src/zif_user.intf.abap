interface ZIF_USER
  public .


  class-methods GET_NAME_USER
    importing
      !I_USER type UNAME
    returning
      value(R_NOME_USER) type STRING
    raising
      ZCX_USER .
  class-methods GET_MAIL_USER
    importing
      !I_USER type UNAME
    returning
      value(R_MAIL_USER) type STRING .
endinterface.

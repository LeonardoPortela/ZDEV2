*&---------------------------------------------------------------------*
*& Report Z_UNOCK_USER_SM12
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_unock_user_sm12.


DATA : zenque  TYPE STANDARD TABLE OF seqg3.


CALL FUNCTION 'ENQUE_READ'
  EXPORTING
    gclient = sy-mandt
   " gname   =       """"
    guname  = 'USERNFE'
  TABLES
    enq     = zenque.



    if zenque[] is not initial.
      call function 'ENQUE_DELETE'
        tables
          enq = zenque.
     clear: zenque, zenque[].
    endif.

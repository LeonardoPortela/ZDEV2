"Name: \PR:SAPLMGMM\EX:LMGMMI0D_02\EI
ENHANCEMENT 0 ZMM01_02.

if sy-tcode = 'MM01' or sy-tcode = 'MM02'.

data: wl_USNAM type USNAM.
data: T_MTART TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE.
  if sy-calld ne 'X' and sy-langu = 'P'.
    SELECT SINGLE * from mara WHERE matnr =  rmmg1-matnr.
    if sy-subrc ne 0.
      CALL FUNCTION 'G_SET_GET_ALL_VALUES'
        EXPORTING
          CLASS         = '0000'
          SETNR         = 'MAGGI_MM01_MTART'
        TABLES
          SET_VALUES    = T_MTART
        EXCEPTIONS
          SET_NOT_FOUND = 1
          OTHERS        = 2.
      IF SY-SUBRC <> 0.
         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      SORT T_MTART BY FROM.
      READ TABLE T_MTART WITH KEY FROM = rmmg1-mtart.
      IF SY-SUBRC eq 0.
         message e000(Z01) WITH 'Tipo de material bloqueado,'
                                'permitido criar somente pela ZWFMAT008.'.
      ENDIF.
    endif.
  Endif.

"======================================================Comentado USER STORY #76508 / Anderson Oenning - 19/04/2022
*select single USNAM from ZMMT0005 into wl_USNAM
*  where USNAM = sy-uname.
*
*    if sy-subrc <> 0.
*        message e000(Z01) WITH 'Usuário sem  autorização'.
*    else.
*        select single USNAM from ZMMT0005 into wl_USNAM
*          where USNAM = sy-uname and
*                WERKS = rmmg1-werks.
*
*          if sy-subrc <> 0.
*            message e000(Z01) WITH 'Usuário sem  autorização para centro informado.'.
*          endif.
*
*    endif.
endif.
ENDENHANCEMENT.

"Name: \PR:SAPLSTXBW\FO:PRINT\SE:BEGIN\EI
ENHANCEMENT 0 ZLSTXBWFGE.

****MOD 20121030 CS INI*****************************************************************************
DATA V_FULLPATH     TYPE STRING.

CLEAR V_FULLPATH.
IMPORT  V_FULLPATH from memory id 'ZV_FULLPATH'. "(Recuperas de memoria)


IF ( sy-cprog = 'ZFIY0003' OR sy-cprog = 'ZFIY0004' ) and  V_FULLPATH is not initial .
*--- Internal tables, Structures and Variables used for PDF conversion
  DATA: IT_OTF   TYPE STANDARD TABLE OF ITCOO,
        IT_DOCS  TYPE STANDARD TABLE OF DOCS,
        IT_LINES TYPE STANDARD TABLE OF TLINE,
        ST_JOB_OUTPUT_INFO      TYPE SSFCRESCL,
        ST_DOCUMENT_OUTPUT_INFO TYPE SSFCRESPD,
        ST_JOB_OUTPUT_OPTIONS   TYPE SSFCRESOP,
        ST_OUTPUT_OPTIONS       TYPE SSFCOMPOP,
        ST_CONTROL_PARAMETERS   TYPE SSFCTRLOP,
        V_LEN_IN       TYPE SO_OBJ_LEN,
        V_LANGUAGE     TYPE SFLANGU VALUE 'S',
        V_E_DEVTYPE    TYPE RSPOPTYPE,
        V_BIN_FILESIZE TYPE I,
        V_NAME         TYPE STRING,
        V_PATH         TYPE STRING VALUE 'C:/',

        V_FILTER       TYPE STRING,
        V_UACT         TYPE I,
        V_GUIOBJ       TYPE REF TO CL_GUI_FRONTEND_SERVICES,
        V_FILENAME     TYPE STRING.


    CALL FUNCTION 'CONVERT_OTF_2_PDF'
      IMPORTING
        BIN_FILESIZE           = V_BIN_FILESIZE
      TABLES
        OTF                    = otf[]
        DOCTAB_ARCHIVE         = IT_DOCS
        LINES                  = IT_LINES
      EXCEPTIONS
        ERR_CONV_NOT_POSSIBLE  = 1
        ERR_OTF_MC_NOENDMARKER = 2
        OTHERS                 = 3.
    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


 MOVE V_FULLPATH TO V_FILENAME.
 IF  V_FILENAME is not initial.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        BIN_FILESIZE            = V_BIN_FILESIZE
        FILENAME                = V_FILENAME
        FILETYPE                = 'BIN'
      TABLES
        DATA_TAB                = IT_LINES
      EXCEPTIONS
        FILE_WRITE_ERROR        = 1
        NO_BATCH                = 2
        GUI_REFUSE_FILETRANSFER = 3
        INVALID_TYPE            = 4
        NO_AUTHORITY            = 5
        UNKNOWN_ERROR           = 6
        HEADER_NOT_ALLOWED      = 7
        SEPARATOR_NOT_ALLOWED   = 8
        FILESIZE_NOT_ALLOWED    = 9
        HEADER_TOO_LONG         = 10
        DP_ERROR_CREATE         = 11
        DP_ERROR_SEND           = 12
        DP_ERROR_WRITE          = 13
        UNKNOWN_DP_ERROR        = 14
        ACCESS_DENIED           = 15
        DP_OUT_OF_MEMORY        = 16
        DISK_FULL               = 17
        DP_TIMEOUT              = 18
        FILE_NOT_FOUND          = 19
        DATAPROVIDER_EXCEPTION  = 20
        CONTROL_FLUSH_ERROR     = 21
        OTHERS                  = 22.
    IF SY-SUBRC = 0.
      MESSAGE I000(ZLSTXBWFGE) WITH 'Se creo el archivo PDF con exito'  DISPLAY LIKE 'S'.
    ELSE.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDIF.

****MOD 20121030 CS FIN ***************************************************************************

data: l_rc2                type sysubrc                 ,
      WK_0014              type ZLEST0014               ,
      wk_zlest0014         type ZLEST0014               ,
      WA_HEADERDATA        TYPE BAPISHIPMENTHEADER      ,
      WA_HEADERDATAACTION  TYPE BAPISHIPMENTHEADERACTION,
      IT_RETURN            TYPE TABLE OF BAPIRET2       ,
      VER                  type Char1                   .

clear: fcode , VER.

if ssfpp-tddevice eq c_device_printer.
  perform show_dialog changing l_rc2.
  check l_rc2 eq 0.
endif.

call function 'SSFCOMP_OTF_OUTPUT'
     exporting
          input_params   = ssfpp
          archive_params = archive_params
          otf_data       = otf[]
          bmnopri        = bkgr_toggle
     importing
          output_params  = ssfpp
     exceptions
*         empty_otf      = 1
*         illegal_device = 2
          others         = 3.
if sy-subrc <> 0.
*  message id sy-msgid type sy-msgty number sy-msgno
*          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
   raise error.
else.

  import WK_0014 from memory id 'ZCARTA'.
  free memory id 'ZCARTA'.
  if not WK_0014 is initial.

    MODIFY zlest0014 FROM wk_0014.
"   Fecha registos que estão permitindo alteração
    SELECT *
    INTO wk_zlest0014
    FROM zlest0014
    WHERE tknum    EQ WK_0014-tknum
    AND   conhec   EQ WK_0014-CONHEC
    AND   ctafrete NE WK_0014-CTAFRETE.

      MOVE: 'N' TO wk_zlest0014-alterar,
            'N' TO wk_zlest0014-reimp.

      MODIFY zlest0014 FROM wk_zlest0014.
    ENDSELECT.

*   Finaliza Transporte

    WA_HEADERDATA-SHIPMENT_NUM             = WK_0014-tknum.
    WA_HEADERDATA-EXTERNAL_ID_1            = WK_0014-CONHEC.
    WA_HEADERDATA-STATUS_SHPMNT_END        = 'X'.
     WA_HEADERDATAACTION-EXTERNAL_ID_1     = 'C'.
    WA_HEADERDATAACTION-STATUS_SHPMNT_END  = 'C'.

    CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
      EXPORTING
        HEADERDATA       = WA_HEADERDATA
        HEADERDATAACTION = WA_HEADERDATAACTION
      TABLES
        RETURN           = IT_RETURN.

    " Comita
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
        VER  = 'X'.
        export VER to memory id 'ZVER'.

  endif.

  case ssfpp-tddevice.
  when c_device_printer.
    message s060(ssfcomposer) with ssfpp-tdspoolid.
  when c_device_mail.
    message s024(ssfcomposer) with ssfpp-mailobj.
  when c_device_telefax.
    message s029(ssfcomposer) with ssfpp-faxobj.
  endcase.
endif.

exit.

ENDENHANCEMENT.

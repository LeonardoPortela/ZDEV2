interface ZIF_NBIOBSP_EXTRACTION
  public .


  data EXTRACTION type OLE2_OBJECT .
  data ERRORCODE type I .
  data ERRORDESCRIPTION type STRING .
  data TEXTENCODEFIR type STRING .
  data FIR type OLE2_OBJECT .
  constants ST_FIR_PURPOSE_EMPTY type I value 0 ##NO_TEXT.
  constants ST_FIR_PURPOSE_VERIFY type I value 1 ##NO_TEXT.
  constants ST_FIR_PURPOSE_DENTIFY type I value 2 ##NO_TEXT.
  constants ST_FIR_PURPOSE_ENROLL type I value 3 ##NO_TEXT.
  constants ST_FIR_ENROLL_VERIF_ONLY type I value 4 ##NO_TEXT.
  constants ST_FIR_ENROLL_IDENT_ONLY type I value 5 ##NO_TEXT.
  constants ST_FIR_PURPOSE_AUDIT type I value 6 ##NO_TEXT.
  constants ST_FIR_PURPOSE_UPDATE type I value 16 ##NO_TEXT.
  constants ST_TIMEOUT_INFINITE type I value 0 ##NO_TEXT.
  constants ST_TIMEOUT_DEFAULT type I value -1 ##NO_TEXT.
  constants ST_TIMEOUT_CONTINUOUS_CAPTURE type I value -2 ##NO_TEXT.
  constants ST_FINGER_ID_UNKNOWN type I value 0 ##NO_TEXT.
  constants ST_FINGER_ID_RIGHT_THUMB type I value 1 ##NO_TEXT.
  constants ST_FINGER_ID_RIGHT_INDEX type I value 2 ##NO_TEXT.
  constants ST_FINGER_ID_RIGHT_MIDDLE type I value 3 ##NO_TEXT.
  constants ST_FINGER_ID_RIGHT_RING type I value 4 ##NO_TEXT.
  constants ST_FINGER_ID_RIGHT_LITTLE type I value 5 ##NO_TEXT.
  constants ST_FINGER_ID_LEFT_THUMB type I value 6 ##NO_TEXT.
  constants ST_FINGER_ID_LEFT_INDEX type I value 7 ##NO_TEXT.
  constants ST_FINGER_ID_LEFT_MIDDLE type I value 8 ##NO_TEXT.
  constants ST_FINGER_ID_LEFT_RING type I value 9 ##NO_TEXT.
  constants ST_FINGER_ID_LEFT_LITTLE type I value 10 ##NO_TEXT.
  constants ST_FINGER_ID_MAX type I value 11 ##NO_TEXT.

  methods GET_ERRORCODE
    exporting
      !E_ERRORCODE type I
    returning
      value(R_EXTRACTION) type ref to ZIF_NBIOBSP_EXTRACTION .
  methods GET_ERRORDESCRIPTION
    exporting
      !E_ERRORDESCRIPTION type STRING
    returning
      value(R_EXTRACTION) type ref to ZIF_NBIOBSP_EXTRACTION .
  methods CAPTURE
    importing
      !I_NPURPOSE type I
    returning
      value(R_EXTRACTION) type ref to ZIF_NBIOBSP_EXTRACTION
    raising
      ZCX_NBIOBSP .
  methods GET_TEXTENCODEFIR
    exporting
      !E_TEXTENCODEFIR type STRING
    returning
      value(R_EXTRACTION) type ref to ZIF_NBIOBSP_EXTRACTION .
  methods GET_FIR
    exporting
      !E_FIR type OLE2_OBJECT
    returning
      value(R_EXTRACTION) type ref to ZIF_NBIOBSP_EXTRACTION .
  methods SET_FINGERWND
    importing
      !I_FINGERWND type I
    returning
      value(R_EXTRACTION) type ref to ZIF_NBIOBSP_EXTRACTION
    raising
      ZCX_NBIOBSP .
  methods SET_WINDOWSTYLE
    importing
      !I_WINDOW_STYLE type I
    returning
      value(R_EXTRACTION) type ref to ZIF_NBIOBSP_EXTRACTION
    raising
      ZCX_NBIOBSP .
  methods SET_FPFORECOLOR
    importing
      !I_FPFORECOLOR type STRING
    returning
      value(R_EXTRACTION) type ref to ZIF_NBIOBSP_EXTRACTION
    raising
      ZCX_NBIOBSP .
  methods SET_FPBACKCOLOR
    importing
      !I_FPBACKCOLOR type STRING
    returning
      value(R_EXTRACTION) type ref to ZIF_NBIOBSP_EXTRACTION
    raising
      ZCX_NBIOBSP .
endinterface.

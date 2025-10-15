interface ZIF_NBIOBSP_MATCHING
  public .


  data MATCHING type OLE2_OBJECT .
  data ERRORCODE type I .
  data ERRORDESCRIPTION type STRING .
  data MATCHINGRESULT type I .

  methods GET_ERRORCODE
    exporting
      !E_ERRORCODE type I
    returning
      value(R_MATCHING) type ref to ZIF_NBIOBSP_MATCHING .
  methods GET_ERRORDESCRIPTION
    exporting
      !E_ERRORDESCRIPTION type STRING
    returning
      value(R_MATCHING) type ref to ZIF_NBIOBSP_MATCHING .
  methods VERIFY_FIR
    importing
      !I_FIR type OLE2_OBJECT
    returning
      value(R_MATCHING) type ref to ZIF_NBIOBSP_MATCHING
    raising
      ZCX_NBIOBSP .
  methods GET_MATCHINGRESULT
    exporting
      !E_MATCHINGRESULT type I
    returning
      value(R_MATCHING) type ref to ZIF_NBIOBSP_MATCHING .
  methods VERIFY_TEXTENCODEFIR
    importing
      !I_TEXTENCODEFIR type STRING
    returning
      value(R_MATCHING) type ref to ZIF_NBIOBSP_MATCHING
    raising
      ZCX_NBIOBSP .
  methods SET_FINGERWND
    importing
      !I_FINGERWND type I
    returning
      value(R_MATCHING) type ref to ZIF_NBIOBSP_MATCHING
    raising
      ZCX_NBIOBSP .
  methods SET_WINDOWSTYLE
    importing
      !I_WINDOW_STYLE type I
    returning
      value(R_MATCHING) type ref to ZIF_NBIOBSP_MATCHING
    raising
      ZCX_NBIOBSP .
  methods VERIFYMATCH
    importing
      !I_PROCESSEDFIR type STRING
      !I_STOREDFIR type STRING
    returning
      value(R_MATCHING) type ref to ZIF_NBIOBSP_MATCHING
    raising
      ZCX_NBIOBSP .
endinterface.

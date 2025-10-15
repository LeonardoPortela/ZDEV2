interface ZIF_NBIOBSP
  public .


  data NBIOBSP type OLE2_OBJECT .
  data DEVICE type ref to ZIF_NBIOBSP_DEVICE .
  data EXTRACTION type ref to ZIF_NBIOBSP_EXTRACTION .
  data MATCHING type ref to ZIF_NBIOBSP_MATCHING .
  data FPDATA type ref to ZIF_NBIOBSP_FPDATA .
  data FPIMAGE type ref to ZIF_NBIOBSP_FPIMAGE .
  data NSEARCH type ref to ZIF_NBIOBSP_NSEARCH .
  data ERRORCODE type I .
  data ERRORDESCRIPTION type STRING .
  data INDEXSEARCH type ref to ZIF_NBIOBSP_INDEXSEARCH .
  constants ST_NBIOBSP_TRUE type I value 1 ##NO_TEXT.
  constants ST_NBIOBSP_FALSE type I value 0 ##NO_TEXT.
  constants ST_WINDOW_STYLE_POPOP type I value 0 ##NO_TEXT.
  constants ST_WINDOW_STYLE_INVISIBLE type I value 1 ##NO_TEXT.
  constants ST_WINDOW_STYLE_CONTINUOUS type I value 2 ##NO_TEXT.
  constants ST_COLOR_SAP_BLUE type STRING value 'D2E1F0' ##NO_TEXT.
  constants ST_COLOR_BLACK type STRING value 'FFFFFF' ##NO_TEXT.
  constants ST_COLOR_RED type STRING value 'FF0000' ##NO_TEXT.
  constants ST_COLOR_GREEN type STRING value '00FF00' ##NO_TEXT.
  constants ST_COLOR_BLUE type STRING value '0000FF' ##NO_TEXT.

  methods GET_ERRORCODE
    exporting
      !E_ERRORCODE type I
    returning
      value(R_NBIOBSP) type ref to ZIF_NBIOBSP .
  methods GET_ERRORDESCRIPTION
    exporting
      value(E_ERRORDESCRIPTION) type STRING
    returning
      value(R_NBIOBSP) type ref to ZIF_NBIOBSP .
  methods GET_DEVICE .
  methods GET_EXTRACTION .
  methods GET_MATCHING .
  methods GET_FPDATA .
  methods GET_FPIMAGE .
  methods GET_NSEARCH .
  methods GET_CHECKVALIDITYMODULE .
  methods GET_MAJORVERSION .
  methods GET_MINORVERSION .
  methods GET_BUILDNUMBER .
  methods GET_INDEXSEARCH .
  methods SET_DEVICE
    returning
      value(R_NBIOBSP) type ref to ZIF_NBIOBSP
    raising
      ZCX_NBIOBSP .
  methods SET_EXTRACTION
    returning
      value(R_NBIOBSP) type ref to ZIF_NBIOBSP
    raising
      ZCX_NBIOBSP .
  methods SET_MATCHING
    returning
      value(R_NBIOBSP) type ref to ZIF_NBIOBSP
    raising
      ZCX_NBIOBSP .
  methods SET_FPDATA
    returning
      value(R_NBIOBSP) type ref to ZIF_NBIOBSP
    raising
      ZCX_NBIOBSP .
  methods SET_FPIMAGE
    returning
      value(R_NBIOBSP) type ref to ZIF_NBIOBSP
    raising
      ZCX_NBIOBSP .
  methods SET_NSEARCH
    returning
      value(R_NBIOBSP) type ref to ZIF_NBIOBSP
    raising
      ZCX_NBIOBSP .
  methods SET_INDEXSEARCH
    returning
      value(R_NBIOBSP) type ref to ZIF_NBIOBSP
    raising
      ZCX_NBIOBSP .
endinterface.

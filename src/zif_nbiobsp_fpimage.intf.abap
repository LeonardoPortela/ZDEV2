interface ZIF_NBIOBSP_FPIMAGE
  public .


  data FPIMAGE type OLE2_OBJECT .
  data ERRORCODE type I .
  data ERRORDESCRIPTION type STRING .
  data RAWDATA type ZMMED_IM_BMT_POLEGAR .
  data TOTALFINGERCOUNT type I .
  constants ST_NBIOAPI_IMG_TYPE_RAW type I value 1 ##NO_TEXT.
  constants ST_NBIOAPI_IMG_TYPE_BMP type I value 2 ##NO_TEXT.
  constants ST_NBIOAPI_IMG_TYPE_JPG type I value 3 ##NO_TEXT.

  methods GET_ERRORCODE
    exporting
      !E_ERRORCODE type I
    returning
      value(R_FPIMAGE) type ref to ZIF_NBIOBSP_FPIMAGE .
  methods GET_ERRORDESCRIPTION
    exporting
      !E_ERRORDESCRIPTION type STRING
    returning
      value(R_FPIMAGE) type ref to ZIF_NBIOBSP_FPIMAGE .
  methods GET_RAWDATA
    importing
      !I_NFINGERID type I default 0
    exporting
      !E_RAWDATA type ZMMED_IM_BMT_POLEGAR
    returning
      value(R_FPIMAGE) type ref to ZIF_NBIOBSP_FPIMAGE
    raising
      ZCX_NBIOBSP .
  methods SAVE
    importing
      !I_BSZIMGFILEPATH type STRING
      !I_NIMAGETYPE type I default 3
      !I_NFINGERID type I default 0
      !I_NSAMPLENUMBER type I default 0
    exporting
      !E_IMAGE type XSTRING
      !E_IMAGE_STRING type STRING
    returning
      value(R_FPIMAGE) type ref to ZIF_NBIOBSP_FPIMAGE
    raising
      ZCX_NBIOBSP .
  methods GET_TOTALFINGERCOUNT
    exporting
      !E_TOTALFINGERCOUNT type I
    returning
      value(R_FPIMAGE) type ref to ZIF_NBIOBSP_FPIMAGE .
  methods EXPORT
    returning
      value(R_FPIMAGE) type ref to ZIF_NBIOBSP_FPIMAGE .
  methods CONVERTRAWTOJPG
    exporting
      !E_JPEG type OLE2_OBJECT
    returning
      value(R_FPIMAGE) type ref to ZIF_NBIOBSP_FPIMAGE
    raising
      ZCX_NBIOBSP .
endinterface.

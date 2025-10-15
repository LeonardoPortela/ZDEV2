interface ZIF_NBIOBSP_DEVICE
  public .


  data DEVICE type OLE2_OBJECT .
  data ERRORCODE type I .
  data ERRORDESCRIPTION type STRING .
  data DEVICEID type I .

  methods GET_ERRORCODE
    exporting
      !E_ERRORCODE type I
    returning
      value(R_DEVICE) type ref to ZIF_NBIOBSP_DEVICE .
  methods GET_ERRORDESCRIPTION
    exporting
      !E_ERRORDESCRIPTION type STRING
    returning
      value(R_DEVICE) type ref to ZIF_NBIOBSP_DEVICE .
  methods OPEN
    returning
      value(R_DEVICE) type ref to ZIF_NBIOBSP_DEVICE
    raising
      ZCX_NBIOBSP .
  methods CLOSE
    returning
      value(R_DEVICE) type ref to ZIF_NBIOBSP_DEVICE
    raising
      ZCX_NBIOBSP .
  methods SET_FPFORECOLOR
    importing
      !I_FPFORECOLOR type STRING
    returning
      value(R_DEVICE) type ref to ZIF_NBIOBSP_DEVICE
    raising
      ZCX_NBIOBSP .
  methods SET_FPBACKCOLOR
    importing
      !I_FPBACKCOLOR type STRING
    returning
      value(R_DEVICE) type ref to ZIF_NBIOBSP_DEVICE
    raising
      ZCX_NBIOBSP .
endinterface.

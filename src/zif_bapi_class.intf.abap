interface ZIF_BAPI_CLASS
  public .


  methods SET_PARAMETERS
    importing
      !I_PARAMETRO type DATA .
  methods RUN .
  methods GET_RESULT
    exporting
      value(R_TABLE) type ANY TABLE .
endinterface.

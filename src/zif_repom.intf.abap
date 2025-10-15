interface ZIF_REPOM
  public .


  data BRANCH type J_1BBRANC_ .
  data BUKRS type BUKRS .

  methods SET_BUKRS
    importing
      !I_BUKRS type BUKRS .
  methods SET_BRANCH
    importing
      !I_BRANCH type J_1BBRANC_ .
endinterface.

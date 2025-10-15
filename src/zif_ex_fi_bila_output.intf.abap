*"* components of interface ZIF_EX_FI_BILA_OUTPUT
interface ZIF_EX_FI_BILA_OUTPUT
  public .


  methods ADDITIONAL_ACTIONS
    importing
      !I_OUTTAB type ZIDFI_BSPL_GRID
      value(FLT_VAL) type LAND1
      !I_COMMENT_TAB type IDFI_T_GRID_HEADER .
endinterface.

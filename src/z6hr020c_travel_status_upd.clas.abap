class Z6HR020C_TRAVEL_STATUS_UPD definition
  public
  final
  create public .

*"* public components of class Z6HR020C_TRAVEL_STATUS_UPD
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces PTRM_TRIPUPDATE_INTERFACE .
protected section.
*"* protected components of class Z6HR020C_TRAVEL_STATUS_UPD
*"* do not include other source files here!!!
private section.
*"* private components of class Z6HR020C_TRAVEL_STATUS_UPD
*"* do not include other source files here!!!
ENDCLASS.



CLASS Z6HR020C_TRAVEL_STATUS_UPD IMPLEMENTATION.


  method PTRM_TRIPUPDATE_INTERFACE~PLAN_CANCELLED.
  endmethod.


  method PTRM_TRIPUPDATE_INTERFACE~TRIP_DELETED.
  endmethod.


  method PTRM_TRIPUPDATE_INTERFACE~TRIP_SAVED.
  endmethod.
ENDCLASS.

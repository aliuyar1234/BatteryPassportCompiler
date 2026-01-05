-- Battery Passport Compiler - Minimal DSL Example
-- This file contains the SSOT 8.4 example for testing

-- Field: Battery Capacity
-- Retrieves the capacity from a Battery fact and converts to Dec(6)
field battery.capacity_kwh: Dec(6) =
  let b = getFact("Battery", "battery:SKU-123");
  let cap = requireSome(
    recordGet(requireSome(b, "E001", "Battery fact not found"), "capacity_kwh"),
    "E002",
    "capacity_kwh field missing"
  );
  toDec(6, cap);

-- Field: Total Weight
-- Sums weights from BOM items with kg unit
field battery.total_weight_kg: Qty("kg") =
  let items = getFactsByPrefix("BOM", "bom:");
  let weights = map(items, λitem →
    requireSome(
      recordGet(item, "weight_kg"),
      "E003",
      "weight_kg missing"
    )
  );
  sumQty("kg", weights);

-- Field: Carbon Footprint per kWh
-- Calculates carbon intensity
field battery.carbon_per_kwh: Qty("gCO2e_per_kWh") =
  let totalCarbon = @battery.total_carbon_gco2e;
  let capacity = @battery.capacity_kwh;
  if capacity > toDec(6, 0) then
    toQty("gCO2e_per_kWh", totalCarbon / capacity)
  else
    toQty("gCO2e_per_kWh", 0);

-- Field: Compliance Status
-- Checks EU Battery Regulation compliance
field battery.eu_compliant: Bool =
  let carbonPerKwh = @battery.carbon_per_kwh;
  let threshold = toQty("gCO2e_per_kWh", 175000);  -- 175 kgCO2e/kWh
  let compliant = carbonPerKwh <= threshold;
  assert(compliant, "EU001", "Carbon footprint exceeds EU threshold");
  emitCompliance("EU_2023_1542_ART7", if compliant then "PASS" else "FAIL", "Calculated value");
  compliant;

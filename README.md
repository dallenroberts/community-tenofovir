
## Calibration

- Ingest forms are located in calibration/ingest_forms
- Calibrated parameter sets are located in calibration/parameter_sets
- Simulation settings for calibrations are set in calibration/simtools.ini
- Calibration can be run using the following command from within ./calibration:
				python ../optim_script.py
- Calibration outputs are saved as subdirectories within calibration/

## Running scenarios
- Base population scale factor and config files are specified in optim_script.py
- Scenarios can be run using an existing parameter set using the following command from within ./:
				python run_scenarios.py -d . -c ./optim_script.py --resample-method provided --table scenarios.csv --samples calibration/parameter_sets/INSERT_PARAMETER_SET_FILENAME -o scenarios/INSERT_SCENARIO_NAME -s INSERT_SCENARIO_NAME
- Scenario results are stored in scenarios/

## Analysis

- Scripts to analyze scenario results are stored in R/
- These are organized using community-tenofovir.Rproj

## Reproduction

### Calibration
- **Ingest form**: calibration_ingest_form_western_Kenya
- **Parameter sets**: calibration_parameters_combined.csv

### Scenarios
- **BASE_POPULATION_SCALE_FACTOR**: 0.03
- **Calibration Analysis (Prevalence)**:
	- **Config**: config.json
	- **Campaign**: campaign_western_Kenya.json
	- **Suite**: c6538d62-011d-ec11-9ecd-9440c9bee941
- **Calibration Analysis (Incidence)**:
	- **Config**: config_incidence.json
	- **Campaign**: campaign_western_Kenya.json
	- **Suite**: 798429c0-031d-ec11-9ecd-9440c9bee941
- **PrEP Sweep**:
	- **Config**: config_PrEPsweep.json
	- **Campaign**: campaign_western_Kenya.json, campaign_western_Kenya-PrEP-XX.json
	- **Suite**: 3b1fca7a-0a20-ec11-9ecd-9440c9bee941
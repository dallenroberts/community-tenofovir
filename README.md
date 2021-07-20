## Calibration
- Ingest forms are located in calibration/ingest_forms
- Calibrated parameter sets are located in calibration/parameter_sets
- Simulation settings for calibrations are set in calibration/simtools.ini
- Calibration can be run using the following command from within ./calibration:
				python ../optim_script.py
- Calibration outputs are saved as subdirectories within calibration/

## Running scenarios
- Scenarios can be run using an existing parameter set using the following command from within ./:
				python run_scenarios.py -d . -c ./optim_script.py --resample-method provided --table scenarios.csv --samples calibration/parameter_sets/INSERT_PARAMETER_SET_FILENAME -o scenarios/INSERT_SCENARIO_NAME -s INSERT_SCENARIO_NAME
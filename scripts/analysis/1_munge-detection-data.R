#--------------------------------------------#
# Munge detection data: Step 1 of analysis
# Myfanwy Johnston
# Wed Dec 18 11:00:43 2019 ------------------------------

# LOAD
# Handle duplicate deployments of same receiver SN with get_stations()
# Scan for orphan detections
# Group detections at gated receivers
# Discard simultaneous detections within grouped stations
# Scan for shed tags/mortalities


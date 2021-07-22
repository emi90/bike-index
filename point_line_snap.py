"""
Used as reference for snapping coordinate to line:
https://medium.com/@brendan_ward/how-to-leverage-geopandas-for-faster-snapping-of-points-to-lines-6113c94e59aa
"""


import pandas as pd
import numpy as np
import geopandas as gpd


class SnapLinePoint:
    """
    Takes GeoDataFrame of Point coordinates and GDF of Linestring
    And returns a new GeoDataFrame with the Point snapped to the
    nearest distance on the Linestring
    """
    def __init__(self, point_gdf, line_gdf, offset_param):
        self.point_gdf = point_gdf
        self.line_gdf = line_gdf
        self.offset_param = offset_param
        line_gdf.sindex
    
    def get_line_for_point(self):
        """
        Create bount box for each coordinate given offset parameter
        Create GDF of Linestring within offset parameter for each coordinate
        Return GDF with each corresponding Linestring for each point within offset
        """

        # Create bound box for the offset parameters
        bbox = self.point_gdf.bounds + [-self.offset_param, 
                                        -self.offset_param, 
                                        self.offset_param, 
                                        self.offset_param]
        
        # Linestring within offset parameter for each point
        hits = bbox.apply(lambda row: list(self.line_gdf.sindex.intersection(row)), axis=1)

        # Temp DF of each Linestring within offset parameter for each point
        temp = pd.DataFrame({
            "pt_idx":np.repeat(hits.index, hits.apply(len)),
            "line_i":np.concatenate(hits.values)
        })

        # Merge back to original Linestring GDF
        line_temp = temp.merge(self.line_gdf.reset_index(drop=False).rename(
            columns={'index':'line_i'}), on='line_i'
            )

        # Rename original Point GDF
        point_temp = self.point_gdf.reset_index(drop=False).rename(
            columns={'index':'pt_idx', 'geometry':'point'}
        )

        # Merge the two temp DF
        line_point_temp = line_temp.merge(point_temp, on='pt_idx')

        # Convert back to GDF
        gdf = gpd.GeoDataFrame(line_point_temp, geometry='geometry', crs=self.point_gdf.crs)

        return gdf
    

    def snap_to_line(self, tolerance=0):
        """"
        tolerance: set tolerance distance between point and line
        For each point/line combination, project to the LineString from Point
        Discard any point/line combination that is greater than the tolerance
        Find and keep just the closest line for each point
        Snap the point to the line and replace the coordinates
        Return merged GDF with both line and point geometries
        """

        gdf = self.get_line_for_point()

        # Calculate the distance of each point/line combination
        gdf['snap_dist'] = gdf.geometry.distance(gpd.GeoSeries(gdf.point))

        # Set tolerance if different from offset parameter
        if tolerance == 0:
            tolerance = self.offset_param

        # Discard any point/line combinations where distance exceeds tolerance
        filter_gdf = gdf.loc[gdf.snap_dist <= tolerance]

        # Sort ascending
        filter_gdf = filter_gdf.sort_values(by=['snap_dist'])

        # Find the closest street segment for each crash point
        closest = filter_gdf.groupby("pt_idx").first()

        # Construct a GDF of the closest lines
        closest_gdf = gpd.GeoDataFrame(closest, geometry="geometry")

        # Position of the nearest point from the start of the line
        pos = closest_gdf.geometry.project(gpd.GeoSeries(closest_gdf.point))

        # Get new point location geometry
        new_pts = closest_gdf.geometry.interpolate(pos)

        # Create new GDF from the columns from the closest line and new point geometries
        merged_gdf = gpd.GeoDataFrame(closest_gdf.rename(columns={'geometry':'line_geo'}), geometry=new_pts)

        return merged_gdf
    
    
